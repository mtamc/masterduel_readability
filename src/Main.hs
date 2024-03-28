{-# LANGUAGE DeriveAnyClass, DeriveGeneric, IncoherentInstances, OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use zipWith" #-}
module Main (main) where

import Control.Lens             (over, set, view, (%~), (.~), (^.))
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Binary.Builder      as Binary
import Data.Binary.Get          hiding (lookAhead)
import Data.ByteString          qualified as BS
import Data.ByteString.Lazy     qualified as BL
import Data.List                (partition)
import Data.List.Extra          (dropEnd)
import Data.Text                qualified as Text
import Prelude                  hiding (many)
import Relude.Unsafe            qualified as Unsafe
import Text.Megaparsec
import Text.Megaparsec.Char
import Util

main ∷ IO ()
main = do
  cards ← getCards
  writeFileLBS "./data/decoded_cards.json" (encodePretty cards)
  -- generateDescAndPartFiles cards
  let cardsWithUpdatedDescs = map updateDesc cards
  writeFileLBS "./data/decoded_cards.updated.json" (encodePretty cardsWithUpdatedDescs)
  generateDescAndPartFiles cardsWithUpdatedDescs
  echo "done"


-- CONFIG

data Paths
  = Paths
    { name :: FilePath
    , desc :: FilePath
    , part :: FilePath
    , pidx :: FilePath
    }
  deriving (Eq, FromJSON, Generic, Show, ToJSON)

paths ∷ Paths
paths
  = Paths
    { name = "./data/CARD_Name.bytes.dec.json"
    , desc = "./data/CARD_Desc.bytes.dec.json"
    , part = "./data/Card_Part.bytes.dec"
    , pidx = "./data/Card_Pidx.bytes.dec"
    }


-- TYPES

data Card effectType
  = Card
    { name            :: Text
      -- Often the first registered effect is not at position 0
    , leadingText     :: Text
    , effects         :: [effectType]
    , pendulumEffects :: [effectType]
    }
  deriving (Eq, FromJSON, Generic, Show, ToJSON)

instance Functor Card where
  fmap ∷ (a → b) → Card a → Card b
  fmap f card
    = card
    { effects = map f card.effects
    , pendulumEffects = map f card.pendulumEffects
    }

cardOriginalText ∷ Card Effect → Text
cardOriginalText c =
  Text.concat
    ( c.leadingText
    : map effectText c.effects
    ⊕ map effectText c.pendulumEffects
    )

data Effect
  = Effect
    { mainEffect       :: Text
      -- Sometimes there's text not part of any effect, most commonly " "
      -- in between effects, but can also be "unregistered" effect text
    , trailingText     :: Text
      -- Some of the effects are out of order in the Part file... lower is first.
    , originalPosition :: Int
    }
  deriving (Eq, FromJSON, Generic, Show, ToJSON)

effectText ∷ Effect → Text
effectText e = e.mainEffect ⊕ e.trailingText

data PidxData
  = PidxData
    { -- Position of the card's first effect in the Part asset (which is a sequence of effect positions)
      firstEffectIndex    :: Int
    , effectCount         :: Int
    , pendulumEffectCount :: Int
    }
  deriving (Eq, Generic, Show)

-- | Position of a single effect within a card's description. 0 is the first
-- character of the card's description.
data PartData
  = PartData
    { start :: Int
    , end   :: Int
    }
  deriving (Eq, Generic, Show)


-- UPDATING DESCS

-- | Data structure used internally to better represent effect elements
data ProcessedEffect
  = ProcessedEffect
    { leadingNewline   :: Bool
    , enclosedNumber   :: Maybe Text
    , tags             :: [Text]
    , condition        :: Maybe Text
    , activation       :: Maybe Text
    , mainEffect       :: Text
    , trailingText     :: Text
    , originalPosition :: Int
      {-| Effect with no offset registered in Card_Part, we parsed it manually -}
    , unregistered     :: Bool
    }
  deriving (Eq, FromJSON, Generic, Show, ToJSON)

mapPEffectText ∷ (Text → Text) → ProcessedEffect → ProcessedEffect
mapPEffectText f
  = #condition %~ fmap f
  ⋙ #activation %~ fmap f
  ⋙ #mainEffect %~ f

type Parser = Parsec Void Text

updateDesc ∷ Card Effect → Card Effect
updateDesc card = let
  processedEffects = map effectToProcessed card.effects
  normalizedLeading = Text.strip card.leadingText
  (leadingWithoutUnregisteredEffects, effectsWithUnregistered) =
    getUnregisteredEffects normalizedLeading processedEffects
  withProcessedEffectsAndNormalizedLeading
    = Card
      { name = card.name
      , leadingText = leadingWithoutUnregisteredEffects
      , effects
        = numberEffects leadingWithoutUnregisteredEffects effectsWithUnregistered
      , pendulumEffects =
        numberEffects "dummy text so first pendulum effect has a newline"
          $ map effectToProcessed card.pendulumEffects
      }
  final
    = uppercaseKeywords
    $ splitActivationsAndConditions
    $ mapAllText (rewordSearch . rewordBounce . rewordPiercing . rewordMill)
    $ tagPhases
    $ tagOncePerTurns
    $ graveyardToGy
    $ tagQuickEffects withProcessedEffectsAndNormalizedLeading
  (finalLeadingText, finalEffects) = fromUnregisteredEffects final.leadingText final.effects
  in Card
    { name = final.name
    , leadingText = finalLeadingText
    , effects = map processedToEffect finalEffects
    , pendulumEffects = map processedToEffect final.pendulumEffects
    }

fromUnregisteredEffects ∷ Text → [ProcessedEffect] → (Text, [ProcessedEffect])
fromUnregisteredEffects leadingText effects = let
  (unregisteredEffects, effectsWithoutUnregistered) = partition (.unregistered) effects
  leadingTextContainingUnregisteredEffects =
    leadingText ⊕ Text.concat (map (effectText . processedToEffect) unregisteredEffects)
  in (leadingTextContainingUnregisteredEffects, effectsWithoutUnregistered)

-- | atm we only parse one specific case: zero registered effects and the entire
-- leading text is one sentence. if there are multiple sentences then it's very
-- hard to know if this is one or multiple effects and how to separate them.
getUnregisteredEffects ∷ Text → [ProcessedEffect] → (Text, [ProcessedEffect])
getUnregisteredEffects leadingText effects
  | not (null effects) ∨ Text.length (Text.filter (≡ '.') leadingText) > 2 =
    (leadingText, effects)
  | Text.length (Text.filter (≡ '.') leadingText) ≡ 2
    ∧ not
      (isAllowableSecondSentence
        (Text.split (≡ '.') leadingText & drop 1 & map (<> ". ") & Text.concat & Text.strip)
      )
      = (leadingText, effects)
  | otherwise = let
    unregisteredEffectParser ∷ Parser (Text, [ProcessedEffect])
    unregisteredEffectParser = do
      cond ←
        (try do
          let end = noneOf ['.', ':'] *> void newline
          condStart ← toText <$> manyTill anySingle (try (lookAhead end) <|> eof)
          condLastChar ← one <$> anySingle
          void newline
          pure (condStart ⊕ condLastChar)
        ) <|> pure ""
      effectTxt ← (Just . toText <$> someTill anySingle eof) <|> pure Nothing
      let effect = effectTxt <&> basicProcessedEffectFromText 0
      pure (cond, maybeToList effect)
    (summoningCondition, unregisteredEffects) =
      fromRight (error "") $ parse unregisteredEffectParser "" leadingText
    in (summoningCondition, unregisteredEffects ⧺ effects)

isAllowableSecondSentence ∷ Text → Bool
isAllowableSecondSentence txt
  = txt
  ∈ [ "It cannot attack this turn."
    ]
  ∨ "Otherwise, " `Text.isPrefixOf` txt
  ∨ isJust (detectStandardHopt "this" txt)

{-| this uses default value and is not the fully processed state -}
effectToProcessed ∷ Effect → ProcessedEffect
effectToProcessed effect =
  ProcessedEffect
    { leadingNewline = False
    , enclosedNumber = Nothing
    , tags = []
    , condition = Nothing
    , activation = Nothing
    , mainEffect = effect.mainEffect
    , trailingText = effect.trailingText
    , originalPosition = effect.originalPosition
    , unregistered = False
    }

basicProcessedEffectFromText ∷ Int → Text → ProcessedEffect
basicProcessedEffectFromText pos text =
  ProcessedEffect
    { leadingNewline = False
    , enclosedNumber = Nothing
    , tags = []
    , condition = Nothing
    , activation = Nothing
    , mainEffect = text
    , trailingText = ""
    , originalPosition = pos
    , unregistered = False
    }

processedToEffect ∷ ProcessedEffect → Effect
processedToEffect processed =
  Effect
    { mainEffect =
      (if processed.leadingNewline then "\n" else "")
      ⊕ maybe "" (⊕ " ") processed.enclosedNumber
      ⊕ Text.concat (map ((<> " | ") . Text.toUpper) $ ordNub processed.tags)
      ⊕ maybe "" (decorate "" ": ") processed.condition
      ⊕ maybe "" (decorate "<b>" ";</b> ") processed.activation
      ⊕ processed.mainEffect
    , trailingText = processed.trailingText
    , originalPosition = processed.originalPosition
    }

numberEffects ∷ Text → [ProcessedEffect] → [ProcessedEffect]
numberEffects cardLeadingText effects = let
  effectsThatNeedNumbering = filter (mustBeNumbered . (.mainEffect)) effects
  in fst $ foldl'
    (\(newEffects, enclosedNums) cur →
      let
        neEnclosedNums = nonEmpty enclosedNums
        enclosedCurrent = maybe '①' head neEnclosedNums
        enclosedRest = maybe [] tail neEnclosedNums
        textSoFar = Text.concat (cardLeadingText : map (effectText . processedToEffect) newEffects)
        mustUseEnclosed = length effectsThatNeedNumbering > 1 ∧ mustBeNumbered cur.mainEffect
        mustAddNewline = Text.strip cur.mainEffect ≢ "" ∧ Text.strip textSoFar ≢ ""
      in
      ( newEffects
        ⊕ [ cur
            & #leadingNewline .~ mustAddNewline
            & #enclosedNumber .~ (if mustUseEnclosed then Just (one enclosedCurrent) else Nothing)
            & #trailingText %~ Text.stripEnd
          ]
      , if mustUseEnclosed then enclosedRest else enclosedNums
      )
    )
    ([], "①②③④⑤⑥⑦⑧⑨⑩⑪⑫⑬⑬⑮⑯⑰⑱⑲⑳")
    effects

mustBeNumbered ∷ Text → Bool
mustBeNumbered effectTxt
  = Text.strip effectTxt ≢ ""
  ∧ not ("●" `Text.isPrefixOf` Text.strip effectTxt)

mapAllText ∷ (Text → Text) → Card ProcessedEffect → Card ProcessedEffect
mapAllText f
  = #leadingText %~ f
  ⋙ #effects
    %~ map
      ( #condition    %~ fmap f
      ⋙ #activation   %~ fmap f
      ⋙ #mainEffect   %~ f
      ⋙ #trailingText %~ f
      )
  ⋙ #pendulumEffects
    %~ map
      ( #condition    %~ fmap f
      ⋙ #activation   %~ fmap f
      ⋙ #mainEffect   %~ f
      ⋙ #trailingText %~ f
      )

uppercaseKeywords ∷ Card ProcessedEffect → Card ProcessedEffect
uppercaseKeywords = mapAllText \txt
  → Text.replace "Set" "SET"
  . Text.replace "Seting" "SETTING"
  $ foldr subStrToUpperCase txt substringsToCapitalize
  where
  substringsToCapitalize ∷ [Text]
  substringsToCapitalize =
    [ "summoning", "summoned", "summon", "summons", "special summon" , "normal summon" , "tribute summon" , "ritual summon" , "pendulum summon" , "flip summon", "synchro summon" , "link summon" , "xyz summon" , "fusion summon", "normal or special summon"
    , "destroy", "destroying", "destroyed", "destroys"
    , "negate", "negating", "negated", "negates"
    , "discard", "discarding", "discarded", "discards"
    , "quick effect"
    , "hand"
    , "deck"
    , "banish", "banished", "banishing", "banishes"
    , "excavate", "excavating", "excavated", "excavates"
    , "draw", "draws", "drew"
    , "piercing"
    ]
  subStrToUpperCase ∷ Text → Text → Text
  subStrToUpperCase "" str = str
  subStrToUpperCase _ "" = ""
  subStrToUpperCase needle haystack
    | Text.toLower needle `Text.isPrefixOf` Text.toLower haystack =
      Text.toUpper needle ⊕ subStrToUpperCase needle (Text.drop (Text.length needle) haystack)
    | otherwise = Text.take 1 haystack ⊕ subStrToUpperCase needle (Text.drop 1 haystack)

splitActivationsAndConditions ∷ Card ProcessedEffect → Card ProcessedEffect
splitActivationsAndConditions =
  (fmap \effect → let
    (condition, activation, mainEffect) = splitActivationsAndConditions' effect.mainEffect
    in effect
      & #condition  .~ condition
      & #activation .~ activation
      & #mainEffect .~ mainEffect
  ) ⋙ #leadingText
    %~ ( Text.splitOn "."
       ⋙ map
         ( removeSurroundingSpaces
         ⋙ splitActivationsAndConditions'
         ⋙ (\(cond, acti, mainEff)
            → maybe "" (decorate "<i>" ":</i> ") cond
            ⊕ maybe "" (decorate "<b>" ";</b> ") acti
            ⊕ mainEff
           )
         )
       ⋙ Text.intercalate ". "
       ⋙ Text.replace ". )" ".)"
       ⋙ Text.strip

       )

splitActivationsAndConditions' ∷ Text → (Maybe Text, Maybe Text, Text)
splitActivationsAndConditions' sentence = let
    (condition, everythingAfterCondition) =
      case Text.split (≡ ':') sentence of
        []        → error "should never happen"
        [noColon] → (Nothing, noColon)
        cond:rest → (textNonEmpty cond, removeSurroundingSpaces $ Text.concat rest)
    (activation, mainEffect) =
      case Text.split (≡ ';') everythingAfterCondition of
        []            → error "should never happen"
        [noSemicolon] → (Nothing, noSemicolon)
        acti:rest     → (textNonEmpty acti, removeSurroundingSpaces $ Text.concat rest)
    in (condition, activation, mainEffect)

removeSurroundingSpaces ∷ Text → Text
removeSurroundingSpaces = Text.dropWhileEnd (≡ ' ') . Text.dropWhile (≡ ' ')

tagPhases ∷ Card ProcessedEffect → Card ProcessedEffect
tagPhases = fmap \effect →
  case parsePhase effect.mainEffect of
    Nothing                   → effect
    Just (phase, newMainText) → effect & #tags %~ (phase:) & #mainEffect .~ newMainText
  where
  parsePhase ∷ Text → Maybe (Text, Text)
  parsePhase = fromRight Nothing . parse
    (do
      _during ← toText <$> string @String "During "
      whose ← choice [string "your opponent's", string "the", string "your"]
      let
        whoseAbbreviated =
          case whose of
            "the"             → ""
            "your opponent's" → "opp's "
            "your"            → "your "
            _                 → error "unexpected whose"
      void $ char ' '
      phaseType ← toText <$> someTill anySingle (try (char ' '))
      _phaseWord ← someTill anySingle (try (char ':' <|> char ','))
      void $ char ' '
      rest ← toText <$> manyTill anySingle eof
      pure $ Just (whoseAbbreviated ⊕ phaseType ⊕ " phase", mapTextHead Text.toUpper rest)
    )
    ""

rewordBounce ∷ Text → Text
rewordBounce =
  Text.replace "returned to the hand" "BOUNCED"
    . fromRight (error "") . parse
      (do
        let
          bounceP ∷ Parser Text
          bounceP = do
            _return ← toText <$> string' "return "
            what ← toText
              <$> someTill anySingle
                (try (string' " from the field to the hand" <|> string' " to the hand"))
            pure $ "BOUNCE " ⊕ what
        everythingBeforeBounce ←
          toText <$> manyTill anySingle (try (void $ lookAhead bounceP) <|> eof)
        bounce ← optional bounceP
        everythingAfter ← toText <$> many anySingle
        pure $ case bounce of
          Nothing    → everythingBeforeBounce ⊕ everythingAfter
          Just found → rewordBounce (everythingBeforeBounce ⊕ found ⊕ everythingAfter)
      )
      ""

-- Piercing wordings:
-- If [this card that was SPECIAL SUMMONED from the GY] attacks a Defense Position monster, inflict piercing battle damage.
-- If [this card] attacks a Defense Position monster, inflict piercing battle damage {optional: to your opponent}.
-- While you control another Insect monster, if [an Insect monster you control] attacks a Defense Position monster, inflict piercing battle damage to your opponent.
-- During battle, when it attacks a Defense Position monster whose DEF is lower than the ATK of the equipped monster, inflict the difference as Battle Damage to your opponent.
rewordPiercing ∷ Text → Text
rewordPiercing = fromRight (error "") . parse
  (do
    let
      piercingP ∷ Parser Text
      piercingP = do
        ifWord ← toText <$> string' "if "
        what ← toText
          <$> someTill (anySingleBut '.')
            (try
              (string' "attacks a defense position monster, inflict piercing battle damage"
              <|> string' "attacks a Defense Position monster whose DEF is lower than the ATK of the equipped monster, inflict the difference as Battle Damage"
              )
            )
        _toYourOpp ← optional $ string' " to your opponent"
        let newStart = what & applyWhen (ifWord ≡ "If ") (mapTextHead Text.toUpper)
        pure $ newStart ⊕ "is PIERCING"
    everythingBeforePiercing ←
      toText <$> manyTill anySingle (try (void $ lookAhead piercingP) <|> eof)
    piercing ← optional piercingP
    everythingAfter ← toText <$> many anySingle
    pure $ case piercing of
      Nothing    → everythingBeforePiercing ⊕ everythingAfter
      Just found → rewordPiercing (everythingBeforePiercing ⊕ found ⊕ everythingAfter)
  )
  ""

rewordMill ∷ Text → Text
rewordMill = fromRight (error "") . parse
  (do
    let
      millP ∷ Parser Text
      millP = do
        _return ← toText <$> string' "send "
        what ← toText
          <$> someTill (anySingleBut '.')
            (try
              (   string' " from the top of your deck to the gy"
              <|> string' " from the top of your deck to the graveyard")
              )
        pure $ "MILL " ⊕ what
    everythingBeforeMill ← toText <$> manyTill anySingle (try (void $ lookAhead millP) <|> eof)
    mill ← optional millP
    everythingAfter ← toText <$> many anySingle
    pure $ case mill of
      Nothing    → everythingBeforeMill ⊕ everythingAfter
      Just found → rewordBounce (everythingBeforeMill ⊕ found ⊕ everythingAfter)
  )
  ""

rewordSearch ∷ Text → Text
rewordSearch = fromRight (error "") . parse
  (do
    let
      searchP ∷ Parser Text
      searchP = do
        _add ← toText <$> string' "add "
        quantity ← numberChar
        void hspace1
        what ← toText <$> someTill (anySingleBut '.') (try (lookAhead (string' " from your deck")))
        _fromYour ← string' " from your "
        deckOrGy ← Text.toLower . toText <$> (string' "deck or gy" <|> string' "deck")
        _toYourHand ← string' " to your hand"
        pure $
          "SEARCH " ⊕ one quantity ⊕ " " ⊕ what ⊕ (if deckOrGy ≡ "deck or gy" then " (from DECK or GY)" else "")
    everythingBeforeSearch ← toText <$> manyTill anySingle (try (void $ lookAhead searchP) <|> eof)
    search ← optional searchP
    everythingAfter ← toText <$> many anySingle
    pure $ case search of
      Nothing    → everythingBeforeSearch ⊕ everythingAfter
      Just found → rewordSearch (everythingBeforeSearch ⊕ found ⊕ everythingAfter)
  )
  ""

graveyardToGy ∷ Card ProcessedEffect → Card ProcessedEffect
graveyardToGy = mapAllText (Text.replace "Graveyard" "GY")

tagOncePerTurns ∷ Card ProcessedEffect → Card ProcessedEffect
tagOncePerTurns
  = tagFollowingHoptInBoth
  . tagHoptActis
  . tagTrailingHoptInBoth
  . fmap tag
  where
  tag ∷ ProcessedEffect → ProcessedEffect
  tag = tagStandardHopt . tagGainHopt . tagOptComma . tagOptColon
  -- e.g. Senet Switch
  tagOptColon ∷ ProcessedEffect → ProcessedEffect
  tagOptColon effect
    | "Once per turn: " `Text.isInfixOf` effect.mainEffect =
      effect & #tags %~ ("opt":) & #mainEffect %~ Text.replace "Once per turn: " ""
    | otherwise = effect
  -- e.g. Infernal Dragon
  tagOptComma ∷ ProcessedEffect → ProcessedEffect
  tagOptComma effect
    | "Once per turn, " `Text.isInfixOf` effect.mainEffect =
      effect
        & #tags %~ ("opt":)
        & #mainEffect %~ (mapTextHead Text.toUpper . Text.replace "Once per turn, " "")
    | otherwise = effect
  -- e.g. Angel 01
  tagGainHopt ∷ ProcessedEffect → ProcessedEffect
  tagGainHopt effect
    | "You can only gain this effect once per turn" `Text.isInfixOf` effect.mainEffect =
      effect & #tags %~ ("hopt":)
    | otherwise = effect
  -- e.g. Motor Shell (trailingText)
  tagStandardHopt ∷ ProcessedEffect → ProcessedEffect
  tagStandardHopt effect =
    case detectStandardHopt "this" effect.mainEffect of
      Just changed → effect & #mainEffect .~ changed & #tags %~ ("hopt":)
      Nothing →
        case detectStandardHopt "this" effect.trailingText of
          Just changed → effect & #trailingText .~ changed & #tags %~ ("hopt":)
          Nothing      → effect
  tagFollowingHopt
    ∷ (([ProcessedEffect] → [ProcessedEffect]) → Card ProcessedEffect → Card ProcessedEffect)
    → Card ProcessedEffect
    → Card ProcessedEffect
  tagFollowingHopt overEffects card =
    card & overEffects (fst . foldl' f ([], False))
    where
    f (result, True) effect = (result ⧺ [effect & #tags %~ ("hopt":)], True)
    f (result, False) effect =
      case detectFollowingHopt effect.trailingText of
        Just changed → (result ⧺ [effect & #trailingText .~ changed], True)
        Nothing      → (result ⧺ [effect], False)
  tagFollowingHoptInBoth ∷ Card ProcessedEffect → Card ProcessedEffect
  tagFollowingHoptInBoth =
    tagFollowingHopt (over #effects) . tagFollowingHopt (over #pendulumEffects)
  tagTrailingHopt
    ∷ (Card ProcessedEffect → [ProcessedEffect])
    → (([ProcessedEffect] → [ProcessedEffect]) → Card ProcessedEffect → Card ProcessedEffect)
    → Card ProcessedEffect
    → Card ProcessedEffect
  tagTrailingHopt getEffects overEffects card =
    case viaNonEmpty last (getEffects card) of
      Nothing → card
      Just effect →
        case detectStandardHopt "each" effect.mainEffect of
          Just changed →
            card
              & #effects
                  %~ ( (\effs → dropEnd 1 effs ⧺ [effect & #mainEffect .~ changed])
                     ⋙ map (#tags %~ ("hopt":))
                     )
          Nothing →
            case detectStandardHopt "each" effect.trailingText of
              Just changed →
                card
                  & overEffects
                    ( (\effs → dropEnd 1 effs ⧺ [effect & #trailingText .~ changed])
                    ⋙ map (#tags %~ ("hopt":))
                    )
              Nothing →
                case detectStandardHopt "each" card.leadingText of
                  Just changed →
                    card
                      & #leadingText .~ changed
                      & overEffects (map (#tags %~ ("hopt":)))
                  Nothing → card
  tagTrailingHoptInBoth ∷ Card ProcessedEffect → Card ProcessedEffect
  tagTrailingHoptInBoth
    = tagTrailingHopt (^. #effects) (over #effects)
    . tagTrailingHopt (^. #pendulumEffects) (over #pendulumEffects)
  detectActiHopt ∷ Text → (Bool, Text)
  detectActiHopt = fromRight (error "") . parse
    (do
      let
        hoptP ∷ Parser Text
        hoptP = do
          start ← toText <$> string "You can only activate 1 \""
          name ← toText <$> someTill anySingle (try (lookAhead (char '"')))
          end ← string "\" per turn."
          spc ← maybe "" one <$> optional (char ' ')
          pure (start ⊕ name ⊕ end ⊕ spc)
      everythingBeforeHopt ← toText <$> manyTill anySingle (try (void $ lookAhead hoptP) <|> eof)
      hopt ← optional hoptP
      everythingAfter ← textNonEmpty . toText <$> many anySingle
      pure (isJust hopt, Text.strip everythingBeforeHopt ⊕ maybe "" (" " <>) everythingAfter)
    )
    ""
  tagHoptActis ∷ Card ProcessedEffect → Card ProcessedEffect
  tagHoptActis card
    | any (\e → fst $ detectActiHopt e.trailingText) card.effects =
      card
        & #leadingText %~ (("HOPT ACTIVATION\n" <>) ⋙ Text.strip)
        & #effects %~ mapHead (#leadingNewline .~ True)
        & #effects
          %~ map
            (\e
              → detectActiHopt e.trailingText
              & \(detected, changed) → if detected then e & #trailingText .~ changed else e
            )
    | otherwise = card

-- | Reports if Hopt detected and returns the string with the Hopt removed
detectStandardHopt ∷ Text → Text → Maybe Text
detectStandardHopt thisOrEach = fromRight (error "") . parse
  (do
    let
      hoptP ∷ Parser Text
      hoptP = do
        start ← toText <$> string ("You can only use " ⊕ thisOrEach ⊕ " effect of \"")
        name ← toText <$> someTill anySingle (try (char '"'))
        end ← toText . ("\"" <>) <$> string " once per turn."
        spc ← maybe "" one <$> optional (char ' ')
        pure (start ⊕ name ⊕ end ⊕ spc)
    everythingBeforeHopt ← toText <$> manyTill anySingle (try (void (lookAhead hoptP)) <|> eof)
    hopt ← optional hoptP
    everythingAfter ← textNonEmpty . toText <$> many anySingle
    pure $
      if isJust hopt then
        Just $ Text.strip everythingBeforeHopt ⊕ maybe "" (" " <>) everythingAfter
      else
        Nothing
  )
  ""

detectFollowingHopt ∷ Text → Maybe Text
detectFollowingHopt = fromRight (error "") . parse
  (do
    let
      hoptP ∷ Parser Text
      hoptP = do
        start ← toText <$> string "You can only use each of the following effects of \""
        name ← toText <$> someTill anySingle (try (char '"'))
        end ← toText . ("\"" <>) <$> string " once per turn."
        spc ← maybe "" one <$> optional (char ' ')
        pure (start ⊕ name ⊕ end ⊕ spc)
    everythingBeforeHopt ← toText <$> manyTill anySingle (try (void (lookAhead hoptP)) <|> eof)
    hopt ← optional hoptP
    everythingAfter ← textNonEmpty . toText <$> many anySingle
    pure $
      if isJust hopt then
        Just $ Text.strip everythingBeforeHopt ⊕ maybe "" (" " <>) everythingAfter
      else
        Nothing
  )
  ""

tagQuickEffects ∷ Card ProcessedEffect → Card ProcessedEffect
tagQuickEffects = fmap tag where
  tag ∷ ProcessedEffect → ProcessedEffect
  tag effect = let
    isQuick = any (maybe False hasQuickEffectText)
      [Just effect.mainEffect, effect.activation, effect.condition]
    in effect
      & applyWhen isQuick (mapPEffectText removeQuickEffectText . (#tags %~ ("quick":)))

quickEffectP ∷ Parser Text
quickEffectP = do
  leadingSpace ← maybe "" one <$> optional (char ' ')
  parensStart ← choice $ map string' ["(quick effect", "(this is a quick effect"]
  dot ← maybe "" one <$> optional (char '.')
  closingParens ← one <$> char ')'
  pure $ leadingSpace ⊕ parensStart ⊕ dot ⊕ closingParens

removeQuickEffectP ∷ Parser Text
removeQuickEffectP = do
  prefix ← toText <$> manyTill anySingle (try (void (lookAhead quickEffectP)) <|> eof)
  try (void quickEffectP <|> eof)
  suffix ← toText <$> many anySingle
  pure $ prefix ⊕ suffix

removeQuickEffectText ∷ Text → Text
removeQuickEffectText = toText . fromRight (error "") . parse removeQuickEffectP ""

hasQuickEffectText ∷ Text → Bool
hasQuickEffectText text = removeQuickEffectText text ≢ text


-- ENCODING

generateDescAndPartFiles ∷ [Card Effect] → IO ()
generateDescAndPartFiles cards = do
  let
    cardTexts = map cardOriginalText cards
    newPartSrc
      = toLazyByteString
      . mconcat
      $ (putWord16le 0 : putWord16le 0 : map partDataToBinary (toPartDatas cards))
  writeFileLBS (paths.part ⊕ ".new") newPartSrc
  writeFileLBS (paths.desc ⊕ ".new") $ encodePretty cardTexts
  where
  partDataToBinary ∷ PartData → Builder
  partDataToBinary (PartData start end) =
    putWord16le (fromIntegral start) ⊕ putWord16le (fromIntegral end)

  toPartDatas ∷ [Card Effect] → [PartData]
  toPartDatas = concatMap toCardPartDatas

  toCardPartDatas ∷ Card Effect → [PartData]
  toCardPartDatas card = let
    originalText = cardOriginalText card
    partDatas
      = ( map (toEffectPartData originalText)
        . filter (\e → e.originalPosition ≢ 0)
        $ sortOn (^. #originalPosition) card.effects
        )
        ⊕ ( map (toEffectPartData (disregardMonsterEffects originalText))
          . filter (\e → e.originalPosition ≢ 0)
          $ sortOn (^. #originalPosition) card.pendulumEffects
          )
    in partDatas

  toEffectPartData ∷ Text → Effect → PartData
  toEffectPartData originalText e = let
    start = Unsafe.fromJust $ utf8SubIndex e.mainEffect originalText
    end = start + BS.length (encodeUtf8 e.mainEffect)
    in PartData start end

  disregardMonsterEffects ∷ Text → Text
  disregardMonsterEffects txtAsText = let
    txtAsBs = encodeUtf8 txtAsText
    pendulumMarkerIndex = fromMaybe 0 $ utf8SubIndex "[Pendulum Effect]" txtAsText
    in decodeUtf8 (BS.replicate pendulumMarkerIndex 0 ⊕ BS.drop pendulumMarkerIndex txtAsBs)

  utf8SubIndex ∷ Text → Text → Maybe Int
  utf8SubIndex substr = go "" "" where
    go incorrect correct (Text.uncons → Nothing)
      | substr ≡ "" = Just 0
      | correct ≡ substr = Just . BS.length $ encodeUtf8 incorrect
      | otherwise =
          log
            ( " |||| SUBSTR = " ⊕ substr
            ⊕ " |||| INCORRECT = " ⊕ incorrect
            ⊕ " |||| CORRECT = " ⊕ correct
            )
            Nothing
    go incorrect correct (Text.uncons → Just (cur, rest))
      | correctPlusCur ≡ substr = Just . BS.length $ encodeUtf8 incorrect
      -- one newline = correct
      | correctPlusCur `Text.isPrefixOf` substr = go incorrect correctPlusCur rest
      -- two newlines = incorrect, but we add both to the incorrect instead of
      -- keeping the second one as the first one :(
      | otherwise = go incorrectPlusCorrectPlusCur "" rest
      where
      correctPlusCur = correct `Text.snoc` cur
      incorrectPlusCorrectPlusCur = incorrect ⊕ correct `Text.snoc` cur


-- DECODING

getCards ∷ IO [Card Effect]
getCards =
  mkCards
    <$> decodeFile paths.name
    <*> decodeFile paths.desc
    <*> readFileLBS paths.part
    <*> readFileLBS paths.pidx

mkCards ∷ [Text] → [Text] → BL.ByteString → BL.ByteString → [Card Effect]
mkCards names (map encodeUtf8 → descs) partSrc pidxSrc =
  map
    (\(name, desc, pidx) → let
      effectPositionsWithOriginalOrder ∷ [(PartData, Int)]
      effectPositionsWithOriginalOrder
        = sortOn (fst ⋙ (^. #start))
        . take (pidx.effectCount + pidx.pendulumEffectCount)
        . drop pidx.firstEffectIndex
        $ zip allEffectPositions [0..]

      firstEffectStartPosition ∷ Int
      firstEffectStartPosition
        = maybe 99999999 (\(part,_) → part.start)
        $ find (\(partData, _) → partData.end ≢ 0) effectPositionsWithOriginalOrder

      leadingText ∷ Text
      leadingText = decodeUtf8 $ BL.take (fromIntegral firstEffectStartPosition) desc

      allEffects ∷ [Effect]
      allEffects =
        map
          (\((effectPos, originalPosition), i) → let
            nextEffectPos currentIndex =
              let next = effectPositionsWithOriginalOrder !!? (currentIndex + 1)
               in if (view #start . fst <$> next) ≡ Just 0 then nextEffectPos (currentIndex + 1)
                  else fst <$> next
            mainEffect
              = decodeUtf8
              . BL.take (fromIntegral (effectPos.end - effectPos.start))
              $ BL.drop (fromIntegral effectPos.start) desc
            in Effect
              { mainEffect = mainEffect
              , trailingText =
                  if mainEffect ≡ "" then ""
                  else decodeUtf8
                     . case nextEffectPos i of
                         Nothing   → identity
                         Just next → BL.take (fromIntegral (next.start - effectPos.end))
                     $ BL.drop (fromIntegral effectPos.end) desc
              , originalPosition = originalPosition
              }
          )
          (zip effectPositionsWithOriginalOrder [0..])

      effects ∷ [Effect]
      effects = take pidx.effectCount allEffects

      pendulumEffects ∷ [Effect]
      pendulumEffects = drop pidx.effectCount allEffects

      in Card name leadingText effects pendulumEffects
    )
    (zip3 names descs pidxData)
  where
  pidxData ∷ [PidxData]
  pidxData = runGet (void getWord32le ≫ many getSinglePidxData) pidxSrc

  allEffectPositions ∷ [PartData]
  allEffectPositions = runGet (many getSinglePartData) partSrc

  getSinglePidxData ∷ Get PidxData
  getSinglePidxData = do
    firstEffectIndex ← fromIntegral <$> getWord16le
    void getWord8
    counts ← getWord8
    let effectCount         = fromIntegral $ counts `div` 16 -- first digit
        pendulumEffectCount = fromIntegral $ counts `mod` 16 -- second digit
    pure $ PidxData firstEffectIndex effectCount pendulumEffectCount

  getSinglePartData ∷ Get PartData
  getSinglePartData = do
    start ← fromIntegral <$> getWord16le
    end ← fromIntegral <$> getWord16le
    pure $ PartData start end
