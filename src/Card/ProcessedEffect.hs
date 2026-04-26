{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Card.ProcessedEffect (processAndGetAllVersions, CardsVersions (..)) where

import Card (Card (..))
import Card qualified
import Control.Lens (over, (%~), (.~), (?~), (^.))
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (toUpper)
import Data.List (partition)
import Data.List.Extra (dropEnd)
import Data.Text qualified as Text
import Relude.Unsafe qualified as Unsafe
import Text.Megaparsec
import Text.Megaparsec.Char
import Util
import Prelude hiding (many)

-- | Data structure used internally to better represent effect elements
data ProcessedEffect
    = ProcessedEffect
    { leadingNewline :: Bool
    , enclosedNumber :: Maybe Text
    , tags :: [Text]
    , condition :: Maybe Text
    , activation :: Maybe Text
    , mainEffect :: Text
    , trailingText :: Text
    , originalPosition :: Int
    , unregistered :: Bool
    -- ^ Card.Effect with no offset registered in Card_Part, we parsed it manually
    }
    deriving (Eq, FromJSON, Generic, Show, ToJSON)

mapProcessedEffectText :: (Text -> Text) -> ProcessedEffect -> ProcessedEffect
mapProcessedEffectText f =
    (#condition %~ fmap f)
        >>> (#activation %~ fmap f)
        >>> (#mainEffect %~ f)

type Parser = Parsec Void Text

data CardVersions
    = CardVersions
    { regular :: Card Card.Effect
    , withDoubleNewlines :: Card Card.Effect
    , numberingAndNewlinesOnly :: Card Card.Effect
    , numberingAndDoubleNewlinesOnly :: Card Card.Effect
    }
    deriving (Eq, FromJSON, Generic, Show, ToJSON)

data CardsVersions
    = CardsVersions
    { regular :: [Card Card.Effect]
    , withDoubleNewlines :: [Card Card.Effect]
    , numberingAndNewlinesOnly :: [Card Card.Effect]
    , numberingAndDoubleNewlinesOnly :: [Card Card.Effect]
    }
    deriving (Eq, FromJSON, Generic, Show, ToJSON)

processAndGetAllVersions :: [Card Card.Effect] -> CardsVersions
processAndGetAllVersions = unzipVersions . map updateDesc

unzipVersions :: [CardVersions] -> CardsVersions
unzipVersions versions =
    CardsVersions
        { regular = map (.regular) versions
        , withDoubleNewlines = map (.withDoubleNewlines) versions
        , numberingAndNewlinesOnly = map (.numberingAndNewlinesOnly) versions
        , numberingAndDoubleNewlinesOnly = map (.numberingAndDoubleNewlinesOnly) versions
        }

updateDesc :: Card Card.Effect -> CardVersions
updateDesc card =
    let processedEffects = map effectToProcessed card.effects
        normalizedLeading = Text.strip card.leadingText
        (leadingWithoutUnregisteredEffects, effectsWithUnregistered) =
            getUnregisteredEffects normalizedLeading processedEffects
        numberedEffects = numberEffects leadingWithoutUnregisteredEffects effectsWithUnregistered
        numberedPendulumEffects =
            numberEffects "dummy" $ -- so first pendulum effect has a newline
                map effectToProcessed card.pendulumEffects
        withProcessedEffectsAndNormalizedLeading =
            Card
                { name = card.name
                , leadingText = leadingWithoutUnregisteredEffects
                , effects = numberedEffects
                , pendulumEffects = numberedPendulumEffects
                }
        reworded =
            mapAllText (Text.replace "SUMMONING conditions" "Summoning conditions")
                . uppercaseKeywords
                . splitActivationsAndConditions
                . mapAllText (rewordSearch . rewordBounce . rewordPiercing . rewordMill)
                . tagPhases
                . tagOncePerTurns
                . mapAllText
                    ( Text.replace "destroyed by battle or card effect" "destroyed"
                        . Text.replace "destroyed by battle or card effects" "destroyed"
                        . Text.replace "Send the top card of your Deck to the GY" "MILL 1 card"
                        . Text.replace "Graveyard" "GY"
                        . Text.replace "●" "● "
                    )
                $ tagQuickEffects withProcessedEffectsAndNormalizedLeading
        (rewordedLeadingText, rewordedEffects) =
            fromUnregisteredEffects False reworded.leadingText reworded.effects
        (rewordedLeadingTextWithEmptyLines, _) =
            fromUnregisteredEffects True reworded.leadingText reworded.effects
    in CardVersions
        { regular =
            Card
                { name = reworded.name
                , leadingText = rewordedLeadingText
                , effects = map (processedToEffect False) rewordedEffects
                , pendulumEffects = map (processedToEffect False) reworded.pendulumEffects
                }
        , withDoubleNewlines =
            Card
                { name = reworded.name
                , leadingText = rewordedLeadingTextWithEmptyLines
                , effects = map (processedToEffect True) rewordedEffects
                , pendulumEffects =
                    case reworded.pendulumEffects of
                        [] -> []
                        x : xs -> processedToEffect False x : map (processedToEffect True) xs
                }
        , numberingAndNewlinesOnly =
            Card
                { name = card.name
                , leadingText =
                    fst $ fromUnregisteredEffects False leadingWithoutUnregisteredEffects numberedEffects
                , effects = map (processedToEffect False) numberedEffects
                , pendulumEffects = map (processedToEffect False) numberedPendulumEffects
                }
        , numberingAndDoubleNewlinesOnly =
            Card
                { name = card.name
                , leadingText =
                    fst $ fromUnregisteredEffects True leadingWithoutUnregisteredEffects numberedEffects
                , effects = map (processedToEffect True) numberedEffects
                , pendulumEffects =
                    case numberedPendulumEffects of
                        [] -> []
                        x : xs -> processedToEffect False x : map (processedToEffect True) xs
                }
        }

fromUnregisteredEffects :: Bool -> Text -> [ProcessedEffect] -> (Text, [ProcessedEffect])
fromUnregisteredEffects emptyLines leadingText effects =
    let (unregisteredEffects, effectsWithoutUnregistered) = partition (.unregistered) effects
        leadingTextContainingUnregisteredEffects =
            applyWhen emptyLines singleNewlinesToDouble leadingText
                <> Text.concat (map (Card.effectText . processedToEffect emptyLines) unregisteredEffects)
    in (leadingTextContainingUnregisteredEffects, effectsWithoutUnregistered)

singleNewlinesToDouble :: Text -> Text
singleNewlinesToDouble = reword do
    before <- anySingleBut '\n'
    void newline
    after <- anySingleBut '\n'
    pure $ one before <> "\n\n" <> one after

{- | atm we only parse one specific case: zero registered effects and the entire
leading text is one sentence. if there are multiple sentences then it's very
hard to know if this is one or multiple effects and how to separate them.
-}
getUnregisteredEffects :: Text -> [ProcessedEffect] -> (Text, [ProcessedEffect])
getUnregisteredEffects leadingText effects
    | not (null effects) = (leadingText, effects)
    | (Text.length (Text.filter (== '.') leadingText) > 1)
        && not
            ( all
                (isAllowableSecondSentence . Text.strip . (<> "."))
                (drop 1 $ Text.split (== '.') leadingText)
            )
        && not (isInherentSummonThenOneSentence leadingText) =
        (leadingText, effects)
    | otherwise =
        let unregisteredEffectParser :: Parser (Text, [ProcessedEffect])
            unregisteredEffectParser = do
                cond <-
                    ( try do
                            let endP = noneOf ['.', ':'] *> void newline
                            condStart <- manyTill anySingle (try (lookAhead endP) <|> eof)
                            condLastChar <- one <$> anySingle
                            void newline
                            pure (condStart <> condLastChar)
                        )
                        <|> pure ""
                nomi <-
                    if isInherentSummonThenOneSentence leadingText
                        then do
                            fstSentence <- manyTill anySingle (try (lookAhead (void $ char '.')) <|> eof)
                            void $ char '.'
                            sndSentence <- manyTill anySingle (try (lookAhead (void $ char '.')) <|> eof)
                            void $ char '.'
                            void $ char ' '
                            pure $ fstSentence <> "." <> sndSentence <> "."
                        else pure ""
                effectTxt <- optional $ someTill anySingle eof
                let effect = basicProcessedEffectFromText 0 . toText <$> effectTxt
                pure (toText (cond <> nomi), maybeToList effect)
            (summoningCondition, unregisteredEffects) =
                fromRight (error "") $ parse unregisteredEffectParser "" leadingText
        in (summoningCondition, unregisteredEffects <> effects)

isInherentSummonThenOneSentence :: Text -> Bool
isInherentSummonThenOneSentence =
    Text.split (== '.') >>> \case
        [a, b, _, d] ->
            (Text.strip a == "Cannot be Normal Summoned/Set")
                && any
                    (`Text.isPrefixOf` Text.strip b)
                    [ "Must be Special Summoned "
                    , "This card cannot be Special Summoned except "
                    , "Must first be Special Summoned"
                    ]
                && (d == "")
        _ -> False

isAllowableSecondSentence :: Text -> Bool
isAllowableSecondSentence txt =
    ( txt
        `elem` [ "It cannot attack this turn."
               , "Your Deck is then shuffled."
               , "."
               ]
    )
        || ("Otherwise, " `Text.isPrefixOf` txt)
        || isJust (detectStandardHopt txt)

-- | this uses default value and is not the fully processed state
effectToProcessed :: Card.Effect -> ProcessedEffect
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

basicProcessedEffectFromText :: Int -> Text -> ProcessedEffect
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

processedToEffect :: Bool -> ProcessedEffect -> Card.Effect
processedToEffect emptyLines processed =
    Card.Effect
        { mainEffect =
            (if processed.leadingNewline then "\n" <> if emptyLines then "\n" else "" else "")
                <> maybe "" (<> " ") processed.enclosedNumber
                <> Text.concat (map ((<> " | ") . Text.toUpper) $ ordNub processed.tags)
                <> maybe "" formatCondition processed.condition
                <> maybe "" formatActivation processed.activation
                <> processed.mainEffect
        , trailingText =
            processed.trailingText
                & mapTextHead
                    ( \c ->
                        case (c == " ", c == "\n", emptyLines) of
                            (True, _, True) -> "\n\n"
                            (True, _, False) -> "\n"
                            (False, True, _) -> c
                            (False, False, True) -> "\n\n" <> c
                            (False, False, False) -> "\n" <> c
                    )
        , originalPosition = processed.originalPosition
        }

formatCondition :: Text -> Text
formatCondition = decorate "" ": "

formatActivation :: Text -> Text
formatActivation = decorate "<b>" ";</b> "

numberEffects :: Text -> [ProcessedEffect] -> [ProcessedEffect]
numberEffects cardLeadingText effects =
    let effectsThatNeedNumbering = filter (mustBeNumbered . (.mainEffect)) effects
    in fst $
        foldl'
            ( \(newEffects, enclosedNums) cur ->
                let
                    neEnclosedNums = nonEmpty enclosedNums
                    enclosedCurrent = maybe '①' head neEnclosedNums
                    enclosedRest = maybe [] tail neEnclosedNums
                    textSoFar =
                        Text.concat (cardLeadingText : map (Card.effectText . processedToEffect False) newEffects)
                    mustUseEnclosed =
                        ( length effectsThatNeedNumbering > 1
                            || all (\x -> Text.strip cardLeadingText /= x) ["", "dummy"]
                        )
                            && mustBeNumbered cur.mainEffect
                    mustAddNewline =
                        (Text.strip cur.mainEffect /= "") && (Text.strip textSoFar /= "")
                in
                    ( newEffects
                        <> [ cur
                                & (#leadingNewline .~ mustAddNewline)
                                & ( #enclosedNumber
                                        .~ if mustUseEnclosed
                                            then Just (one enclosedCurrent)
                                            else Nothing
                                  )
                                & (#trailingText %~ Text.stripEnd)
                           ]
                    , if mustUseEnclosed then enclosedRest else enclosedNums
                    )
            )
            ([], enclosedList)
            effects

enclosedList :: [Char]
enclosedList = "①②③④⑤⑥⑦⑧⑨⑩⑪⑫⑬⑬⑮⑯⑰⑱⑲⑳"

mustBeNumbered :: Text -> Bool
mustBeNumbered effectTxt =
    (Text.strip effectTxt /= "")
        && not ("●" `Text.isPrefixOf` Text.strip effectTxt)

mapAllText :: (Text -> Text) -> Card ProcessedEffect -> Card ProcessedEffect
mapAllText f =
    (#leadingText %~ f)
        >>> ( #effects
                %~ map
                    ( (#condition %~ fmap f)
                        >>> (#activation %~ fmap f)
                        >>> (#mainEffect %~ f)
                        >>> (#trailingText %~ f)
                    )
            )
        >>> ( #pendulumEffects
                %~ map
                    ( (#condition %~ fmap f)
                        >>> (#activation %~ fmap f)
                        >>> (#mainEffect %~ f)
                        >>> (#trailingText %~ f)
                    )
            )

uppercaseKeywords :: Card ProcessedEffect -> Card ProcessedEffect
uppercaseKeywords =
    mapAllText \txt ->
        foldr subStrToUpperCase txt substringsToCapitalize
            & ( \t ->
                    foldr
                        (\substr -> Text.replace (Text.toUpper substr) (Text.toLower substr))
                        t
                        substringsToUncapitalize
              )
  where
    substringsToUncapitalize :: [Text]
    substringsToUncapitalize =
        ["destroyed", "negated", "discarded", "banished", "excavated", "drew"]
    substringsToCapitalize :: [Text]
    substringsToCapitalize =
        -- [ "summoning", "summoned", "summon", "summons", "special summon" , "normal summon" , "tribute summon" , "ritual summon" , "pendulum summon" , "flip summon", "synchro summon" , "link summon" , "xyz summon" , "fusion summon", "normal or special summon"
        [ "destroy"
        , "destroying"
        , "destroyed"
        , "destroys"
        , "negate"
        , "negating"
        , "negated"
        , "negates"
        , "discard"
        , "discarding"
        , "discarded"
        , "discards"
        , -- , "extra deck"
          "quick effect"
        , -- , "hand"
          -- , "deck"
          "banish"
        , "banished"
        , "banishing"
        , "banishes"
        , "excavate"
        , "excavating"
        , "excavated"
        , "excavates"
        , "draw"
        , "draws"
        , "drew"
        , "drawing"
        , "drawn"
        , "piercing"
        ]
    subStrToUpperCase :: Text -> Text -> Text
    subStrToUpperCase "" str = str
    subStrToUpperCase _ "" = ""
    subStrToUpperCase needle haystack
        | Text.toLower needle `Text.isPrefixOf` Text.toLower haystack =
            Text.toUpper needle
                <> subStrToUpperCase needle (Text.drop (Text.length needle) haystack)
        | otherwise = Text.take 1 haystack <> subStrToUpperCase needle (Text.drop 1 haystack)

splitActivationsAndConditions :: Card ProcessedEffect -> Card ProcessedEffect
splitActivationsAndConditions =
    ( fmap \effect ->
        let (prea, condition, activation, mainEffect) = splitActivationsAndConditions' effect.mainEffect
        in effect
            & (#condition .~ ((maybe "" (<> "\n") prea <>) <$> condition))
            & (#activation .~ activation)
            & (#mainEffect .~ mainEffect)
    )
        >>> ( #leadingText
                %~ ( Text.splitOn "."
                        >>> map
                            ( removeSurroundingSpaces
                                >>> splitActivationsAndConditions'
                                >>> ( \(prea, cond, acti, mainEff) ->
                                        maybe "" (<> "\n") prea
                                            <> maybe
                                                ""
                                                ( \c ->
                                                    if "|" `Text.isInfixOf` c
                                                        then decorate "" ": " c
                                                        else formatCondition c
                                                )
                                                cond
                                            <> maybe "" (decorate "<b>" ";</b> ") acti
                                            <> mainEff
                                    )
                            )
                        >>> Text.intercalate ". "
                        >>> Text.replace ". )" ".)"
                        >>> Text.strip
                   )
            )

splitActivationsAndConditions' :: Text -> (Maybe Text, Maybe Text, Maybe Text, Text)
splitActivationsAndConditions' sentence =
    let (conditionWithPreamble, everythingAfterCondition) =
            case Text.split (== ':') sentence of
                [] -> error "should never happen"
                [noColon] -> (Nothing, noColon)
                cond : rest -> (textNonEmpty cond, removeSurroundingSpaces $ Text.intercalate ":" rest)
        (preamble, condition) =
            case Text.split (== '\n') $ fromMaybe "" conditionWithPreamble of
                [] -> error "should never happen"
                [noNewline] -> (Nothing, textNonEmpty noNewline)
                prea : rest ->
                    ( textNonEmpty prea
                    , fmap ((if isJust (textNonEmpty prea) then "" else "\n") <>)
                        . textNonEmpty
                        . removeSurroundingSpaces
                        $ Text.intercalate "\n" rest
                    )
        (activation, mainEffect) =
            case Text.split (== ';') everythingAfterCondition of
                [] -> error "should never happen"
                [noSemicolon] -> (Nothing, noSemicolon)
                acti : rest -> (textNonEmpty acti, removeSurroundingSpaces $ Text.intercalate ";" rest)
    in (preamble, condition, activation, mainEffect)

removeSurroundingSpaces :: Text -> Text
removeSurroundingSpaces = Text.dropWhileEnd (== ' ') . Text.dropWhile (== ' ')

tagPhases :: Card ProcessedEffect -> Card ProcessedEffect
tagPhases = fmap \effect ->
    case parsePhase effect.mainEffect of
        Nothing -> effect
        Just (phase, newMainText) -> effect & #tags %~ (phase :) & #mainEffect .~ newMainText
  where
    parsePhase :: Text -> Maybe (Text, Text)
    parsePhase =
        fromRight Nothing
            . parse
                ( do
                    _during <- string @String "During "
                    void . optional $ string "each of "
                    whose <- choice $ map string ["your opponent's", "the", "your"]
                    let
                        whoseAbbreviated =
                            case whose of
                                "the" -> ""
                                "your opponent's" -> "opp's "
                                "your" -> "your "
                                _ -> error "unexpected whose"
                    void $ char ' '
                    phaseTypeOrTurn <-
                        (toString <$> string "turn")
                            <|> ( do
                                    phaseType <- someTill anySingle (try (char ' '))
                                    void $ string' "phase"
                                    pluralS <- optional $ string "s"
                                    pure $ phaseType <> " phase" <> maybe "" toString pluralS
                                )
                    _connector <- char ':' <|> char ','
                    void $ char ' '
                    rest <- manyTill anySingle eof
                    let phaseOrTurnTag = if phaseTypeOrTurn == "turn" then "turn" else phaseTypeOrTurn
                    pure $ Just (toText (whoseAbbreviated <> phaseOrTurnTag), toText $ mapHead toUpper rest)
                )
                ""

reword :: Parser String -> Text -> Text
reword findAndReplaceP =
    fromRight (error "")
        . parse
            ( do
                everythingBefore <- manyTill anySingle $ try (void $ lookAhead findAndReplaceP) <|> eof
                replacement <- optional findAndReplaceP
                everythingAfter <- many anySingle
                -- TODO in future release: make this less hacky?
                let fixCommaPeriod = Text.replace ",." "."
                pure $ case replacement of
                    Nothing -> toText $ everythingBefore <> everythingAfter
                    Just found ->
                        reword findAndReplaceP $
                            fixCommaPeriod $
                                toText (everythingBefore <> found <> everythingAfter)
            )
            ""

rewordBounce :: Text -> Text
rewordBounce =
    Text.replace "returned to the hand" "bounced"
        . Text.replace "returned to the hand/Extra Deck" "bounced"
        . Text.replace "returned to the hand or Extra Deck" "bounced"
        . reword do
            void $ string' "return "
            thingToBounce <- someTill anyNotConjunction $ try do
                void $ string " "
                void $ optional (string "from the field ")
                void $ string "to the hand"
                optional (string "/Extra Deck" <|> string " or Extra Deck")
            pure $ "BOUNCE " <> thingToBounce

-- Piercing wordings:
-- If [this card that was SPECIAL SUMMONED from the GY] attacks a Defense Position monster, inflict piercing battle damage.
-- If [this card] attacks a Defense Position monster, inflict piercing battle damage {optional: to your opponent}.
-- While you control another Insect monster, if [an Insect monster you control] attacks a Defense Position monster, inflict piercing battle damage to your opponent.
-- During battle, when it attacks a Defense Position monster whose DEF is lower than the ATK of the equipped monster, inflict the difference as Battle Damage to your opponent.
rewordPiercing :: Text -> Text
rewordPiercing = reword do
    ifWord <- string' "if "
    thingThatIsPiercing <-
        someTill anyNotConjunction . try . choice $
            map
                string'
                [ "attacks a defense position monster, inflict piercing battle damage"
                , "attacks a Defense Position monster whose DEF is lower than the ATK of the equipped monster, inflict the difference as Battle Damage"
                ]
    void . optional $ string' " to your opponent"
    let capitalizedIfNeeded = applyWhen (ifWord == "If ") (mapHead toUpper)
    pure $ capitalizedIfNeeded thingThatIsPiercing <> "is PIERCING"

rewordMill :: Text -> Text
rewordMill = reword do
    void $ string' "send "
    thingToMill <- someTill anyNotConjunction $ try do
        void $ string' " from the top of your deck to the "
        choice $ map string' ["gy", "graveyard"]
    pure $ "MILL " <> thingToMill

notConjunction :: Parser ()
notConjunction = notFollowedBy $ choice $ map string' [".", ", then", ", and if you do", ", also"]

anyNotConjunction :: Parser Char
anyNotConjunction = notConjunction *> anySingle

rewordSearch :: Text -> Text
rewordSearch = reword do
    void $ string' "add "
    quantity <- numberChar
    hspace1
    thingToSearch <- someTill anyNotConjunction . try . lookAhead $ string' " from your deck"
    void $ string' " from your "
    deckOrGy <- choice $ map string' ["deck or gy", "deck"]
    void $ string' " to your hand"
    void $ optional $ char ','
    pure $
        "SEARCH "
            <> one quantity
            <> " "
            <> thingToSearch
            <> (if Text.toLower deckOrGy == "deck or gy" then " (from DECK or GY)" else "")

tagOncePerTurns :: Card ProcessedEffect -> Card ProcessedEffect
tagOncePerTurns =
    tagFollowingHoptInMonsterAndPend
        . tagHoptActis
        . tagLeadingHoptOneEffect
        . tagTrailingHoptOneEffect
        . tagTrailingHoptInMonsterAndPend
        . fmap tag
  where
    tag :: ProcessedEffect -> ProcessedEffect
    tag = tagPreviousHopt . tagSsHopt . tagStandardHopt . tagGainHopt . tagOptComma . tagOptColon
    -- e.g. Senet Switch
    tagOptColon :: ProcessedEffect -> ProcessedEffect
    tagOptColon effect
        | "Once per turn: " `Text.isInfixOf` effect.mainEffect =
            effect & #tags %~ ("opt" :) & #mainEffect %~ Text.replace "Once per turn: " ""
        | otherwise = effect
    -- e.g. Infernal Dragon
    tagOptComma :: ProcessedEffect -> ProcessedEffect
    tagOptComma effect
        | "Once per turn, " `Text.isInfixOf` effect.mainEffect =
            effect
                & #tags
                %~ ("opt" :)
                    & #mainEffect
                %~ (mapTextHead Text.toUpper . Text.replace "Once per turn, " "")
        | otherwise = effect
    -- e.g. Angel 01
    tagGainHopt :: ProcessedEffect -> ProcessedEffect
    tagGainHopt effect
        | "You can only gain this effect once per turn" `Text.isInfixOf` effect.mainEffect =
            effect & #tags %~ ("hopt" :)
        | otherwise = effect
    -- e.g. Motor Shell (trailingText)
    tagStandardHopt :: ProcessedEffect -> ProcessedEffect
    tagStandardHopt effect =
        case detectStandardHopt effect.mainEffect of
            Just changed -> effect & #mainEffect .~ changed & #tags %~ ("hopt" :)
            Nothing ->
                case detectStandardHopt effect.trailingText of
                    Just changed -> effect & #trailingText .~ changed & #tags %~ ("hopt" :)
                    Nothing -> effect
    tagPreviousHopt :: ProcessedEffect -> ProcessedEffect
    tagPreviousHopt effect =
        case detectPreviousHopt effect.trailingText of
            Just changed -> effect & #trailingText .~ changed & #tags %~ ("hopt" :)
            Nothing -> effect
    tagSsHopt :: ProcessedEffect -> ProcessedEffect
    tagSsHopt effect =
        case detectSsHopt effect.trailingText of
            Just changed -> effect & #trailingText .~ changed & #tags %~ ("hopt" :)
            Nothing -> effect
    tagFollowingHopt ::
        ( ([ProcessedEffect] -> [ProcessedEffect]) ->
          Card ProcessedEffect ->
          Card ProcessedEffect
        ) ->
        Card ProcessedEffect ->
        Card ProcessedEffect
    tagFollowingHopt overEffects card =
        card & overEffects (fst . foldl' f ([], False))
      where
        f (result, True) effect =
            ( result
                <> [ effect
                        & applyWhen
                            (effect.mainEffect /= "" || effect.trailingText /= "")
                            (#tags %~ ("hopt" :))
                        & #mainEffect
                        %~ ( \ef ->
                                applyWhen (not ("1 of these" `Text.isInfixOf` ef)) (Text.replace "● " "") ef
                           )
                            & applyWhen
                                (effect.mainEffect /= "" || effect.trailingText /= "")
                                ( #enclosedNumber
                                    ?~ ( case mapMaybe (textNonEmpty <=< (.enclosedNumber)) $ reverse result of
                                            [] -> one $ Unsafe.head enclosedList
                                            (Text.head -> lastEnclosed) : _ ->
                                                one . Unsafe.head . drop 1 $ dropWhile (/= lastEnclosed) enclosedList
                                       )
                                )
                   ]
            , True
            )
        f (result, False) effect =
            case detectFollowingHopt effect.trailingText of
                Just changed -> (result <> [effect & #trailingText .~ changed], True)
                Nothing -> (result <> [effect], False)
    tagFollowingHoptInMonsterAndPend :: Card ProcessedEffect -> Card ProcessedEffect
    tagFollowingHoptInMonsterAndPend =
        tagFollowingHopt (over #effects) . tagFollowingHopt (over #pendulumEffects)
    tagTrailingHopt ::
        (Card ProcessedEffect -> [ProcessedEffect]) ->
        ( ([ProcessedEffect] -> [ProcessedEffect]) ->
          Card ProcessedEffect ->
          Card ProcessedEffect
        ) ->
        Card ProcessedEffect ->
        Card ProcessedEffect
    tagTrailingHopt getEffects overEffects card =
        case viaNonEmpty last (getEffects card) of
            Nothing -> card
            Just effect ->
                case detectStandardHoptEach effect.mainEffect of
                    Just changed ->
                        card
                            & ( #effects
                                    %~ ( (\effs -> dropEnd 1 effs <> [effect & #mainEffect .~ changed])
                                            >>> map
                                                ( \ef ->
                                                    applyWhen
                                                        (ef.mainEffect /= "" || ef.trailingText /= "")
                                                        (#tags %~ ("hopt" :))
                                                        ef
                                                )
                                       )
                              )
                    Nothing ->
                        case detectStandardHoptEach effect.trailingText of
                            Just changed ->
                                card
                                    & overEffects
                                        ( (\effs -> dropEnd 1 effs <> [effect & #trailingText .~ changed])
                                            >>> map
                                                ( \ef ->
                                                    applyWhen
                                                        (ef.mainEffect /= "" || ef.trailingText /= "")
                                                        (#tags %~ ("hopt" :))
                                                        ef
                                                )
                                        )
                            Nothing ->
                                case detectStandardHoptEach card.leadingText of
                                    Just changed ->
                                        card
                                            & (#leadingText .~ changed)
                                            & overEffects
                                                ( map
                                                    ( \ef ->
                                                        applyWhen
                                                            ((ef.mainEffect /= "") || (ef.trailingText /= ""))
                                                            (#tags %~ ("hopt" :))
                                                            ef
                                                    )
                                                )
                                    Nothing -> card
    -- eg
    -- last effect trailing text: Fluffal Bear/Infernoble Arms Durendal
    --  You can only use 1 \"Fluffal Bear\" effect per turn, and only once that turn.
    --
    --  chaos witch is the only one where the last effect triling text is:
    --   You can only use 1 \"Chaos Witch\" effect per turn, and only once that turn, also you cannot SPECIAL SUMMON monsters from the Extra DECK, except LIGHT or DARK Synchro Monsters, the turn you activate either effect.
    --   not yet supported/implemented
    tagTrailingHoptOneEffect ::
        Card ProcessedEffect ->
        Card ProcessedEffect
    tagTrailingHoptOneEffect card =
        case viaNonEmpty last card.effects of
            Nothing -> card
            Just lastEffect ->
                case detectOneOnlyOnce lastEffect.trailingText of
                    Just changed ->
                        card
                            & ( #effects
                                    %~ ( mapHead (#leadingNewline .~ True)
                                            . mapLast (#trailingText .~ changed)
                                       )
                              )
                            & ( #leadingText
                                    %~ ( \leading ->
                                            leading
                                                <> (if leading == "" then "" else "\n")
                                                <> "HOPT | You can use ONE of —"
                                       )
                              )
                    Nothing -> card
    -- first effect's trailing text: Scarm, Malebranche of the Burning Abyss
    -- " You can only use 1 of these effects of \"Scarm, Malebranche of the Burning Abyss\" per turn, and only once that turn."
    --
    -- Graff, Malebranche of the Burning Abyss, same^
    -- Naturia Sacred Tree:  You can only use 1 of the following effects of \"Naturia Sacred Tree\" per turn, and only once that turn.
    tagLeadingHoptOneEffect ::
        Card ProcessedEffect ->
        Card ProcessedEffect
    tagLeadingHoptOneEffect card =
        case viaNonEmpty head card.effects of
            Nothing -> card
            Just firstEffect ->
                case detectFollowingOneOnlyOnce firstEffect.trailingText of
                    Just changed ->
                        card
                            & ( #effects
                                    %~ mapHead
                                        ( #trailingText
                                            .~ changed
                                                <> ((if changed == "" then "" else "\n") <> "HOPT | You can use ONE of:")
                                        )
                              )
                    Nothing ->
                        case detectFollowingOneOnlyOnce card.leadingText of
                            Nothing -> card
                            Just changed ->
                                card
                                    & ( #leadingText
                                            .~ ( changed
                                                    <> (if changed == "" then "" else "\n")
                                                    <> "HOPT | You can use ONE of:"
                                               )
                                      )
    tagTrailingHoptInMonsterAndPend :: Card ProcessedEffect -> Card ProcessedEffect
    tagTrailingHoptInMonsterAndPend =
        tagTrailingHopt (^. #effects) (over #effects)
            . tagTrailingHopt (^. #pendulumEffects) (over #pendulumEffects)
    tagHoptActis :: Card ProcessedEffect -> Card ProcessedEffect
    tagHoptActis card
        | any (\e -> isJust $ detectActiHopt e.trailingText) card.effects =
            card
                & ( #leadingText
                        %~ (("HOPT ACTIVATION\n" <>) >>> Text.strip)
                  )
                & ( #effects
                        %~ mapHead (#leadingNewline .~ True)
                  )
                & ( #effects
                        %~ map
                            ( \e ->
                                detectActiHopt e.trailingText
                                    & \case
                                        Just changed -> e & #trailingText .~ changed
                                        Nothing -> e
                            )
                  )
        | otherwise = card

detectAndRemove :: Parser Text -> Text -> Maybe Text
detectAndRemove partToRemoveP =
    fromRight (error "")
        . parse
            ( do
                everythingBeforeHopt <-
                    manyTill anySingle (try (void (lookAhead partToRemoveP)) <|> eof)
                partToRemove <- optional partToRemoveP
                everythingAfter <- many anySingle
                pure $
                    if isJust partToRemove
                        then
                            Just
                                . Text.strip
                                . toText
                                $ everythingBeforeHopt <> maybe "" (" " <>) (strNonEmpty everythingAfter)
                        else
                            Nothing
            )
            ""

standardHoptParser :: Text -> Text -> Parser Text
standardHoptParser start end = standardHoptParser' (string start) (string end)

standardHoptParser' :: Parser Text -> Parser Text -> Parser Text
standardHoptParser' startP endP = do
    startStr <- startP
    name <- someTill' anySingle (try (lookAhead endP))
    endStr <- endP
    spc <- maybe "" one <$> optional (char ' ')
    pure $ startStr <> name <> endStr <> spc

-- | Reports if Hopt detected and returns the string with the Hopt removed
detectStandardHopt :: Text -> Maybe Text
detectStandardHopt =
    detectAndRemove $
        choice
            [ standardHoptParser "You can only use this effect of \"" "\" once per turn."
            , -- Gather Your Mind only
              standardHoptParser "You can only use 1 \"" "\" per turn."
            ]

detectStandardHoptEach :: Text -> Maybe Text
detectStandardHoptEach =
    detectAndRemove $
        standardHoptParser'
            ( choice $
                map
                    string
                    [ "You can only use each effect of \""
                    , "You can only use each Pendulum Effect of \""
                    ]
            )
            (string "\" once per turn.")

detectPreviousHopt :: Text -> Maybe Text
detectPreviousHopt =
    detectAndRemove $
        standardHoptParser
            "You can only use the previous effect of \""
            "\" once per turn."

detectOneOnlyOnce :: Text -> Maybe Text
detectOneOnlyOnce =
    detectAndRemove $
        standardHoptParser
            "You can only use 1 \""
            "\" effect per turn, and only once that turn."

detectFollowingOneOnlyOnce :: Text -> Maybe Text
detectFollowingOneOnlyOnce =
    detectAndRemove $
        standardHoptParser'
            ( choice $
                map
                    string
                    [ "You can only use 1 of these effects of \""
                    , "You can only use 1 of the following effects of \""
                    ]
            )
            (string "\" per turn, and only once that turn.")

detectActiHopt :: Text -> Maybe Text
detectActiHopt =
    detectAndRemove $
        standardHoptParser
            "You can only activate 1 \""
            "\" per turn."

detectSsHopt :: Text -> Maybe Text
detectSsHopt =
    detectAndRemove $
        standardHoptParser
            "You can only Special Summon \""
            "\" once per turn this way."

detectFollowingHopt :: Text -> Maybe Text
detectFollowingHopt =
    detectAndRemove $
        standardHoptParser'
            ( choice $
                map
                    string
                    [ "You can only use each of the following effects of \""
                    , "You can only use each of these effects of \""
                    ]
            )
            (string "\" once per turn.")

tagQuickEffects :: Card ProcessedEffect -> Card ProcessedEffect
tagQuickEffects = fmap tag
  where
    tag :: ProcessedEffect -> ProcessedEffect
    tag effect =
        let
            isQuick =
                any
                    (maybe False hasQuickEffectText)
                    [Just effect.mainEffect, effect.activation, effect.condition]
        in
            effect
                & applyWhen isQuick (mapProcessedEffectText removeQuickEffectText . (#tags %~ ("quick" :)))

quickEffectP :: Parser ()
quickEffectP = do
    void $ optional (char ' ')
    void $ char '('
    void . optional $ string' "this is a "
    void $ string' "quick effect"
    void $ optional $ char '.'
    void $ char ')'
    pass

removeQuickEffectP :: Parser Text
removeQuickEffectP = do
    prefix <- manyTill anySingle $ try (lookAhead quickEffectP) <|> eof
    try (quickEffectP <|> eof)
    suffix <- many anySingle
    pure . toText $ prefix <> suffix

removeQuickEffectText :: Text -> Text
removeQuickEffectText = toText . fromRight (error "") . parse removeQuickEffectP ""

hasQuickEffectText :: Text -> Bool
hasQuickEffectText text = removeQuickEffectText text /= text
