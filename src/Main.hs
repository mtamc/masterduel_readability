{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use zipWith" #-}
module Main (main) where

import Control.Lens
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Binary.Builder      as Binary
import Data.Binary.Get
import Data.ByteString          qualified as BS
import Data.ByteString.Lazy     qualified as BL
import Data.Text                qualified as Text
import Relude.Unsafe            qualified as Unsafe
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
    { name ∷ FilePath
    , desc ∷ FilePath
    , part ∷ FilePath
    , pidx ∷ FilePath
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

data Card
  = Card
    { name            ∷ Text
      -- Often the first registered effect is not at position 0
    , leadingText     ∷ Text
    , effects         ∷ [Effect]
    , pendulumEffects ∷ [Effect]
    }
  deriving (Eq, FromJSON, Generic, Show, ToJSON)

cardOriginalText ∷ Card → Text
cardOriginalText c =
  Text.concat
    ( c.leadingText
    : map effectText c.effects
    ⊕ map effectText c.pendulumEffects
    )

data Effect
  = Effect
    { content          ∷ Text
      -- Sometimes there's text not part of any effect, most commonly " "
      -- in between effects, but can also be "unregistered" effect text
    , trailingText     ∷ Text
      -- Some of the effects are out of order in the Part file... lower is first.
    , originalPosition ∷ Int
    }
  deriving (Eq, FromJSON, Generic, Show, ToJSON)

effectText ∷ Effect → Text
effectText e = e.content ⊕ e.trailingText

data PidxData
  = PidxData
    { -- Position of the card's first effect in the Part asset (which is a sequence of effect positions)
      firstEffectIndex    ∷ Int
    , effectCount         ∷ Int
    , pendulumEffectCount ∷ Int
    }
  deriving (Eq, Generic, Show)

-- | Position of a single effect within a card's description. 0 is the first
-- character of the card's description.
data PartData
  = PartData
    { start ∷ Int
    , end   ∷ Int
    }
  deriving (Eq, Generic, Show)


-- UPDATING DESCS

updateDesc ∷ Card → Card
updateDesc = numberEffects

numberEffects ∷ Card → Card
numberEffects card = card
  { effects =
      fst $ foldl'
        (\(newEffects, enclosedNums) cur →
          let
            neEnclosedNums = nonEmpty enclosedNums
            enclosedCurrent = maybe '①' head neEnclosedNums
            enclosedRest = maybe [] tail neEnclosedNums
            textSoFar = Text.concat (card.leadingText : map effectText newEffects)
            mustUseEnclosed = Text.strip cur.content ≢ ""
            mustAddNewline = Text.strip cur.content ≢ "" ∧ Text.strip textSoFar ≢ ""
            newPrefix
              = (if mustAddNewline then "\n" else "")
              ⊕ (if mustUseEnclosed then Text.cons enclosedCurrent " " else "")
          in
          ( newEffects
            ⊕ [ cur
                & #content %~ (newPrefix ⊕)
                & #trailingText %~ Text.stripEnd

              ]
          , if mustUseEnclosed then enclosedRest else enclosedNums
          )
        )
        ([], enclosedNumbers)
        card.effects
  , leadingText = Text.strip card.leadingText
  }
  where
  enclosedNumbers = "①②③④④⑥⑦⑧⑨⑩⑪⑫⑬⑬⑮⑯⑰⑱⑲⑳"


-- ENCODING

generateDescAndPartFiles ∷ [Card] → IO ()
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

  toPartDatas ∷ [Card] → [PartData]
  toPartDatas = concatMap toCardPartDatas

  toCardPartDatas ∷ Card → [PartData]
  toCardPartDatas card = let
    originalText = cardOriginalText card
    partDatas
      = map (toEffectPartData originalText)
        (sortOn (^. #originalPosition) card.effects)
      ⊕ map (toEffectPartData (disregardMonsterEffects originalText))
        (sortOn (^. #originalPosition) card.pendulumEffects)
    in partDatas

  toEffectPartData ∷ Text → Effect → PartData
  toEffectPartData originalText e = let
    start = Unsafe.fromJust $ utf8SubIndex e.content originalText
    end = start + BS.length (encodeUtf8 e.content)
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

getCards ∷ IO [Card]
getCards =
  mkCards
    <$> decodeFile paths.name
    <*> decodeFile paths.desc
    <*> readFileLBS paths.part
    <*> readFileLBS paths.pidx

mkCards ∷ [Text] → [Text] → BL.ByteString → BL.ByteString → [Card]
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
            content
              = decodeUtf8
              . BL.take (fromIntegral (effectPos.end - effectPos.start))
              $ BL.drop (fromIntegral effectPos.start) desc
            in Effect
              { content = content
              , trailingText =
                  if content ≡ "" then ""
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
