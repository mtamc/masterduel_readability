{-# LANGUAGE OverloadedRecordDot #-}

module Card.Encode (generateDescAndPartFiles) where

import Card (Card)
import Card qualified
import Config qualified
import Control.Lens
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Binary.Builder qualified as Binary
import Data.ByteString qualified as BS
import Data.Text qualified as Text

generateDescAndPartFiles :: FilePath -> [Card Card.Effect] -> IO ()
generateDescAndPartFiles suffix cards = do
    let cardTexts = map Card.originalText cards
        newPartSrc =
            Binary.toLazyByteString
                . mconcat
                $ (Binary.putWord16le 0 : Binary.putWord16le 0 : map partDataToBinary (toPartDatas cards))
    writeFileLBS (Config.paths.part <> suffix) newPartSrc
    writeFileLBS (Config.paths.desc <> suffix) $ encodePretty cardTexts
  where
    partDataToBinary :: Card.PartData -> Binary.Builder
    partDataToBinary (Card.PartData start end) =
        Binary.putWord16le (fromIntegral start) <> Binary.putWord16le (fromIntegral end)

    toPartDatas :: [Card Card.Effect] -> [Card.PartData]
    toPartDatas = concatMap toCardPartDatas

    toCardPartDatas :: Card Card.Effect -> [Card.PartData]
    toCardPartDatas card =
        let
            originalText = Card.originalText card
            partDatas =
                ( map (toEffectPartData originalText)
                    . filter (\e -> e.originalPosition /= 0)
                    $ sortOn (^. #originalPosition) card.effects
                )
                    <> ( map (toEffectPartData (disregardMonsterEffects originalText))
                            . filter (\e -> e.originalPosition /= 0)
                            $ sortOn (^. #originalPosition) card.pendulumEffects
                       )
        in
            partDatas

    toEffectPartData :: Text -> Card.Effect -> Card.PartData
    toEffectPartData originalText e =
        let start =
                fromMaybe (error $ "|| " <> e.mainEffect <> " | " <> originalText) $
                    utf8SubIndex e.mainEffect originalText
            end = start + BS.length (encodeUtf8 e.mainEffect)
        in Card.PartData start end

    disregardMonsterEffects :: Text -> Text
    disregardMonsterEffects txtAsText =
        let txtAsBs = encodeUtf8 txtAsText
            pendulumMarkerIndex = fromMaybe 0 $ utf8SubIndex "[Pendulum Effect]" txtAsText
        in decodeUtf8 (BS.replicate pendulumMarkerIndex 0 <> BS.drop pendulumMarkerIndex txtAsBs)

    utf8SubIndex :: Text -> Text -> Maybe Int
    utf8SubIndex substr = go "" ""
      where
        go incorrect correct (Text.uncons -> Nothing)
            | substr == "" = Just 0
            | correct == substr = Just . BS.length $ encodeUtf8 incorrect
            | otherwise =
                Nothing
        go incorrect correct (Text.uncons -> Just (cur, rest))
            | correctPlusCur == substr = Just . BS.length $ encodeUtf8 incorrect
            | correctPlusCur `Text.isPrefixOf` substr = go incorrect correctPlusCur rest
            | one cur `Text.isPrefixOf` substr = go incorrectPlusCorrect (one cur) rest
            | otherwise = go incorrectPlusCorrectPlusCur "" rest
          where
            correctPlusCur = correct `Text.snoc` cur
            incorrectPlusCorrect = incorrect <> correct
            incorrectPlusCorrectPlusCur = incorrect <> correct `Text.snoc` cur
