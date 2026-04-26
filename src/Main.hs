{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# HLINT ignore "Use zipWith" #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{-# HLINT ignore "Use ?~" #-}
module Main (main) where

import Card (Card (..))
import Card qualified
import Card.Decode qualified
import Card.Encode qualified
import Card.ProcessedEffect qualified
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Map qualified as Map
import Util (decodeFile)

main :: IO ()
main = do
    cards <- Card.Decode.fromDisk
    writeFileLBS "./data/decoded_cards.json" (encodePretty cards)
    let cardVersions = Card.ProcessedEffect.processAndGetAllVersions cards
    mapM_
        runWriteJob
        [ WriteJob
            { debugPath = "./data/decoded_cards.updated.json"
            , suffix = ".new"
            , cardData = cardVersions.regular
            }
        , WriteJob
            { debugPath = "./data/decoded_cards.updated.withNewlines.json"
            , suffix = ".withemptylines.new"
            , cardData = cardVersions.withDoubleNewlines
            }
        , WriteJob
            { debugPath = "./data/decoded_cards.updated.numberingOnly.json"
            , suffix = ".numberingOnly.new"
            , cardData = cardVersions.numberingAndNewlinesOnly
            }
        , WriteJob
            { debugPath = "./data/decoded_cards.updated.numberingAndNewlines.json"
            , suffix = ".numberingAndNewlines.new"
            , cardData = cardVersions.numberingAndDoubleNewlinesOnly
            }
        ]
    generateDiff
    echo "done"

data WriteJob = WriteJob
    { debugPath :: FilePath
    , suffix :: FilePath
    , cardData :: [Card Card.Effect]
    }

runWriteJob :: WriteJob -> IO ()
runWriteJob (WriteJob debugPath suffix cardData) = do
    writeFileLBS debugPath $ encodePretty cardData
    Card.Encode.generateDescAndPartFiles suffix cardData

data CardDiff
    = CardDiff
    { previous :: Maybe Text
    , _current :: Text
    }
    deriving (Eq, FromJSON, Generic, Show, ToJSON)

generateDiff :: IO ()
generateDiff = do
    let toMap cards = Map.fromList $ map (\c -> (c.name, Card.originalText c)) cards
    previousReleaseEffects <- toMap <$> decodeFile @[Card Card.Effect] "./release/effects.json"
    currentEffects <- toMap <$> decodeFile @[Card Card.Effect] "./data/decoded_cards.updated.json"
    let
        diff =
            Map.filter (\d -> d.previous /= Just d._current) $
                Map.mapWithKey
                    (\name -> CardDiff (Map.lookup name previousReleaseEffects))
                    currentEffects
    writeFileLBS "./data/difference_with_last_release.json" (encodePretty diff)
    echo "made diff"
