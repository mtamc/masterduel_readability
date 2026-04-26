{-# LANGUAGE DeriveAnyClass #-}

module Config (Paths (..), paths) where

import Data.Aeson (FromJSON, ToJSON)

data Paths
    = Paths
    { name :: FilePath
    , desc :: FilePath
    , part :: FilePath
    , pidx :: FilePath
    }
    deriving (Eq, FromJSON, Generic, Show, ToJSON)

paths :: Paths
paths =
    Paths
        { name = "./data/CARD_Name.bytes.dec.json"
        , desc = "./data/CARD_Desc.bytes.dec.json"
        , part = "./data/Card_Part.bytes.dec"
        , pidx = "./data/Card_Pidx.bytes.dec"
        }
