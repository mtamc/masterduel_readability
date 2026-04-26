{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Card (
    Card (..),
    originalText,
    Effect (..),
    effectText,
    PidxData (..),
    PartData (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as Text

data Card effectType
    = Card
    { name :: Text
    , -- Often the first registered effect is not at position 0
      leadingText :: Text
    , effects :: [effectType]
    , pendulumEffects :: [effectType]
    }
    deriving (Eq, FromJSON, Generic, Show, ToJSON)

instance Functor Card where
    fmap :: (a -> b) -> Card a -> Card b
    fmap f card =
        card
            { effects = map f card.effects
            , pendulumEffects = map f card.pendulumEffects
            }

originalText :: Card Effect -> Text
originalText c =
    Text.concat
        ( c.leadingText
            : map effectText c.effects
                <> map effectText c.pendulumEffects
        )

data Effect
    = Effect
    { mainEffect :: Text
    , trailingText :: Text
    -- ^ Sometimes there's text not part of any effect, most commonly " "
    -- in between effects, but can also be "unregistered" effect text
    , originalPosition :: Int
    -- ^ Some of the effects are out of order in the Part file... lower is first.
    }
    deriving (Eq, FromJSON, Generic, Show, ToJSON)

effectText :: Effect -> Text
effectText e = e.mainEffect <> e.trailingText

data PidxData
    = PidxData
    { firstEffectIndex :: Int
    -- ^ Position of the card's first effect in the Part asset (which is a sequence of effect positions)
    , effectCount :: Int
    , pendulumEffectCount :: Int
    }
    deriving (Eq, Generic, Show)

-- | Position of a single effect within a card's description. 0 is the first character of the card's description.
data PartData
    = PartData
    { start :: Int
    , end :: Int
    }
    deriving (Eq, Generic, Show)
