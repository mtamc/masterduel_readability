{-# OPTIONS_GHC -Wno-deprecations #-}
module Util where

import Data.Aeson hiding (Null)

decodeFile ∷ FromJSON a ⇒ FilePath → IO a
decodeFile f = eitherDecodeFileStrict f ≫= \case
  Right a  → pure a
  Left err → print (err ⊕ "\n" ⊕ f) ≫ error (toText (err ⊕ "\n" ⊕ f))

log ∷ (Show a1, Show a2) ⇒ a1 → a2 → a2
log lbl x = trace (show lbl ⊕ " | " ⊕ show x) x
