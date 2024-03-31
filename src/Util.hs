{-# OPTIONS_GHC -Wno-deprecations #-}
module Util where

import Data.Aeson       hiding (Null)
import Data.Text        qualified as Text
import System.Directory (createDirectory, doesDirectoryExist)
import Text.Megaparsec


decodeFile ∷ FromJSON a ⇒ FilePath → IO a
decodeFile f = eitherDecodeFileStrict f ≫= \case
  Right a  → pure a
  Left err → print (err ⊕ "\n" ⊕ f) ≫ error (toText (err ⊕ "\n" ⊕ f))

log ∷ (Show a1, Show a2) ⇒ a1 → a2 → a2
log lbl x = trace (show lbl ⊕ " | " ⊕ show x) x

textNonEmpty ∷ Text → Maybe Text
textNonEmpty txt = if Text.strip txt ≡ "" then Nothing else Just txt

strNonEmpty ∷ String → Maybe String
strNonEmpty = fmap toString . textNonEmpty . toText

mapTextHead ∷ (Text → Text) → Text → Text
mapTextHead f = Text.uncons ⋙ \case
  Nothing     → ""
  Just (x,xs) → f (one x) ⊕ xs

mapHead ∷ (a → a) → [a] → [a]
mapHead _ []     = []
mapHead f (x:xs) = f x : xs

mapLast ∷ (a → a) → [a] → [a]
mapLast f = reverse . mapHead f . reverse

someTill' ∷ (ToText [a], MonadPlus f) ⇒ f a → f end → f Text
someTill' a b = toText <$> someTill a b

manyTill' ∷ (ToText [a], MonadPlus f) ⇒ f a → f end → f Text
manyTill' a b = toText <$> manyTill a b

createDirectoryIfMissing ∷ FilePath → IO ()
createDirectoryIfMissing dirPath = do
    directoryExists ← doesDirectoryExist dirPath
    unless directoryExists $ createDirectory dirPath
