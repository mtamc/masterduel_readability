module Prelude (
    module Control.Exception.Safe,
    module Control.Monad.Except,
    module Relude,
    applyWhen,
    decorate,
    echo,
    justIf,
    positJust,
) where

import Control.Exception.Safe (MonadCatch, MonadThrow, catchAny)
import Control.Monad.Except
import Data.Generics.Labels ()
import Relude

-- | Short name for putTextLn
echo :: (MonadIO m) => Text -> m ()
echo = putTextLn

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen cond f a = if cond then f a else a

justIf :: Bool -> a -> Maybe a
justIf cond a = if cond then Just a else Nothing

positJust :: (MonadError e m) => e -> Maybe a -> m a
positJust _err (Just a) = pure a
positJust err Nothing = throwError err

decorate :: (Monoid m) => m -> m -> m -> m
decorate left right a = left <> a <> right
