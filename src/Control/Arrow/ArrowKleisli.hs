{- |
The `ArrowKleisli' type class allows for embedding monadic operations in
Kleisli arrows.
-}
{-# LANGUAGE
    TypeOperators
  , MultiParamTypeClasses
  , FlexibleInstances
  , FunctionalDependencies
  #-}
module Control.Arrow.ArrowKleisli where

import Control.Arrow
import Control.Category
import Control.Monad.Trans
import Prelude hiding ((.), id)

class (Monad m, Arrow ar) => ArrowKleisli m ar | ar -> m where
  arrM :: (a -> m b) -> a `ar` b

instance Monad m => ArrowKleisli m (Kleisli m) where
  arrM f = Kleisli f

constM :: ArrowKleisli m ar => m b -> a `ar` b
constM a = arrM (const a)

effect :: ArrowKleisli m ar => m () -> a `ar` a
effect a = arrM (\b -> a >> return b)

arrIO :: (MonadIO m, ArrowKleisli m ar) => (a -> IO b) -> a `ar` b
arrIO f = arrM (liftIO . f)

