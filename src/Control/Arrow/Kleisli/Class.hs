{- |
The `ArrowKleisli' type class allows for embedding monadic operations in
Kleisli arrows.
-}
{-# LANGUAGE
    FlexibleInstances
  , FunctionalDependencies
  , MultiParamTypeClasses
  , TypeOperators
  #-}
module Control.Arrow.Kleisli.Class where

import Control.Arrow
import Control.Category
import Control.Monad.Trans
import Prelude hiding (id, (.))

class (Monad m, Arrow arr) => ArrowKleisli m arr | arr -> m where
  arrM :: (a -> m b) -> a `arr` b

instance Monad m => ArrowKleisli m (Kleisli m) where
  arrM f = Kleisli f

constM :: ArrowKleisli m arr => m b -> a `arr` b
constM a = arrM (const a)

effect :: ArrowKleisli m arr => m () -> a `arr` a
effect a = arrM (\b -> a >> return b)

arrIO :: (MonadIO m, ArrowKleisli m arr) => (a -> IO b) -> a `arr` b
arrIO f = arrM (liftIO . f)
