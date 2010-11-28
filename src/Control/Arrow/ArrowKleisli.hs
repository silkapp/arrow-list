{- |
The `ArrowKleisli' typeclass allows for embedding monadic function in Kleisli
arrows.
-}

{-# LANGUAGE
    TypeOperators
  , MultiParamTypeClasses
  , FlexibleInstances
  , FunctionalDependencies
  #-}
module Control.Arrow.ArrowKleisli where

import Control.Monad.Trans
import Control.Arrow

class (Monad m, Arrow (~>)) => ArrowKleisli m (~>) | (~>) -> m where
  arrM :: (a -> m b) -> a ~> b

instance Monad m => ArrowKleisli m (Kleisli m) where
  arrM f = Kleisli f

arrIO :: (MonadIO m, ArrowKleisli m (~>)) => (a -> IO b) -> a ~> b
arrIO f = arrM (liftIO . f)

