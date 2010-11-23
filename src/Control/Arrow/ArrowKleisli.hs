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

import Control.Arrow

class (Monad m, Arrow (~>)) => ArrowKleisli m (~>) | (~>) -> m where
  arrM :: (a -> m b) -> a ~> b

instance Monad m => ArrowKleisli m (Kleisli m) where
  arrM f = Kleisli f

