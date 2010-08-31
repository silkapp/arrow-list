{-# LANGUAGE
    TypeOperators
  , MultiParamTypeClasses
  , FlexibleContexts
  , FlexibleInstances
  #-}
module Control.Arrow.ArrowKleisli where

import Control.Arrow

class (Monad m, Arrow (~>)) => ArrowKleisli m (~>) where
  arrM :: (a -> m b) -> a ~> b

instance Monad m => ArrowKleisli m (Kleisli m) where
  arrM f = Kleisli f

