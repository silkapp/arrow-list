{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, Arrows #-}
module Control.Arrow.Instances where

import Control.Applicative
import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)

instance Arrow (~>) => Functor ((~>) a) where
  fmap f a = arr f . a

instance Arrow (~>) => Applicative ((~>) a) where
  pure a = arr (const a)
  a <*> b = proc i ->
    do x <- a -< i
       y <- b -< i
       id -< x y

instance ArrowApply (~>) => Monad ((~>) a) where
  return a = arr (const a)
  a >>= b = proc i ->
    do x <- a -< i
       b x -<< i

