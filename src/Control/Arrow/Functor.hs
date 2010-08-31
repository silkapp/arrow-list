{-# LANGUAGE TypeOperators, Arrows #-}
module Control.Arrow.Functor where

import Control.Category
import Control.Arrow
import Prelude ()

class FunctorA f where
  mapA :: ArrowChoice (~>) => (a ~> b) -> (f a ~> f b)

instance FunctorA [] where
  mapA a = proc i ->
    case i of
      []   -> id -< []
      x:xs -> do y  <- a      -< x
                 ys <- mapA a -< xs
                 id -< y:ys

