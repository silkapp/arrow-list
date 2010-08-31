{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , TypeOperators
  , FlexibleInstances
  , MultiParamTypeClasses
  , StandaloneDeriving
  #-}
module Control.Arrow.List where

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Arrow.ArrowKleisli
import Control.Arrow.ArrowList
import Control.Category
import Control.Monad.Identity
import Control.Monad.List

newtype ListTArrow m a b = ListTArrow { runListTArrow' :: Kleisli (ListT m) a b }
  deriving
    ( Category
    , Arrow
    , ArrowZero
    , ArrowPlus
    , ArrowApply
    , ArrowChoice
    )

instance Monad m => ArrowKleisli m (ListTArrow m) where
  arrM a = ListTArrow (Kleisli (ListT . (liftM return . a)))

runListTArrow :: ListTArrow m a b -> a -> m [b]
runListTArrow a = runListT . runKleisli (runListTArrow' a)

type ListArrow a b = ListTArrow Identity a b

runListArrow :: ListArrow a b -> a -> [b]
runListArrow a = runIdentity . runListTArrow a

instance Monad m => ArrowList (ListTArrow m) where
  arrL a   = ListTArrow (Kleisli (ListT . return . a))
  mapL f g = arrML (liftM f . runListTArrow g)

arrML :: (ArrowList (~>), ArrowKleisli m (~>)) => (a -> m [b]) -> a ~> b
arrML x = unlistA . arrM x

