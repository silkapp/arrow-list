{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , TypeOperators
  , FlexibleInstances
  , MultiParamTypeClasses
  , StandaloneDeriving
  #-}
module Control.Arrow.List.Ordered where

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Arrow.ArrowKleisli
import Control.Arrow.ArrowF
import Control.Category
import Control.Monad.Identity
import Data.List.Ordered

-- * ListT arrow for ordered lists.

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

runListTArrow :: ListTArrow m a b -> a -> m (List b)
runListTArrow a = runListT . runKleisli (runListTArrow' a)

-- * List arrow for ordered lists.

type ListArrow a b = ListTArrow Identity a b

runListArrow :: ListArrow a b -> a -> List b
runListArrow a = runIdentity . runListTArrow a

instance Monad m => ArrowF List (ListTArrow m) where
  embed     = ListTArrow (Kleisli (ListT . return))
  observe f = ListTArrow . Kleisli $ \a -> ListT $
                singleton `liftM` runListT (runKleisli (runListTArrow' f) a)

-- * Embed a monadic function returning an ordered list into a container arrow.

arrML :: (ArrowF f (~>), ArrowKleisli m (~>)) => (a -> m (f c)) -> a ~> c
arrML x = embed . arrM x

