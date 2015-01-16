{-# LANGUAGE
    FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , StandaloneDeriving
  , TypeOperators
  #-}
module Control.Arrow.List where

import Control.Arrow
import Control.Arrow.Kleisli.Class
import Control.Arrow.List.Class
import Control.Arrow.ListLike.Class
import Control.Category
import Control.Monad.Identity
import Control.Monad.List
import Prelude hiding (id, (.))

-- * ListT arrow.

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

-- * List arrow.

type ListArrow a b = ListTArrow Identity a b

runListArrow :: ListArrow a b -> a -> [b]
runListArrow a = runIdentity . runListTArrow a

instance Monad m => ArrowList (ListTArrow m) where
  arrL a   = ListTArrow (Kleisli (ListT . return . a))
  mapL f g = arrML (liftM f . runListTArrow g)

instance Monad m => ArrowListLike [] (ListTArrow m) where
  embed     = ListTArrow (Kleisli (ListT . return))
  observe f = ListTArrow . Kleisli $ \a -> ListT $
                return `liftM` runListT (runKleisli (runListTArrow' f) a)

-- * Embed a monadic function returning lists.

arrML :: (ArrowList arr, ArrowKleisli m arr) => (a -> m [b]) -> a `arr` b
arrML x = unlist . arrM x
