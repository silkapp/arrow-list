{-# LANGUAGE
    FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , StandaloneDeriving
  , TypeOperators
  #-}
module Control.Arrow.Sequence where

import Control.Arrow
import Control.Arrow.Kleisli.Class
import Control.Arrow.ListLike.Class
import Control.Category
import Control.Monad.Identity
import Control.Monad.Sequence
import Data.Sequence
import Prelude hiding (const, id, (.))

-- * SeqT arrow.

newtype SeqTArrow m a b = SeqTArrow { runSeqTArrow' :: Kleisli (SeqT m) a b }
  deriving
    ( Category
    , Arrow
    , ArrowZero
    , ArrowPlus
    , ArrowApply
    , ArrowChoice
    )

instance Monad m => ArrowKleisli m (SeqTArrow m) where
  arrM a = SeqTArrow (Kleisli (SeqT . (liftM return . a)))

runSeqTArrow :: SeqTArrow m a b -> a -> m (Seq b)
runSeqTArrow a = runSeqT . runKleisli (runSeqTArrow' a)

-- * Seq arrow.

type SeqArrow a b = SeqTArrow Identity a b

runSeqArrow :: SeqArrow a b -> a -> Seq b
runSeqArrow a = runIdentity . runSeqTArrow a

instance Monad m => ArrowListLike Seq (SeqTArrow m) where
  embed     = SeqTArrow (Kleisli (SeqT . return))
  observe f = SeqTArrow . Kleisli $ \a -> SeqT $
                singleton `liftM` runSeqT (runKleisli (runSeqTArrow' f) a)

