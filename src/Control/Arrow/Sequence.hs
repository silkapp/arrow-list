{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , TypeOperators
  , FlexibleInstances
  , MultiParamTypeClasses
  , StandaloneDeriving
  #-}
module Control.Arrow.Sequence where

import Control.Arrow
import Control.Arrow.ArrowF
import Control.Arrow.ArrowKleisli
import Control.Category
import Control.Monad.Identity
import Control.Monad.Sequence
import Data.Sequence
import Prelude hiding ((.), id, const)

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

type ListArrow a b = SeqTArrow Identity a b

runListArrow :: ListArrow a b -> a -> Seq b
runListArrow a = runIdentity . runSeqTArrow a

instance Monad m => ArrowF Seq (SeqTArrow m) where
  embed     = SeqTArrow (Kleisli (SeqT . return))
  observe f = SeqTArrow . Kleisli $ \a -> SeqT $
                singleton `liftM` runSeqT (runKleisli (runSeqTArrow' f) a)

