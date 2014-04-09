{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
module Control.Monad.Sequence
(
-- * The `Sequence' monad transformer.
  SeqT (..)
)
where

import Control.Applicative
import Control.Monad hiding (mapM, msum)
import Control.Monad.Trans
import Data.Foldable
import Data.Monoid
import Data.Sequence
import Data.Traversable
import Prelude hiding (mapM)

#if !MIN_VERSION_containers(0,5,2)
instance Applicative Seq where
  pure  = return
  (<*>) = ap

instance Alternative Seq where
  empty = mzero
  (<|>) = mplus
#endif

-- | Parameterizable `Sequence' monad, with an inner monad. The semantics of
-- `SeqT' are comparable to that of `ListT`.
--
-- /Note:/ Like the ListT monad, this does not yield a monad unless the
-- argument monad is commutative.
newtype SeqT m a = SeqT { runSeqT :: m (Seq a) }

mapSeqT :: (m (Seq a) -> n (Seq b)) -> SeqT m a -> SeqT n b
mapSeqT f = SeqT . f . runSeqT

instance Functor m => Functor (SeqT m) where
  fmap = mapSeqT . fmap . fmap

instance Applicative m => Applicative (SeqT m) where
  pure    = SeqT . pure . return
  a <*> b = SeqT (ap <$> runSeqT a <*> runSeqT b)

instance Applicative m => Alternative (SeqT m) where
  empty   = SeqT (pure mempty)
  a <|> b = SeqT (mappend <$> runSeqT a <*> runSeqT b)

instance Monad m => Monad (SeqT m) where
  return  = SeqT . return . return
  m >>= k = SeqT $
    do a <- runSeqT m
       b <- mapM (runSeqT . k) a
       return (msum b)
  fail _ = SeqT (return mempty)

instance Monad m => MonadPlus (SeqT m) where
  mzero       = SeqT (return mempty)
  m `mplus` n = SeqT (liftM2 mappend (runSeqT m) (runSeqT n))

instance MonadTrans SeqT where
  lift m = SeqT (liftM return m)

instance MonadIO m => MonadIO (SeqT m) where
  liftIO = lift . liftIO

