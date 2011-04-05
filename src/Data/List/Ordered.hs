{-# LANGUAGE
    DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , TupleSections
  #-}
{- | Ordered list type with efficient and streaming observation in two directions.  -}
module Data.List.Ordered
( 
-- * Ordered list type.
  List
, Direction (..)

-- * Constructing ordered lists.
, empty
, singleton
, add
, merge
, merges

-- * Operations on ordered lists.
, mapMonotonic
, mergeMap
, filter
, take
, drop
, length
, atLeast
, null

-- * Creation from Haskell sequences.
, fromSeq
, fromSeqs
, fromAscSeq
, fromDescSeq

-- * Creation from Haskell lists.
, fromList
, fromAscList
, fromDescList
, fromAscOrDescList
, fromLists

-- * Creation from Maps.
, fromMap
, fromMapRange

-- * Observing as Haskell sequences.
, toSeq
, toUnorderedSeq
, toAscSeq
, toDescSeq

-- * Observing as regular Haskell list.
, toList
, toUnorderedList
, toAscList
, toDescList

-- * List monad transformer wrapper.
, ListT (..)

-- * Internally used helper functions.
, mergeBy
, localNubBy
, mapRange
)
where

import Control.Applicative hiding (empty)
import Control.Monad.Identity
import Control.Monad.Trans
import Data.Function (on)
import Data.Ord
import Data.Maybe
import Data.Monoid
import Data.Foldable (Foldable (foldMap))
import Prelude hiding (filter, drop, take, length, null)
import Data.Sequence (Seq, breakl, viewl, ViewL(..), (<|))
import qualified Control.Applicative
import qualified Data.Sequence as S
import qualified Data.Map as M
import qualified Data.Foldable as F

-------------------------------------------------------------------------------

data List a =
    FromAsc       (Seq a)
  | FromDesc      (Seq a)
  | FromAscOrDesc (Seq a) (Seq a)
  | Merge         (List a) (List a)
  | Take          Int (List a)
  | Drop          Int (List a)

instance Foldable List where
  foldMap f = foldMap f . toUnorderedList

instance Ord a => Eq (List a) where
  a == b = compare a b == EQ 

instance Ord a => Ord (List a) where
  compare = compare `on` toAscList

instance (Ord a, Show a) => Show (List a) where
  show = show . toAscList

instance Monoid (List a) where
  mempty  = empty
  mappend = merge

-- | A sorting direction, either ascending or descending.
data Direction = Asc | Desc
  deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------

empty :: List a
empty = FromAscOrDesc S.empty S.empty

-- | /O(1)/ Create a singleton ordered list.

singleton :: a -> List a
singleton x = FromAscOrDesc (S.singleton x) (S.singleton x)

-- | /O(1)/ Add a single element to an ordered list.

add :: a -> List a -> List a
add x = Merge (singleton x)

-- | /O(1)/ Merge two ordered lists.

merge :: List a -> List a -> List a
merge a b | null a    = b
          | null b    = a
          | otherwise = Merge a b

merges :: [List a] -> List a
merges = foldr merge empty

mapMonotonic :: (a -> b) -> List a -> List b
mapMonotonic f (FromAsc       xs   ) = FromAsc       (fmap f xs)
mapMonotonic f (FromDesc      xs   ) = FromDesc      (fmap f xs)
mapMonotonic f (FromAscOrDesc xs ys) = FromAscOrDesc (fmap f xs) (fmap f ys)
mapMonotonic f (Merge         xs ys) = Merge         (mapMonotonic f xs) (mapMonotonic f ys)
mapMonotonic f (Take        j xs   ) = Take j        (mapMonotonic f xs)
mapMonotonic f (Drop        j xs   ) = Drop j        (mapMonotonic f xs)

mergeMap :: (a -> List b) -> List a -> List b
mergeMap f = merges . map f . toUnorderedList

filter :: (a -> Bool) -> List a -> List a
filter f (FromAsc       xs   ) = FromAsc       (S.filter f xs)
filter f (FromDesc      xs   ) = FromDesc      (S.filter f xs)
filter f (FromAscOrDesc xs ys) = FromAscOrDesc (S.filter f xs) (S.filter f ys)
filter f (Merge         xs ys) = Merge         (filter f xs) (filter f ys)
filter f (Take        j xs   ) = Take j        (filter f xs)
filter f (Drop        j xs   ) = Drop j        (filter f xs)

-- | /O(n) Take a fixed number of items from the beginning of the list.

take :: Int -> List a -> List a
take j = Take j

-- | /O(n) Drop a fixed number of items from the beginning of the list.

drop :: Int -> List a -> List a
drop i = Drop i

-- | /O(n) Compute the length of the list.

length :: List a -> Int
length (FromAsc       xs   ) = S.length xs
length (FromDesc      xs   ) = S.length xs
length (FromAscOrDesc xs _ ) = S.length xs
length (Merge         xs ys) = length xs + length ys
length (Take        j xs   ) = max 0 (min j (length xs))
length (Drop        j xs   ) = max 0 (length xs - j)

atLeast :: Int -> List a -> Maybe Int
atLeast n (FromAsc       xs   ) = smartLength n xs
atLeast n (FromDesc      xs   ) = smartLength n xs
atLeast n (FromAscOrDesc xs _ ) = smartLength n xs
atLeast n (Merge         xs ys) = do l <- atLeast n xs
                                     j <- atLeast (n - l) ys
                                     return (l + j)
atLeast n (Take        j xs   ) = if n < j
                                  then atLeast n xs
                                  else atLeast j xs <|> Just j
atLeast n (Drop        j xs   ) = (\m -> max 0 (m - j)) <$> atLeast (j + n) xs

-- | /O(1) Is the list empty?

null :: List a -> Bool
null = isJust . atLeast 0

-------------------------------------------------------------------------------

-- | Create an ordered list form a Haskell Sequence. No assumption is made about
-- the order of the items of the input list, it will be sorted before it is
-- converted. When you know in advance the input list is in a certain order use
-- the 'fromAscSeq' or 'fromDescSeq'.

fromSeq :: Ord a => Seq a -> List a
fromSeq xs = FromAscOrDesc
  (S.unstableSort                  xs)
  (S.unstableSortBy (flip compare) xs)

fromAscSeq :: Seq a -> List a
fromAscSeq = FromAsc

fromDescSeq :: Seq a -> List a
fromDescSeq = FromDesc

fromAscOrDescSeq :: Seq a -> Seq a -> List a
fromAscOrDescSeq = FromAscOrDesc

fromSeqs :: Ord a => [Seq a] -> List a
fromSeqs = fromSeq . mconcat

fromList :: Ord a => [a] -> List a
fromList = fromSeq . S.fromList

fromAscList :: [a] -> List a
fromAscList = fromAscSeq . S.fromList

fromDescList :: [a] -> List a
fromDescList = fromDescSeq . S.fromList

fromAscOrDescList :: [a] -> [a] -> List a
fromAscOrDescList xs ys = fromAscOrDescSeq (S.fromList xs) (S.fromList ys)

fromLists :: Ord a => [[a]] -> List a
fromLists = fromSeqs . map S.fromList

fromMap :: M.Map k a -> List a
fromMap m = fromAscOrDescList (map snd (M.toAscList m)) (map snd (M.toDescList m))

fromMapRange :: Ord k => Maybe k -> Maybe k -> M.Map k a -> List a
fromMapRange a b = fromMap . mapRange a b

-------------------------------------------------------------------------------

-- | Observe the ordered list as a Haskell Sequence. The Sequence can be
-- constructed with all elements in ascending order, in descending order, or
-- without any specific ordering.

toSeq :: Ord a => Maybe Direction -> List a -> Seq a
toSeq Nothing     = toUnorderedSeq
toSeq (Just Desc) = toDescSeq
toSeq (Just Asc)  = toAscSeq

toUnorderedSeq :: List a -> Seq a
toUnorderedSeq (FromAsc       xs   ) = xs
toUnorderedSeq (FromDesc      xs   ) = xs
toUnorderedSeq (FromAscOrDesc xs _ ) = xs
toUnorderedSeq (Merge         xs ys) = toUnorderedSeq xs `mappend` toUnorderedSeq ys
toUnorderedSeq (Take        j xs   ) = S.take j (toUnorderedSeq xs)
toUnorderedSeq (Drop        j xs   ) = S.drop j (toUnorderedSeq xs)

toAscSeq :: Ord a => List a -> Seq a
toAscSeq (FromAsc       xs   ) = xs
toAscSeq (FromDesc      xs   ) = S.reverse xs
toAscSeq (FromAscOrDesc xs _ ) = xs
toAscSeq (Merge         xs ys) = mergeBy (>) (toAscSeq xs) (toAscSeq ys)
toAscSeq (Take        j xs   ) = S.take j (toAscSeq xs)
toAscSeq (Drop        j xs   ) = S.drop j (toAscSeq xs)

toDescSeq :: Ord a => List a -> Seq a
toDescSeq (FromAsc       xs   ) = S.reverse xs
toDescSeq (FromDesc      xs   ) = xs
toDescSeq (FromAscOrDesc _  ys) = ys
toDescSeq (Merge         xs ys) = mergeBy (<) (toDescSeq xs) (toDescSeq ys)
toDescSeq (Take        j xs   ) = S.take j (toDescSeq xs)
toDescSeq (Drop        j xs   ) = S.drop j (toDescSeq xs)

toList :: Ord a => Maybe Direction -> List a -> [a]
toList a = F.toList . toSeq a

toUnorderedList :: List a -> [a]
toUnorderedList = F.toList . toUnorderedSeq

toAscList :: Ord a => List a -> [a]
toAscList = F.toList . toAscSeq

toDescList :: Ord a => List a -> [a]
toDescList = F.toList . toDescSeq

-------------------------------------------------------------------------------

-- Helper functions.

mergeBy :: (a -> a -> Bool) -> Seq a -> Seq a -> Seq a
mergeBy f = m
  where m as bs = case viewl as of
                    EmptyL    -> bs
                    (x :< xs) -> let (a, b) = breakl (`f` x) bs
                                 in mappend a (x <| m b xs)

localNubBy :: (a -> a -> Bool) -> Seq a -> Seq a
localNubBy f = n
  where n zs = case viewl zs of
                 EmptyL  -> zs
                 x :< xs ->
                   case viewl xs of
                     EmptyL              ->        zs
                     y :< _  | x `f` y   ->      n xs
                             | otherwise -> x <| n xs

mapRange :: Ord k => Maybe k -> Maybe k -> M.Map k v -> M.Map k v
mapRange from to m0 =
  let (e0, m1) = case from of Nothing -> (Nothing, m0)
                              Just f  -> let (_, m, o) = M.splitLookup f m0 in (m, o)
      (e1, m2) = case to   of Nothing -> (Nothing, m1)
                              Just f  -> let (o, m, _) = M.splitLookup f m1 in (m, o)
  in case (M.insert <$> from <*> e0, M.insert <$> to <*> e1) of
       (Just f, Just g) -> f (g m2)
       (Just f, _     ) -> f m2
       (_     , Just g) -> g m2
       (_     , _     ) -> m2

-- | Lazily compute the length of a list, never count any further than the
-- input bound. A result of Right means the length result is exact, a Left
-- result means at least that many items are contained.

smartLength :: Num i => i -> Seq a -> Maybe i
smartLength a b = f a b
  where f n zs = case (n, viewl zs) of
                   (_, EmptyL ) -> Just 0
                   (0, _      ) -> Nothing
                   (m, _ :< xs) -> fmap (1 +) (f (m - 1) xs)

-------------------------------------------------------------------------------

newtype ListT m a = ListT { runListT :: m (List a) }

instance Monad m => Monad (ListT m) where
  return a = ListT (return (singleton a))
  m >>= k  = ListT $
    do a <- runListT m
       merges `liftM` mapM (runListT . k) (toUnorderedList a)

instance MonadTrans ListT where
   lift m = ListT (singleton `liftM` m)

instance MonadIO m => MonadIO (ListT m) where
   liftIO = lift . liftIO

instance Monad m => MonadPlus (ListT m) where
  mzero       = ListT (return empty)
  m `mplus` n = ListT (liftM2 merge (runListT m) (runListT n))

instance Monad m => Functor (ListT m) where
  fmap = liftM

instance Monad m => Applicative (ListT m) where
  pure  = return
  (<*>) = ap

instance Monad m => Alternative (ListT m) where
  empty = mzero
  (<|>) = mplus

instance Monad List where
  return = singleton
  (>>=)  = flip mergeMap

instance MonadPlus List where
  mzero = empty
  mplus = merge

instance Functor List where
  fmap = liftM

instance Applicative List where
  pure  = return
  (<*>) = ap

instance Alternative List where
  empty = mzero
  (<|>) = mplus

