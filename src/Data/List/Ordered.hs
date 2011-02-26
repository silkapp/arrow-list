{- |
Ordered list type with efficient and streaming observation in two directions.
-}
module Data.List.Ordered
( 
-- * Ordered list type.
  List
, Direction (..)

-- * Constructing ordered lists.
, singleton
, add
, merge
, mergeMap

, fromList
, fromAscList
, fromDescList
, fromAscOrDescList

-- * Observing as regular Haskell list.
, toList
, toUnorderedList
, toAscList
, toDescList

, ListT (..)

-- * Internally used helper functions.
, mergeBy
, localNubBy
)
where

import Control.Applicative hiding (empty)
import Control.Monad.Identity
import Data.List
import Data.Ord
import qualified Control.Applicative

-------------------------------------------------------------------------------

data List a =
    FromAsc       [a]
  | FromDesc      [a]
  | FromAscOrDesc [a] [a]
  | Merge         (List a) (List a)

-- | A sorting direction, either ascending or descending.
data Direction = Asc | Desc

-------------------------------------------------------------------------------

empty :: List a
empty = FromAsc []

-- | /O(1)/ Create a singleton ordered list.

singleton :: a -> List a
singleton x = FromAsc [x]

-- | /O(1)/ Add a single element to an ordered list.

add :: a -> List a -> List a
add x = Merge (FromAsc [x])

-- | /O(1)/ Merge two ordered lists.

merge :: List a -> List a -> List a
merge = Merge

fromList :: Ord a => [a] -> List a
fromList xs = FromAscOrDesc
  (sort xs)
  (sortBy (flip (comparing id)) xs)

fromAscList :: [a] -> List a
fromAscList xs = FromAsc xs

fromDescList :: [a] -> List a
fromDescList xs = FromDesc xs

fromAscOrDescList :: [a] -> [a] -> List a
fromAscOrDescList = FromAscOrDesc

mergeMap :: (a -> List b) -> List a -> List b
mergeMap f = foldl1 merge . map f . toUnorderedList

-------------------------------------------------------------------------------

-- | Observe the ordered list as a regular list. The list can be constructed
-- with all elements in ascending order, in descending order, or without any
-- specific ordering.

toList :: Ord a => Maybe Direction -> List a -> [a]
toList Nothing     = toUnorderedList
toList (Just Desc) = toDescList
toList (Just Asc)  = toAscList

toUnorderedList :: List a -> [a]
toUnorderedList (FromAsc       xs   ) = xs
toUnorderedList (FromDesc      xs   ) = xs
toUnorderedList (FromAscOrDesc xs _ ) = xs
toUnorderedList (Merge         xs ys) = toUnorderedList xs ++ toUnorderedList ys

toAscList :: Ord a => List a -> [a]
toAscList (FromAsc       xs   ) = xs
toAscList (FromDesc      xs   ) = reverse xs
toAscList (FromAscOrDesc xs _ ) = xs
toAscList (Merge         xs ys) = mergeBy (>) (toAscList xs) (toAscList ys)

toDescList :: Ord a => List a -> [a]
toDescList (FromAsc       xs   ) = reverse xs
toDescList (FromDesc      xs   ) = xs
toDescList (FromAscOrDesc _  ys) = ys
toDescList (Merge         xs ys) = mergeBy (<) (toDescList xs) (toDescList ys)

-------------------------------------------------------------------------------

-- Helper functions.

mergeBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
mergeBy f = m
  where m []     ys = ys
        m (x:xs) ys = let (a, b) = break (`f` x) ys
                      in a ++ x : m b xs

localNubBy :: (a -> a -> Bool) -> [a] -> [a]
localNubBy f = n
  where n (x:y:xs) | x `f` y   =     n (y:xs)
                   | otherwise = x : n (y:xs)
        n xs                   = xs

-------------------------------------------------------------------------------

newtype ListT m a = ListT { runListT :: m (List a) }

instance Monad m => Monad (ListT m) where
  return a = ListT (return (singleton a))
  m >>= k  = ListT $
    do a <- runListT m
       b <- mapM (runListT . k) (toUnorderedList a)
       return (foldl1 merge b)

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

-------------------------------------------------------------------------------

{-
myExampleList :: List Integer
myExampleList = fromAscOrDescList [10, 20, 30] [30, 20, 10]
        `merge` fromAscList       [4, 50, 600]
        `merge` fromAscList       [3, 41, 501]
        `merge` fromDescList      [300, 200, 100, 50, 25, 10, 5, 2, 1]
        `merge` fromAscList       [3, 40, 500]
        `merge` fromDescList      [3000, 2000, 1000, 500, 250, 100, 50, 20, 10]

testerdetest :: [Integer]
testerdetest = toAscList (myExampleList >>= (\a -> singleton a `merge` singleton (a * 100)))
-}

