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
, fromMap
, fromMapRange

-- * Observing as regular Haskell list.
, toList
, toUnorderedList
, toAscList
, toDescList

, ListT (..)

-- * Internally used helper functions.
, mergeBy
, localNubBy
, mapRange
)
where

import Control.Applicative hiding (empty)
import Control.Monad.Identity
import Data.List
import Data.Ord
import qualified Control.Applicative
import qualified Data.Map as M

-------------------------------------------------------------------------------

data List a =
    FromAsc       [a]
  | FromDesc      [a]
  | FromAscOrDesc [a] [a]
  | Merge         (List a) (List a)

-- | A sorting direction, either ascending or descending.
data Direction = Asc | Desc
  deriving (Eq, Ord, Show)

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

-- | /O(1)/ Create an ordered list form a Haskell list. No assumption is made
-- about the order of the items of the input list, it will be sorted before it
-- is converted. When you know in advance the input list is in a certain order
-- use the 'fromAscList' or 'fromDescList'.

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

fromMap :: M.Map k a -> List a
fromMap m = FromAscOrDesc (map snd (M.toAscList m)) (map snd (M.toDescList m))

fromMapRange :: Ord k => Maybe k -> Maybe k -> M.Map k a -> List a
fromMapRange a b = fromMap . mapRange a b

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

