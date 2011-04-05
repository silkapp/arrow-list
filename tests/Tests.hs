{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Applicative
import Data.Monoid
import Data.Maybe
import Data.List
import Data.List.Ordered hiding (null, length, take, drop, filter, empty)
import Test.QuickCheck
import System.Random
import qualified Data.List.Ordered as O
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Foldable as F

import System.IO.Unsafe
import Debug.Trace

newtype Val = Val { unVal :: Int }
  deriving ( Random
           , Num
           , Real
           , Enum
           , Integral
           , Show
           , Eq
           , Ord
           )

instance Arbitrary Val where
  arbitrary = choose (0, 1000)

newtype Slice = Slice { unSlice :: Int }
  deriving ( Random
           , Num
           , Real
           , Enum
           , Integral
           , Show
           , Eq
           , Ord
           )

instance Arbitrary Slice where
  arbitrary = choose (0, 4)

sortR :: Ord a => [a] -> [a]
sortR = sortBy (flip compare)

instance (Ord a, Arbitrary a) => Arbitrary (List a) where
  arbitrary = frequency [ (2, (fromAscList . sort) `fmap` arbitrary)
                        , (2, (fromDescList . sortR) `fmap` arbitrary)
                        , (2, fromList `fmap` arbitrary)
                        , (3, merge <$> arbitrary <*> arbitrary)
                        , (1, O.take <$> (unSlice <$> arbitrary) <*> arbitrary)
                        , (1, O.drop <$> (unSlice <$> arbitrary) <*> arbitrary)
                        ]

-------------------------------------------------------------------------------

check :: (Show a, Ord a) => List a -> [a] -> (List a -> List a) -> ([a] -> [a]) -> Bool
check a b f g = and
  [                    (f a) == f (fromList              b)
  , toList (Just Asc)  (f a) == g (sort                  b)
  , toList (Just Desc) (f a) == g (sortBy (flip compare) b)
  , O.null             (f a) == null                  (g b)
  , O.length           (f a) == length                (g b)
  , show               (f a) == show                  (g (sort b))
  , sort (F.toList a)        == sort b
  ]

observe :: (Show a, Ord a) => List a -> [a] -> Bool
observe a b =
  let n = length b
      h = n `div` 2
      m = n * 2
  in and [ check a b id id
         , check a b (O.drop 0) (drop 0)
         , check a b (O.drop 1) (drop 1)
         , check a b (O.drop h) (drop h)
         , check a b (O.drop n) (drop n)
         , check a b (O.drop m) (drop m)
         , check a b (O.take 0) (take 0)
         , check a b (O.take 1) (take 1)
         , check a b (O.take h) (take h)
         , check a b (O.take n) (take n)
         , check a b (O.take m) (take m)
--          , check a b (O.mapMonotonic (*2)) (map (*2))
         ]

fromListT :: [Val] -> Bool
fromListT a = observe (fromList a) a

fromAscListT :: [Val] -> Bool
fromAscListT a = observe (fromAscList (sort a)) a

fromDescListT :: [Val] -> Bool
fromDescListT a = observe (fromDescList (sortBy (flip compare) a)) a

fromAscOrDescListT :: [Val] -> Bool
fromAscOrDescListT a = observe (fromAscOrDescList (sort a) (sortBy (flip compare) a)) a

fromListsT :: [[Val]] -> Bool
fromListsT a = observe (fromLists a) (concat a)

mergesT :: [[Val]] -> Bool
mergesT a = observe (merges (map fromList a)) (concat a)

addEmptyT :: [Val] -> Bool
addEmptyT a = observe (foldr add O.empty a) a

mappendMemptyT :: [Val] -> Bool
mappendMemptyT a = observe (foldr mappend mempty (map singleton a)) a

fromMapT :: [Val] -> Bool
fromMapT keys =
  let m = M.fromList (zip keys (map (*2) keys))
   in observe (fromMap m) (M.elems m)

fromMapRangeT :: Val -> [Val] -> Val -> Val -> Val -> Bool -> Bool -> Bool
fromMapRangeT key keys mid ii jj x y = 
  let i     = mid - ii
      j     = mid + jj
      m     = M.fromList (zip (key:keys) (map (*2) (key:keys)))
      vs    = M.elems m
      ks    = M.keys m
      mink  = head (sort ks)
      maxk  = last (sort ks)
      delta = maxk - mink
      keyA  = mink + (delta * i) `div` 1000
      keyB  = mink + (delta * j) `div` 1000
      olist = fromMapRange (if x then Just keyA else Nothing) (if y then Just keyB else Nothing) m
      list  = (if y then takeWhile (<= keyB * 2) else id)
            . (if x then dropWhile (<  keyA * 2) else id)
            $ vs
  in observe olist list

localNubByT :: [Val] -> Bool
localNubByT a = (localNubBy (==) . toAscSeq . fromList) a == S.fromList (nub (sort a))

filterT :: List Val -> Bool
filterT a =
  let x = toAscList  (O.filter even a)
      y = toAscList  (O.filter (not . even) a)
      u = toDescList (O.filter even a)
      v = toDescList (O.filter (not . even) a)
  in and [ filter even         x == x
         , filter (not . even) x == []
         , filter even         y == []
         , filter (not . even) y == y
         , filter even         u == u
         , filter (not . even) u == []
         , filter even         v == []
         , filter (not . even) v == v
         ]

mapMonotonicT :: List Val -> Bool
mapMonotonicT a = filterT (mapMonotonic (+10) a)

bindT :: [Val] -> [Val] -> Bool
bindT a b = and
  [ observe olist                               list
  , observe (unsafePerformIO (runListT olistT)) list
  ]
  where olist =
          do let c = fromList a
             let d = fromList b
             x <- (,) <$> c <*> c
             y <- (*2) `fmap` d
             z <- empty <|> c <|> d <|> pure 4
             return (x, y, z)
        olistT =
          do let c = ListT (return (fromList a))
             let d = ListT (return (fromList b))
             x <- (,) <$> c <*> c
             y <- (*2) `fmap` d
             z <- empty <|> c <|> d <|> liftIO (putStr "" >> return 4)
             pure (x, y, z)
        list =
          do let c = a
             let d = b
             x <- (,) <$> c <*> c
             y <- (*2) `fmap` d
             z <- empty <|> c <|> d <|> pure 4
             return (x, y, z)

-------------------------------------------------------------------------------

main :: IO ()
main =
  do putStrLn "fromListT: ";             quickCheck fromListT
     putStrLn "fromAscListT: ";          quickCheck fromAscListT
     putStrLn "fromDescListT: ";         quickCheck fromDescListT
     putStrLn "fromAscOrDescListT: ";    quickCheck fromAscOrDescListT
     putStrLn "fromListsT: ";            quickCheck fromListsT
     putStrLn "mergesT: ";               quickCheckWith stdArgs { maxSize = 10 } bindT
     putStrLn "addEmptyT: ";             quickCheck addEmptyT
     putStrLn "mappendMemptyT: ";        quickCheck mappendMemptyT
     putStrLn "fromMapT: ";              quickCheck fromMapT
     putStrLn "localNubByT: ";           quickCheck localNubByT
     putStrLn "filterT:";                quickCheck filterT
     putStrLn "filterT:";                quickCheck mapMonotonicT
     putStrLn "fromMapRangeT:";          quickCheck fromMapRangeT
     putStrLn "bindT:";                  quickCheckWith stdArgs { maxSize = 7 } bindT

