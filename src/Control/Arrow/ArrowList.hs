{-# LANGUAGE TypeOperators, Arrows #-}
{- |
The `ArrowList' type class, and a collection of list arrow related functions.
-}
module Control.Arrow.ArrowList
(
  -- * ArrowList type class.
  ArrowList (..)

  -- * Creating list arrows.
, unlistA
, listA
, none
, concatA
, maybeL

  -- * Collecting the results.
, collect
, empty

  -- * Conditional and filter arrows.
, isA
, ifA
, when
, guards
, filterA
, notA
, orElse

  -- * Optionality.
, optional
)
where

import Control.Monad hiding (when)
import Control.Category
import Control.Arrow
import Prelude hiding ((.), id)

-- | The `ArrowList' class represents two possible actions:
-- 
--   1. Lifting functions from one value to a list of values into a list arrow.
-- 
--   2. Mapping a function over the result list of a list arrow.

class Arrow (~>) => ArrowList (~>) where
  arrL :: (a -> [b]) -> a ~> b
  mapL :: ([b] -> [c]) -> (a ~> b) -> (a ~> c)

-- | Create a list arrow of an input list.

unlistA :: ArrowList (~>) => [b] ~> b
unlistA = arrL id

-- | Take the output of an arrow producing two results and concatenate them
-- into the result of the list arrow.

listA :: ArrowList (~>) => (a ~> (b, b)) -> a ~> b
listA = mapL (concatMap (\(a, b) -> [a, b]))

-- | Ignore the input and produce no results. Like `zeroArrow'.

none :: ArrowList (~>) => a ~> b
none = arrL (const [])

-- | Collect the results of applying multiple arrows to the same input.

concatA :: ArrowPlus (~>) => [a ~> b] -> a ~> b
concatA = foldr (<+>) zeroArrow

-- | Map a `Maybe' input to a list output. When the Maybe is a `Nothing' an
-- empty list will be returned, `Just' will result in a singleton list.

maybeL :: ArrowList (~>) => Maybe a ~> a
maybeL = arrL (maybe [] return)

-- | Collect the entire results of an list arrow as a singleton value in the
-- result list.

collect :: ArrowList (~>) => (a ~> b) -> a ~> [b]
collect = mapL return

-- | Returns a `Bool' indicating whether the input arrow produce any results.

empty :: ArrowList (~>) => (a ~> b) -> a ~> Bool
empty = mapL (\xs -> [if null xs then True else False])

-- | Create a filtering list arrow by mapping a predicate function over the
-- input. When the predicate returns `True' the input will be returned in the
-- output list, when `False' the empty list is returned.

isA :: ArrowList (~>) => (a -> Bool) -> a ~> a
isA f = arrL (\a -> if f a then [a] else [])

-- | Use the result a list arrow as a conditional, like an if-then-else arrow.
-- When the first arrow produces any results the /then/ arrow will be used,
-- when the first arrow produces no results the /else/ arrow will be used.

ifA :: (ArrowList (~>), ArrowChoice (~>))
    => (a ~> c)  -- ^ Arrow used as condition.
    -> (a ~> b)  -- ^ Arrow to use when condition has results.
    -> (a ~> b)  -- ^ Arrow to use when condition has no results.
    -> a ~> b
ifA c t e = proc i -> do x <- empty c -< i; if x then e -< i else t -< i

-- | Apply a list arrow only when a conditional arrow produces any results.
-- When the conditional produces no results the output arrow /behaves like the identity/.
-- The /second/ input arrow is used as the conditional, this allow
-- you to write: @ a \`when\` c @

when :: (ArrowList (~>), ArrowChoice (~>))
     => (a ~> a)  -- ^ The arrow to apply,
     -> (a ~> b)  -- ^ when this conditional holds.
     -> a ~> a
when a c = ifA c a id

-- | Apply a list arrow only when a conditional arrow produces any results.
-- When the conditional produces no results the output arrow /produces no results/.
-- The /first/ input arrow is used as the conditional, this allow you
-- to write: @ c \`guards\` a @

infix 8 `guards`

guards :: (ArrowList (~>), ArrowChoice (~>))
       => (a ~> c)  -- ^ When this condition holds,
       -> (a ~> b)  -- ^ then apply this arrow.
       -> a ~> b
guards c a = ifA c a none




filterA :: (ArrowChoice (~>), ArrowList (~>)) => (a ~> c) -> a ~> a
filterA c = ifA c id none

notA :: (ArrowList (~>), ArrowChoice (~>)) => (a ~> b) -> a ~> a
notA c = ifA c none id




orElse :: (ArrowList (~>), ArrowChoice (~>)) => (a ~> b) -> (a ~> b) -> a ~> b
orElse a = ifA a a

optional :: (ArrowChoice (~>), ArrowList (~>)) => (a ~> b) -> a ~> Maybe b
optional a = ifA a (arr Just . a) (arr (const Nothing))

