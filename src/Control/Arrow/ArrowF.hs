{-# LANGUAGE
    TypeOperators
  , Arrows
  , MultiParamTypeClasses
  , FunctionalDependencies
  #-}
module Control.Arrow.ArrowF
(
  -- * Container arrow type class.
  ArrowF (..)

  -- * Generic arrow utilities.
, unite
, constA
, concatA

  -- * Container arrow utilities.
, constF
, none
, results

  -- * Conditional and filter arrows.
, isA
, ifA
, when
, guards
, filterA
, notA
, orElse

  -- * Optionality.
, maybeL
, optional
)
where

import Control.Applicative hiding (optional)
import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)

-- | A type class for arrows that produce containers of results. The container
-- arrow can be seen as a generalization for list arrows. Most operations
-- assume the container type has an 'Applicative' and a 'Alternative' instance.

class (Eq (f ()), Alternative f, Arrow (~>)) => ArrowF f (~>) | (~>) -> f where
  arrF :: (a -> f b) -> a ~> b    -- ^ Use a container as the input for an arrow.
  unF  :: (a ~> b) -> (a ~> f b)  -- ^ Get the result as container.

-- | Take the output of an arrow producing two results and concatenate them
-- into the result of the container arrow.

unite :: ArrowPlus (~>) => (b, b) ~> b
unite = (arr fst <+> arr snd)

-- | Skip the input and produce a constant output.

constA :: Arrow (~>) => b -> a ~> b
constA = arr . const

-- | Collect the results of applying multiple arrows to the same input.

concatA :: ArrowPlus (~>) => [a ~> b] -> a ~> b
concatA = foldr (<+>) zeroArrow

-- | Skip the input and produce a constant output specified as a container.

constF :: ArrowF f (~>) => f b -> a ~> b
constF = arrF . const

-- | Ignore the input and produce no results. Like `zeroArrow'.

none :: ArrowF f (~>) => a ~> b
none = constF empty

-- | Returns a `Bool' indicating whether the input arrow produces a container
-- with any results.

results :: (Eq (f ()), ArrowF f (~>)) => (a ~> b) -> (a ~> Bool)
results a = arr ((/= empty) . fmap (const ())) . unF a

-- | Create a filtering container arrow by mapping a predicate function over the
-- input. When the predicate returns `True' the input will be returned in the
-- output container, when `False' the empty container is returned.

isA :: ArrowF f (~>) => (a -> Bool) -> a ~> a
isA f = arrF (\a -> if f a then pure a else empty)

-- | Use the result of a container arrow as a conditional, like an if-then-else
-- arrow. When the first arrow produces any results the /then/ arrow will be
-- used, when the first arrow produces no results the /else/ arrow will be
-- used.

ifA :: (ArrowF f (~>), ArrowChoice (~>)) => (a ~> b) -> (a ~> t) -> (a ~> t) -> a ~> t
ifA c t e = proc i -> do x <- results c -< i; if x then e -< i else t -< i

-- | Apply a container arrow only when a conditional arrow produces any
-- results.  When the conditional produces no results the output arrow /behaves
-- like the identity/. The /second/ input arrow is used as the conditional,
-- this allow you to write: @ a \`when\` c @

when :: (ArrowF f (~>), ArrowChoice (~>)) => (a ~> a) -> (a ~> c) -> a ~> a
when a c = ifA c a id

-- | Apply a container arrow only when a conditional arrow produces any
-- results.  When the conditional produces no results the output arrow
-- /produces no results/. The /first/ input arrow is used as the conditional,
-- this allow you to write: @ c \`guards\` a @

infix 8 `guards`

guards :: (ArrowChoice (~>), ArrowF f (~>)) => (a ~> c) -> (a ~> b) -> (a ~> b)
guards c a = ifA c a none

-- | Filter the results of an arrow with a predicate arrow, when the filter
-- condition produces results the input is accepted otherwise it is excluded.

filterA :: (ArrowF f (~>), ArrowChoice (~>)) => (a ~> c) -> a ~> a
filterA c = ifA c id none

-- | Negation container arrow. Only accept the input when the condition
-- produces no output.

notA :: (ArrowF f (~>), ArrowChoice (~>)) => (a ~> c) -> a ~> a
notA c = ifA c none id

-- | Apply the input arrow, when the arrow does not produces any results the
-- second fallback arrow is applied.
-- Likely written infix like this @ a \`orElse\` b @

orElse :: (ArrowF f (~>), ArrowChoice (~>)) => (a ~> b) -> (a ~> b) -> a ~> b
orElse a = ifA a a 

-- | Map a `Maybe' input to a container output. When the Maybe is a `Nothing'
-- an empty container will be returned, `Just' will result in a singleton
-- container.

maybeL :: ArrowF f (~>) => Maybe a ~> a
maybeL = arrF (maybe empty pure)

-- | Apply a container arrow, when there are no results a `Nothing' will be
-- returned, otherwise the results will be wrapped in a `Just'. This function
-- always produces result.

optional :: (ArrowF f (~>), ArrowChoice (~>)) => (a ~> b) -> a ~> Maybe b
optional a = ifA a (arr Just . a) (arr (const Nothing))

