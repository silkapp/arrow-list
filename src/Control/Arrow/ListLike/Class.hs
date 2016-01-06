{-# LANGUAGE
    Arrows
  , FunctionalDependencies
  , MultiParamTypeClasses
  , TypeOperators
  #-}
module Control.Arrow.ListLike.Class
(
  -- * Container arrow type class.
  ArrowListLike (..)
, mapF
, arrMF

  -- * Generic arrow utilities.
, unite
, const
, concatA
, plus

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
, maybeA
, optional
)
where

import Prelude hiding (const, id, (.))

import Control.Applicative hiding (optional)
import Control.Arrow
import Control.Arrow.Kleisli.Class
import Control.Category
import qualified Data.Foldable as F
import qualified Prelude

-- | A type class for arrows that produce containers of results. The container
-- arrow can be seen as a generalization for list arrows. Most operations
-- assume the container type has an 'Applicative', an 'Alternative' and a
-- 'F.Foldable' instance.

class Arrow arr => ArrowListLike f arr | arr -> f where
  embed   :: f a `arr` a                 -- ^ Use a container as the input for an arrow.
  observe :: (a `arr` b) -> a `arr` f b  -- ^ Get the result as container.

-- | Embed a monadic function returning an ordered list into a container arrow.

arrMF :: (ArrowListLike f arr, ArrowKleisli m arr) => (a -> m (f c)) -> a `arr` c
arrMF x = embed . arrM x

-- | Map a function over the result collection of a container arrow.

mapF :: ArrowListLike f arr => (f b -> f c) -> a `arr` b -> a `arr` c
mapF f a = embed . arr f . observe a

-- | Take the output of an arrow producing two results and concatenate them
-- into the result of the container arrow.

unite :: ArrowPlus arr => (b, b) `arr` b
unite = arr fst <+> arr snd

-- | Skip the input and produce a constant output.

const :: Arrow arr => b -> a `arr` b
const = arr . Prelude.const

-- | Collect the results of applying multiple arrows to the same input.

concatA :: ArrowPlus arr => [a `arr` b] -> a `arr` b
concatA = foldr (<+>) zeroArrow

-- | Join the results of two arrows, like (<+>) from ArrowPlus.

plus :: (Alternative f, ArrowListLike f arr) => (a `arr` b) -> (a `arr` b) -> a `arr` b
plus a b = embed . arr (\(x, y) -> x <|> y) . (observe a &&& observe b)

-- | Skip the input and produce a constant output specified as a container.

constF :: ArrowListLike f arr => f c -> a `arr` c
constF f = embed . const f

-- | Ignore the input and produce no results. Like `zeroArrow'.

none :: (Alternative f, ArrowListLike f arr) => a `arr` b
none = constF empty

-- | Returns a `Bool' indicating whether the input arrow produces a container
-- with any results.

results :: (F.Foldable f, ArrowListLike f arr) => (a `arr` b) -> (a `arr` Bool)
results a = arr (not . null . F.toList) . observe a

-- | Create a filtering container arrow by mapping a predicate function over the
-- input. When the predicate returns `True' the input will be returned in the
-- output container, when `False' the empty container is returned.

isA :: (Alternative f, ArrowListLike f arr) => (a -> Bool) -> a `arr` a
isA f = embed . arr (\a -> if f a then pure a else empty)

-- | Use the result of a container arrow as a conditional, like an if-then-else
-- arrow. When the first arrow produces any results the /then/ arrow will be
-- used, when the first arrow produces no results the /else/ arrow will be
-- used.

ifA :: (F.Foldable f, ArrowListLike f arr, ArrowChoice arr) => (a `arr` b) -> (a `arr` t) -> (a `arr` t) -> a `arr` t
ifA c t e = proc i -> do x <- results c -< i; if x then t -< i else e -< i

-- | Apply a container arrow only when a conditional arrow produces any
-- results.  When the conditional produces no results the output arrow /behaves
-- like the identity/. The /second/ input arrow is used as the conditional,
-- this allow you to write: @ a \`when\` condition @

infix 7 `when`

when :: (F.Foldable f, ArrowListLike f arr, ArrowChoice arr) => (a `arr` a) -> (a `arr` c) -> a `arr` a
when a c = ifA c a id

-- | Apply a container arrow only when a conditional arrow produces any
-- results.  When the conditional produces no results the output arrow
-- /produces no results/. The /first/ input arrow is used as the conditional,
-- this allow you to write: @ condition \`guards\` a @

infix 8 `guards`

guards :: (Alternative f, F.Foldable f, ArrowListLike f arr, ArrowChoice arr) => (a `arr` c) -> (a `arr` b) -> (a `arr` b)
guards c a = ifA c a none

-- | Filter the results of an arrow with a predicate arrow, when the filter
-- condition produces results the input is accepted otherwise it is excluded.

filterA :: (Alternative f, F.Foldable f, ArrowListLike f arr, ArrowChoice arr) => (a `arr` c) -> a `arr` a
filterA c = ifA c id none

-- | Negation container arrow. Only accept the input when the condition
-- produces no output.

notA :: (Alternative f, F.Foldable f, ArrowListLike f arr, ArrowChoice arr) => (a `arr` c) -> a `arr` a
notA c = ifA c none id

-- | Apply the input arrow, when the arrow does not produces any results the
-- second fallback arrow is applied.
-- Likely written infix like this @ a \`orElse\` b @

infix 6 `orElse`

orElse :: (F.Foldable f, ArrowListLike f arr, ArrowChoice arr) => (a `arr` b) -> (a `arr` b) -> a `arr` b
orElse a = ifA a a

-- | Map a `Maybe' input to a container output. When the Maybe is a `Nothing'
-- an empty container will be returned, `Just' will result in a singleton
-- container.

maybeA :: (Alternative f, ArrowListLike f arr) => Maybe a `arr` a
maybeA = embed . arr (maybe empty pure)

-- | Apply a container arrow, when there are no results a `Nothing' will be
-- returned, otherwise the results will be wrapped in a `Just'. This function
-- always produces result.

optional :: (F.Foldable f, ArrowListLike f arr, ArrowChoice arr) => (a `arr` b) -> a `arr` Maybe b
optional a = ifA a (arr Just . a) (arr (const Nothing))
