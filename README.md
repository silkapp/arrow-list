List arrows for Haskell.
========================

This small Haskell library provides some type class, types and functions to
work with list arrows. List arrows represent computations that may return
multiple outputs. Making functions that return lists an instance of both the
`Category` and `Arrow` type class allow you to easily compose multiple
computations into one with standard building blocks.

This package provides:

  - A type class `ArrowList` for embedding functions that produce a list of
    outputs into _some_ list arrow.
 
  - A list of utility functions for working with list-arrows, these functions
    are based on the `ArrowList` type class so they are not tied one specific
    instance.

  - A concrete list arrow type that is implemented as a `Kleisli` arrow over
    the `ListT` list monad transformer. In short, you can both build pure list
    arrows and list arrows that produce tributary effects.

  - Not list arrow specific: A type class `ArrowKleisli` for embedding monadic
    computations into an arrow.

