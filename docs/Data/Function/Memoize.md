## Module Data.Function.Memoize

This module defines functions for _memoizing_ functions, i.e. creating functions which
remember their results.

This module works by turning a function into a lazily-evaluated data structure depending on
its codomain type.

#### `Tabulate`

``` purescript
class Tabulate a where
  tabulate :: forall r. (a -> r) -> a -> Lazy r
```

The `Tabulate` class identifies those types which can be used as the codomain of
a memoized function, i.e. those for which the results can be _tabulated_.

##### Instances
``` purescript
instance tabulateUnit :: Tabulate Unit
instance tabulateBool :: Tabulate Boolean
instance tabulateMaybe :: (Tabulate a) => Tabulate (Maybe a)
instance tabulateEither :: (Tabulate a, Tabulate b) => Tabulate (Either a b)
instance tabulateTuple :: (Tabulate a, Tabulate b) => Tabulate (Tuple a b)
instance tabulateNat :: Tabulate Int
```

#### `Memoize`

``` purescript
class Memoize a where
  memoize :: a -> a
```

The `Memoize` class identifies those function types which can be memoized.

If the codomain type can be tabulated, then functions can be memoized.

##### Instances
``` purescript
instance tabulateFunction :: (Tabulate a) => Memoize (a -> r)
```


