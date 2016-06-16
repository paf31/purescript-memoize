## Module Data.Function.Memoize

This module defines functions for _memoizing_ functions, i.e. creating functions which
remember their results.

This module works by turning a function into a lazily-evaluated data structure depending on
its domain type.

#### `Tabulate`

``` purescript
class Tabulate a where
  tabulate :: forall r. (a -> r) -> a -> Lazy r
```

The `Tabulate` class identifies those types which can be used as the domain of
a memoized function, i.e. those for which the results can be _tabulated_.

##### Instances
``` purescript
Tabulate Unit
Tabulate Boolean
(Tabulate a) => Tabulate (Maybe a)
(Tabulate a, Tabulate b) => Tabulate (Either a b)
(Tabulate a, Tabulate b) => Tabulate (Tuple a b)
(Tabulate a) => Tabulate (List a)
Tabulate Int
```

#### `Memoize`

``` purescript
class Memoize a where
  memoize :: a -> a
```

The `Memoize` class identifies those function types which can be memoized.

If the domain type can be tabulated, then functions can be memoized.

##### Instances
``` purescript
(Tabulate a) => Memoize (a -> r)
```


