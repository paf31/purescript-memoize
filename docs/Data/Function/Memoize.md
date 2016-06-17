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
Tabulate Char
Tabulate String
(Tabulate a) => Tabulate (Maybe a)
(Tabulate a, Tabulate b) => Tabulate (Either a b)
(Tabulate a, Tabulate b) => Tabulate (Tuple a b)
(Tabulate a) => Tabulate (List a)
(Tabulate a) => Tabulate (Array a)
Tabulate Int
(Partial) => Tabulate GenericSpine
```

#### `memoize`

``` purescript
memoize :: forall a b. Tabulate a => (a -> b) -> a -> b
```

Memoize a function of one argument

#### `memoize2`

``` purescript
memoize2 :: forall a b c. (Tabulate a, Tabulate b) => (a -> b -> c) -> a -> b -> c
```

Memoize a function of two arguments

#### `memoize3`

``` purescript
memoize3 :: forall a b c d. (Tabulate a, Tabulate b, Tabulate c) => (a -> b -> c -> d) -> a -> b -> c -> d
```

Memoize a function of three arguments

#### `gTabulate`

``` purescript
gTabulate :: forall a r. Partial => Generic a => (a -> r) -> a -> Lazy r
```

A default implementation of `Tabulate` for `Generic` types.

This function is marked as `Partial`, since it is not implemented
for the `Number` type, or types containing `Number`. However, all other
`Generic` types are supported.


