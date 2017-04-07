## Module Data.Function.Memoize

This module defines functions for _memoizing_ functions, i.e. creating functions which
remember their results.

This module works by turning a function into a lazily-evaluated data structure depending on
its domain type.

#### `Tabulate`

``` purescript
class Tabulate a  where
  tabulate :: forall r. (a -> r) -> a -> Lazy r
```

The `Tabulate` class identifies those types which can be used as the domain of
a memoized function, i.e. those for which the results can be _tabulated_.

##### Instances
``` purescript
Tabulate Unit
Tabulate NoArguments
Tabulate Boolean
Tabulate Char
Tabulate String
(Tabulate a) => Tabulate (Constructor name a)
(Tabulate a) => Tabulate (Argument a)
(Tabulate a) => Tabulate (Maybe a)
(Tabulate a, Tabulate b) => Tabulate (Either a b)
(Tabulate a, Tabulate b) => Tabulate (Sum a b)
(Tabulate a, Tabulate b) => Tabulate (Tuple a b)
(Tabulate a, Tabulate b) => Tabulate (Product a b)
(Tabulate a) => Tabulate (List a)
(Tabulate a) => Tabulate (Array a)
Tabulate Int
```

#### `memoize`

``` purescript
memoize :: forall a b. Tabulate a => (a -> b) -> a -> b
```

Memoize a function of one argument

#### `memoize2`

``` purescript
memoize2 :: forall a b c. Tabulate a => Tabulate b => (a -> b -> c) -> a -> b -> c
```

Memoize a function of two arguments

#### `memoize3`

``` purescript
memoize3 :: forall a b c d. Tabulate a => Tabulate b => Tabulate c => (a -> b -> c -> d) -> a -> b -> c -> d
```

Memoize a function of three arguments

#### `genericTabulate`

``` purescript
genericTabulate :: forall a r rep. Generic a rep => Tabulate rep => (a -> r) -> a -> Lazy r
```

A default implementation of `Tabulate` for `Generic` types.

Given a data type made up of data types with `Tabulate` instances:

```purescript
data MyDataType
  = A Int
  | B String
```

First, derive an instance of `Data.Generics.Rep.Generic`:

```purescript
derive instance genericMyDataType :: Generic MyDataType _
```

Now, `Tabulate` can be defined in terms of `genericTabulate`:

```purescript
instance tabulateMyDataType :: Tabulate MyDataType where
  tabulate = genericTabulate
```

_Note_: this function should not be used to derive instances for recursive
data types, and attempting to do so will lead to stack overflow errors
at runtime.


