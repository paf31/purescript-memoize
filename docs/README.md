# Module Documentation

## Module Data.Function.Memoized

### Types


### Type Classes

    class Memoize a where
      memoize :: a -> a

    class Tabulate a where
      tabulate :: forall r. (a -> r) -> a -> Lazy r


### Type Class Instances

    instance tabulateBool :: Tabulate Boolean

    instance tabulateEither :: (Tabulate a, Tabulate b) => Tabulate (Either a b)

    instance tabulateFunction :: (Tabulate a) => Memoize (a -> r)

    instance tabulateMaybe :: (Tabulate a) => Tabulate (Maybe a)

    instance tabulateNat :: Tabulate Number

    instance tabulateTuple :: (Tabulate a, Tabulate b) => Tabulate (Tuple a b)

    instance tabulateUnit :: Tabulate Unit



