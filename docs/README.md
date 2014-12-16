# Module Documentation

## Module Data.Function.Memoized

### Types


### Type Classes

    class Memo a where
      memo :: forall r. (a -> r) -> a -> Lazy r


### Type Class Instances

    instance memoBool :: Memo Boolean

    instance memoEither :: (Memo a, Memo b) => Memo (Either a b)

    instance memoNat :: Memo Number

    instance memoTuple :: (Memo a, Memo b) => Memo (Tuple a b)

    instance memoUnit :: Memo Unit



