module Test.Main where

import Prelude
import Data.Generic.Rep as G
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Ref (REF, newRef, modifyRef, readRef)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Function.Memoize (class Tabulate, memoize, memoize2, genericTabulate)
import Data.List ((:), length, singleton)
import Data.String (take, drop)
import Test.QuickCheck (quickCheck')
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.Assert (ASSERT, assert')

data Diff a = Add a | Remove a

instance showDiff :: Show a => Show (Diff a) where
  show (Add a) = "(Add " <> show a <> ")"
  show (Remove a) = "(Remove " <> show a <> ")"

data Ints
  = Int1 Int
  | Int2 Int

derive instance genericInts :: G.Generic Ints _

instance tabulateInts :: Tabulate Ints where
  tabulate = genericTabulate


newtype SmallInt = SmallInt Int

instance arbSmallInt :: Arbitrary SmallInt where
  arbitrary = SmallInt <<< (_ `mod` 1000) <$> arbitrary

main :: forall eff. Eff (assert :: ASSERT, ref :: REF, console :: CONSOLE, random :: RANDOM, exception :: EXCEPTION | eff) Unit
main = do
  let fibonacciFast = go 0 1
        where
        go n _ 0 = n
        go n m i = go m (n + m) (i - 1)

      -- Remove the call to memoize and this will be very slow
      fibonacci = memoize
        case _ of
          0 -> 0
          1 -> 1
          n -> fibonacci (n - 1) + fibonacci (n - 2)

      fibonacciInts = memoize
        case _ of
          Int1 0 -> 0
          Int2 0 -> 0
          Int1 1 -> 1
          Int2 1 -> 1
          Int1 n -> fibonacciInts (Int2 (n - 1)) + fibonacciInts (Int1 (n - 2))
          Int2 n -> fibonacciInts (Int1 (n - 1)) + fibonacciInts (Int2 (n - 2))
  logShow $ fibonacciFast 40
  logShow $ fibonacci 40
  logShow $ fibonacciInts (Int1 40)

  let smallest xs ys | length xs <= length ys = xs
                     | otherwise = ys

      -- Remove the call to memoize2 and this will be very slow
      diff = memoize2
        case _, _ of
          "", s2 -> singleton (Add s2)
          s1, "" -> singleton (Remove s1)
          s1, s2
            | take 1 s1 == take 1 s2 -> diff (drop 1 s1) (drop 1 s2)
            | otherwise -> smallest (Add (take 1 s2)    : diff s1 (drop 1 s2))
                                    (Remove (take 1 s1) : diff (drop 1 s1) s2)
  logShow $ diff "Hello, PureScript" "ello, PureScript!"

  called <- newRef 0
  let fn x = 2 * x
      msin = memoize \n -> unsafePerformEff do
        modifyRef called (_ + 1) 
        pure $ fn n
  quickCheck' 10000 $ \(SmallInt x) -> fn x == msin x
  ncalled <- readRef called
  assert' "Memoized function called too many times" (ncalled < 2000) 
