module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Function.Memoize (memoize)
import Data.Array ((..))
import Data.List (List(..), (:), fromFoldable, length)
import Data.Tuple (Tuple(..))

data Diff a = Add a | Remove a

instance showDiff :: Show a => Show (Diff a) where
  show (Add a) = "(Add " <> show a <> ")"
  show (Remove a) = "(Remove " <> show a <> ")"

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = do
  let fibonacciFast = go 0 1
        where
        go n _ 0 = n
        go n m i = go m (n + m) (i - 1)

      fibonacci = memoize
        case _ of
          0 -> 0
          1 -> 1
          n -> fibonacci (n - 1) + fibonacci (n - 2)
  logShow $ fibonacciFast 40
  logShow $ fibonacci 40

  let smallest xs ys | length xs <= length ys = xs
                     | otherwise = ys

      diff = memoize
        case _ of
          Tuple Nil xs -> map Add xs
          Tuple xs Nil -> map Remove xs
          Tuple (Cons x xs) (Cons y ys)
            | x == y -> diff (Tuple xs ys)
            | otherwise -> smallest (Add y    : diff (Tuple (Cons x xs) ys))
                                    (Remove x : diff (Tuple xs (Cons y ys)))
  logShow $ diff (Tuple (fromFoldable (1 .. 30))
                        (fromFoldable (2 .. 31)))
