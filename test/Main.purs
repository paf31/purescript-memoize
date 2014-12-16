module Test.Main where

import Data.Lazy
import Data.Function.Memoized

import Control.Monad.Eff

import Debug.Trace

main = do
  let fib = force <<< memo (\n -> fib' n)

      fib' 0 = 0
      fib' 1 = 1
      fib' n = fib (n - 1) + fib (n - 2)

  print $ fib 100


