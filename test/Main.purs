module Test.Main where

import Data.Lazy
import Data.Function.Memoized

import Control.Monad.Eff

import Debug.Trace

main = do
  let fibonacciSlow 0 = 0
      fibonacciSlow 1 = 1
      fibonacciSlow n = fibonacci (n - 1) + 
                        fibonacci (n - 2)
      
      fibonacci = memoize $ \n -> fibonacciSlow n

  print $ fibonacci 100


