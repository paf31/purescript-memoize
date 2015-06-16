module Test.Main where

import Prelude

import Data.Lazy
import Data.Function.Memoize

import Control.Monad.Eff
import Control.Monad.Eff.Console

main = do
  let fibonacciSlow 0 = 0
      fibonacciSlow 1 = 1
      fibonacciSlow n = fibonacci (n - 1) + 
                        fibonacci (n - 2)
                        
      fibonacciFast = go 0 1
        where
        go n _ 0 = n 
        go n m i = go m (n + m) (i - 1)
      
      fibonacci = memoize $ \n -> fibonacciSlow n

  print $ fibonacci 25
  print $ fibonacciFast 25

