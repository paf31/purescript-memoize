module Data.Function.Memoized 
  ( Tabulate
  , tabulate  
  , Memoize
  , memoize
  ) where

import Data.Maybe
import Data.Tuple
import Data.Either
import Data.Lazy

class Tabulate a where
  tabulate :: forall r. (a -> r) -> a -> Lazy r

instance tabulateUnit :: Tabulate Unit where
  tabulate f = let r = defer (\_ -> f unit) 
               in \_ -> r 

instance tabulateBool :: Tabulate Boolean where
  tabulate f = let r1 = defer (\_ -> f true)
                   r2 = defer (\_ -> f false)
               in \b -> if b then r1 else r2

instance tabulateMaybe :: (Tabulate a) => Tabulate (Maybe a) where
  tabulate f = let n = defer (\_ -> f Nothing)
                   j = tabulate (f <<< Just)
               in \m -> case m of
                          Nothing -> n
                          Just a  -> j a

instance tabulateEither :: (Tabulate a, Tabulate b) => Tabulate (Either a b) where
  tabulate f = let l = tabulate (f <<< Left)
                   r = tabulate (f <<< Right)
               in \e -> case e of
                          Left a  -> l a
                          Right b -> r b

instance tabulateTuple :: (Tabulate a, Tabulate b) => Tabulate (Tuple a b) where
  tabulate f = let f' = tabulate (\a -> tabulate (\b -> f (Tuple a b)))
               in \(Tuple a b) -> do g <- f' a
                                     g b

data NatTrie a = NatTrie (Lazy a) 
                         (Lazy (NatTrie a)) 
                         (Lazy (NatTrie a))

instance tabulateNat :: Tabulate Number where
  tabulate f = let t = build 0

                   build n = NatTrie (defer (\_ -> f n)) 
                                     (defer (\_ -> build (n `shl` 1)))
                                     (defer (\_ -> build ((n `shl` 1) .|. 1)))

                   bits 0 = []
                   bits n | n % 2 == 0 = bits (n `shr` 1) <> [false]
                          | otherwise  = bits (n `shr` 1) <> [true]

                   walk [] (NatTrie r _ _) = r
                   walk (false : bs) (NatTrie _ t _) = t >>= walk bs
                   walk (true  : bs) (NatTrie _ _ t) = t >>= walk bs
               in \n -> walk (bits n) t

class Memoize a where
  memoize :: a -> a

instance tabulateFunction :: (Tabulate a) => Memoize (a -> r) where
  memoize f = force <<< tabulate f

