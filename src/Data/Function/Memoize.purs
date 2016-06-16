-- | This module defines functions for _memoizing_ functions, i.e. creating functions which
-- | remember their results.
-- |
-- | This module works by turning a function into a lazily-evaluated data structure depending on
-- | its domain type.

module Data.Function.Memoize
  ( class Tabulate
  , tabulate
  , class Memoize
  , memoize
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Lazy (Lazy, force, defer)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- | The `Tabulate` class identifies those types which can be used as the domain of
-- | a memoized function, i.e. those for which the results can be _tabulated_.
class Tabulate a where
  tabulate :: forall r. (a -> r) -> a -> Lazy r

instance tabulateUnit :: Tabulate Unit where
  tabulate f = let r = defer (\_ -> f unit)
               in \_ -> r

instance tabulateBool :: Tabulate Boolean where
  tabulate f = let r1 = defer (\_ -> f true)
                   r2 = defer (\_ -> f false)
               in \b -> if b then r1 else r2

instance tabulateMaybe :: Tabulate a => Tabulate (Maybe a) where
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
  tabulate f = let f' = tabulate \a -> tabulate \b -> f (Tuple a b)
               in \(Tuple a b) -> do g <- f' a
                                     g b

instance tabulateList :: Tabulate a => Tabulate (List a) where
  tabulate f = let f' = tabulate (f <<< toList)
               in f' <<< fromList
    where
      toList Nothing = Nil
      toList (Just (Tuple head tail)) = Cons head tail

      fromList Nil = Nothing
      fromList (Cons head tail) = Just (Tuple head tail)

data NatTrie a = NatTrie (Lazy a)
                         (Lazy (NatTrie a))
                         (Lazy (NatTrie a))

instance tabulateNat :: Tabulate Int where
  tabulate = tabulateImpl
    where
      tabulateImpl :: forall r. (Int -> r) -> Int -> Lazy r
      tabulateImpl f = go
        where
          go :: Int -> Lazy r
          go 0 = zer
          go n = walk (bits (if n > 0 then n else (-n)))
                      (if n > 0 then pos else neg)

          pos :: NatTrie r
          pos = build 1

          neg :: NatTrie r
          neg = build (-1)

          zer :: Lazy r
          zer = defer \_ -> f 0

          build :: Int -> NatTrie r
          build n = NatTrie (defer \_ -> f n)
                            (defer \_ -> build (n * 2))
                            (defer \_ -> build (n * 2 + 1))

          bits :: Int -> List Boolean
          bits = bits' Nil
            where
            bits' acc 1 = acc
            bits' acc n = bits' (Cons (mod n 2 /= 0) acc) (n / 2)

          walk :: forall a. List Boolean -> NatTrie a -> Lazy a
          walk Nil             (NatTrie a _ _) = a
          walk (Cons false bs) (NatTrie _ l _) = l >>= walk bs
          walk (Cons true  bs) (NatTrie _ _ r) = r >>= walk bs

-- | The `Memoize` class identifies those function types which can be memoized.
-- |
-- | If the domain type can be tabulated, then functions can be memoized.
class Memoize a where
  memoize :: a -> a

instance tabulateFunction :: Tabulate a => Memoize (a -> r) where
  memoize f = force <<< f1
    where
      f1 = tabulate f
