-- | This module defines functions for _memoizing_ functions, i.e. creating functions which
-- | remember their results.
-- |
-- | This module works by turning a function into a lazily-evaluated data structure depending on
-- | its domain type.

module Data.Function.Memoize
  ( class Tabulate
  , tabulate
  , memoize
  , memoize2
  , memoize3
  , gTabulate
  ) where

import Prelude
import Data.Char (fromCharCode, toCharCode)
import Data.Either (Either(..))
import Data.Generic (class Generic, GenericSpine(..), toSpine, fromSpine)
import Data.Lazy (Lazy, force, defer)
import Data.List (List(..), fromFoldable, toUnfoldable)
import Data.Maybe (Maybe(..), fromJust)
import Data.String (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..), curry, uncurry)

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

instance tabulateChar :: Tabulate Char where
  tabulate f = f1 <<< toCharCode
    where
      f1 = tabulate (f <<< fromCharCode)

instance tabulateString :: Tabulate String where
  tabulate f = f1 <<< toCharArray
    where
      f1 = tabulate (f <<< fromCharArray)

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


instance tabulateArray :: Tabulate a => Tabulate (Array a) where
  tabulate f = f1 <<< fromFoldable
    where
      f1 = tabulate (f <<< toUnfoldable)

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

-- | Memoize a function of one argument
memoize :: forall a b. Tabulate a => (a -> b) -> a -> b
memoize f = force <<< f1
  where
    f1 = tabulate f

-- | Memoize a function of two arguments
memoize2 :: forall a b c. (Tabulate a, Tabulate b) => (a -> b -> c) -> a -> b -> c
memoize2 f = curry f1
  where
    f1 = memoize (uncurry f)

-- | Memoize a function of three arguments
memoize3 :: forall a b c d. (Tabulate a, Tabulate b, Tabulate c) => (a -> b -> c -> d) -> a -> b -> c -> d
memoize3 f = curry (curry f1)
  where
    f1 = memoize (uncurry (uncurry f))

instance tabulateSpine :: Partial => Tabulate GenericSpine where
  tabulate f =
    let
      sProd    = tabulate (\(Tuple ctor args) -> f (SProd ctor (map const args)))
      sRecord  = tabulate (f <<< SRecord <<< map (\(Tuple recLabel recValue) -> { recLabel, recValue: const recValue }))
      sBoolean = tabulate (f <<< SBoolean)
      sInt     = tabulate (f <<< SInt)
      sString  = tabulate (f <<< SString)
      sChar    = tabulate (f <<< SChar)
      sArray   = tabulate (f <<< SArray <<< map const)
      sUnit    = tabulate (f <<< (\(_ :: Unit) -> SUnit))
    in case _ of
         SProd ctor args  -> sProd (Tuple ctor (map (_ $ unit) args))
         SRecord rec      -> sRecord (map (\{ recLabel, recValue } -> Tuple recLabel (recValue unit)) rec)
         SBoolean b       -> sBoolean b
         SInt i           -> sInt i
         SString s        -> sString s
         SChar c          -> sChar c
         SArray arr       -> sArray (map (_ $ unit) arr)
         SUnit            -> sUnit unit

-- | A default implementation of `Tabulate` for `Generic` types.
-- |
-- | This function is marked as `Partial`, since it is not implemented
-- | for the `Number` type, or types containing `Number`. However, all other
-- | `Generic` types are supported.
gTabulate :: forall a r. Partial => Generic a => (a -> r) -> a -> Lazy r
gTabulate f = map fromJust <<< f1 <<< toSpine
  where
    f1 = tabulate (map f <<< fromSpine)
