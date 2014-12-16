module Data.Function.Memoized 
  ( Memo
  , memo  
  ) where

import Data.Maybe
import Data.Tuple
import Data.Either
import Data.Lazy

class Memo a where
  memo :: forall r. (a -> r) -> a -> Lazy r

instance memoUnit :: Memo Unit where
  memo f = let r = defer (\_ -> f unit) 
           in \_ -> r 

instance memoBool :: Memo Boolean where
  memo f = let r1 = defer (\_ -> f true)
               r2 = defer (\_ -> f false)
           in \b -> if b then r1 else r2

instance memoMaybe :: (Memo a) => Memo (Maybe a) where
  memo f = let n = defer (\_ -> f Nothing)
               j = memo (f <<< Just)
           in \m -> case m of
                      Nothing -> n
                      Just a  -> j a

instance memoEither :: (Memo a, Memo b) => Memo (Either a b) where
  memo f = let l = memo (f <<< Left)
               r = memo (f <<< Right)
           in \e -> case e of
                      Left a  -> l a
                      Right b -> r b

instance memoTuple :: (Memo a, Memo b) => Memo (Tuple a b) where
  memo f = let f' = memo (\a -> memo (\b -> f (Tuple a b)))
           in \(Tuple a b) -> do g <- f' a
                                 g b

data NatTrie a = NatTrie (Lazy a) 
                         (Lazy (NatTrie a)) 
                         (Lazy (NatTrie a))

instance memoNat :: Memo Number where
  memo f = let t = build 0

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

