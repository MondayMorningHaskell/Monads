module ApplicativesComplete where

-- For part 2, you can work entirely within GHCI
-- This module just contains some references to
-- the Applicative definitions mentioned in the article.

{- Applicative Instances

class Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where
  pure = Just
  (<*>) Nothing _ = Nothing
  (<*>) _ Nothing = Nothing
  (<*>) (Just f) (Just x) = Just (f x)

instance Applicative [] where
  pure a = [a]
  fs <*> xs = [f x | f <- fs, x <- xs]
-}
