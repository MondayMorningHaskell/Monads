module LawsComplete where

import qualified Data.Map as M
import Test.QuickCheck

{- Functor Laws

class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- Identity Law
fmap id = id

-- Composition Law
fmap (g . f) = fmap g . fmap f

-}

{- Applicative Laws

-- Identity Law
pure id <*> v = v

-- Homomorphism Law
pure f <*> pure x = pure (f x)

-- Interchange Law
u <*> pure y = pure ($ y) <*> u

-- Composition Law
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

-}

{- Monad Laws

-- Identity Laws
return a >>= f = f
m >>= return = m

-- Composition Law
(m >>= f) >>= g = m >>= (\x -> f x >>= g)

-}

data GovDirectory a = GovDirectory
  { mayor :: a
  , interimMayor :: Maybe a
  , cabinet :: M.Map String a
  , councilMembers :: [a]
  } deriving (Show, Eq)

-- An invalid functor instance!
instance Functor GovDirectory where
  fmap f oldDirectory = GovDirectory {
    mayor = f (mayor oldDirectory),
    interimMayor = Nothing, -- This isn't right!
    cabinet = f <$> cabinet oldDirectory,
    councilMembers = f <$> councilMembers oldDirectory
  }

instance Arbitrary a => Arbitrary (GovDirectory a) where
  arbitrary = do
    m <- arbitrary
    im <- arbitrary
    cab <- arbitrary
    cm <- arbitrary
    return $ GovDirectory
      { mayor = m
      , interimMayor = im
      , cabinet = cab
      , councilMembers = cm
      }

-- TODO: Fix the functor instance and run again!
main :: IO ()
main = quickCheck govDirectoryFunctorCheck

govDirectoryFunctorCheck :: GovDirectory String -> Bool
govDirectoryFunctorCheck gd = fmap id gd == gd
