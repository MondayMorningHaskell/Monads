module ReaderWriterComplete where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)

main1 :: IO ()
main1 = do
  env <- loadEnv
  let str = func1 env
  print str

data Environment = Environment
  { param1 :: String
  , param2 :: String
  , param3 :: String
  }

loadEnv :: IO Environment
loadEnv = do
  p1 <- lookupEnv "param1"
  p2 <- lookupEnv "param2"
  p3 <- lookupEnv "param3"
  return $ Environment
    (fromMaybe "param1" p1)
    (fromMaybe "parameter2" p2)
    (fromMaybe "p3" p3)

-- These functions all need to have the Environment,
-- even though only func3 uses it!
func1 :: Environment -> String
func1 env = "Result: " ++ (show (func2 env))

func2 :: Environment -> Int
func2 env = 2 + floor (func3 env)

func3 :: Environment -> Float
func3 env = (fromIntegral $ l1 + l2 + l3) * 2.1
  where
    l1 = length (param1 env)
    l2 = length (param2 env) * 2
    l3 = length (param3 env) * 3

-- Now we use the Reader monad!
main2 :: IO ()
main2 = do
  env <- loadEnv
  let str = runReader func1' env
  print str

-- func1 and func2 don't need the extra parameter anymore
func1' :: Reader Environment String
func1' = do
  res <- func2'
  return ("Result: " ++ show res)

func2' :: Reader Environment Int
func2' = do
  env <- ask
  let res3 = func3 env
  return (2 + floor res3)

-- Accumulation Functions

-- Calls acc2 if even length, acc3 and acc4 if odd
acc1 :: String -> (Int, String)
acc1 input = if length input `mod` 2 == 0
  then acc2 (0, input)
  else (i1 + i2, str1 ++ str2)
    where
      (i1, str1) = acc3 (0, tail input)
      (i2, str2) = acc4 (0, take 1 input)

-- Calls acc4 on truncated version
acc2 :: (Int, String) -> (Int, String)
acc2 (prev, input) = if (length input) > 10
  then acc4 (prev + 1, take 9 input)
  else (10, input)

-- Calls acc2 on expanded version if a multiple of 3
acc3 :: (Int, String) -> (Int, String)
acc3 (prev, input) = if (length input) `mod` 3 == 0
  then (prev + f2resI + 3, f2resStr)
  else (prev + 1, tail input)
  where
    (f2resI, f2resStr) = acc2 (prev, input ++ "ab")

acc4 :: (Int, String) -> (Int, String)
acc4 (prev, input) = if (length input) < 10
  then (prev + length input, input ++ input)
  else (prev + 5, take 5 input)

{-
class Semigroup a where
  -- Also known as `mappend`
  (<>) :: a -> a -> a

class (Semigroup a) => Monoid a where
  mempty :: a
-}

instance Semigroup Int where
  a <> b = a + b

instance Monoid Int where
  mempty = 0

acc1' :: String -> (String, Int)
acc1' input = if length input `mod` 2 == 0
  then runWriter (acc2' input)
  else runWriter $ do
    str1 <- acc3' (tail input)
    str2 <- acc4' (take 1 input)
    return (str1 ++ str2)

acc2' :: String -> Writer Int String
acc2' input = if (length input) > 10
  then do
    tell 1
    acc4' (take 9 input)
  else do
    tell 10
    return input

acc3' :: String -> Writer Int String
acc3' input = if (length input) `mod` 3 == 0
  then do
    tell 3
    acc2' (input ++ "ab")
  else do
    tell 1
    return $ tail input

acc4' :: String -> Writer Int String
acc4' input = if (length input) < 10
  then do
    tell (length input)
    return (input ++ input)
  else do
    tell 5
    return (take 5 input)
