module MonadsBasicComplete where

import Data.Char

{- Monad instances

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

instance Monad Maybe where
  return = Just
  Nothing >>= _ = Nothing
  Just a >>= f = f a

instance Monad (Either a) where
  return r = Right r
  (Left l) >>= _ = Left l
  (Right r) >>= f = f r
-}

maybeFunc1 :: String -> Maybe Int
maybeFunc1 "" = Nothing
maybeFunc1 str = Just $ length str

maybeFunc2 :: Int -> Maybe Float
maybeFunc2 i = if i `mod` 2 == 0
  then Nothing
  else Just ((fromIntegral i) * 3.14159)

maybeFunc3 :: Float -> Maybe [Int]
maybeFunc3 f = if f > 15.0
  then Nothing
  else Just [floor f, ceiling f]

runMaybeFuncs :: String -> Maybe [Int]
runMaybeFuncs input = case maybeFunc1 input of
  Nothing -> Nothing
  Just i -> case maybeFunc2 i of
    Nothing -> Nothing
    Just f -> maybeFunc3 f

-- No "triangle pattern!"
runMaybeFuncsBind :: String -> Maybe [Int]
runMaybeFuncsBind input = maybeFunc1 input >>= maybeFunc2 >>= maybeFunc3

-- Almost like we're writing procedural code!
runMaybeFuncsDo :: String -> Maybe [Int]
runMaybeFuncsDo input = do
  i <- maybeFunc1 input
  f <- maybeFunc2 i
  maybeFunc3 f

-- Adding 2 to the first result
-- Easy with do-notation!
runMaybeFuncsDo2 :: String -> Maybe [Int]
runMaybeFuncsDo2 input = do
  i <- maybeFunc1 input
  f <- maybeFunc2 (i + 2)
  maybeFunc3 f

-- Not so nice
runMaybeFuncsBind2 :: String -> Maybe [Int]
runMaybeFuncsBind2 input = maybeFunc1 input
  >>= (\i -> maybeFunc2 (i + 2))
  >>= maybeFunc3

-- Using the Either monad
eitherFunc1 :: String -> Either String Int
eitherFunc1 "" = Left "String cannot be empty!"
eitherFunc1 str = Right $ length str

eitherFunc2 :: Int -> Either String Float
eitherFunc2 i = if i `mod` 2 == 0
  then Left "Length cannot be even!"
  else Right ((fromIntegral i) * 3.14159)

eitherFunc3 :: Float -> Either String [Int]
eitherFunc3 f = if f > 15.0
  then Left "Float is too large!"
  else Right [floor f, ceiling f]

runEitherFuncs :: String -> Either String [Int]
runEitherFuncs input = do
  i <- eitherFunc1 input
  f <- eitherFunc2 i
  eitherFunc3 f

-- Using a different error type is a **different monad**!
data CustomError = CustomError

eitherFuncCustom :: Either CustomError Float
eitherFuncCustom = undefined

-- Using the IO monad to write an "echo" program
main :: IO ()
main = do
  -- getLine :: IO String
  input <- getLine
  let uppercased = map Data.Char.toUpper input
  -- print :: String -> IO ()
  print uppercased
