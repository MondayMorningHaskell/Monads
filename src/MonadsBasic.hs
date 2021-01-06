module MonadsBasic where

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

-- Evaluating each input leads to the "triangle" anti-pattern
runMaybeFuncs :: String -> Maybe [Int]
runMaybeFuncs input = case maybeFunc1 input of
  Nothing -> Nothing
  Just i -> case maybeFunc2 i of
    Nothing -> Nothing
    Just f -> maybeFunc3 f
    -- Imagine it keeps going...
    -- case maybeFunc3 of
    --   Nothing -> ...
    --   Just y -> ...

-- TODO: Write the above function in one line using the >>= operator!
runMaybeFuncsBind :: String -> Maybe [Int]
runMaybeFuncsBind input = undefined

-- TODO: Write the above function with "do" notation!
runMaybeFuncsDo :: String -> Maybe [Int]
runMaybeFuncsDo input = undefined

-- TODO: How do we add "2" to the result of the first function?
--       First try using "do" notation, then try using (>>=)
runMaybeFuncsDo2 :: String -> Maybe [Int]
runMaybeFuncsDo2 input = undefined

runMaybeFuncsBind2 :: String -> Maybe [Int]
runMaybeFuncsBind2 input = undefined

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

-- TODO: Call the 3 functions above!
--       Use do-notation or (>>=)
runEitherFuncs :: String -> Either String [Int]
runEitherFuncs input = undefined

-- Using a different error type is a **different monad**!
data CustomError = CustomError

eitherFuncCustom :: Either CustomError Float
eitherFuncCustom = undefined

-- TODO: use the IO monad to write an "echo" program
--
-- Hint: Use these functions:
--       getLine :: IO String
--       print :: String -> IO ()
main :: IO ()
main = undefined
