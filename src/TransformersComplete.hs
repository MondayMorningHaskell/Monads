module TransformersComplete where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Data.Char (isUpper, isLower)

main1 :: IO ()
main1 = do
  maybeUserName <- readUserName
  case maybeUserName of
    Nothing -> print "Invalid user name!"
    Just (uName) -> do
      maybeEmail <- readEmail
      case maybeEmail of
        Nothing -> print "Invalid email!"
        Just (email) -> do
          maybePassword <- readPassword
          case maybePassword of
            Nothing -> print "Invalid Password"
            Just password -> login uName email password

readUserName :: IO (Maybe String)
readUserName = do
  putStrLn "Please enter your username!"
  str <- getLine
  if length str > 5
    then return $ Just str
    else return Nothing

readEmail :: IO (Maybe String)
readEmail = do
  putStrLn "Please enter your email!"
  str <- getLine
  if '@' `elem` str && '.' `elem` str
    then return $ Just str
    else return Nothing

readPassword :: IO (Maybe String)
readPassword = do
  putStrLn "Please enter your Password!"
  str <- getLine
  if length str < 8 || null (filter isUpper str) || null (filter isLower str)
    then return Nothing
    else return $ Just str

login :: String -> String -> String -> IO ()
login username email password = putStrLn $ "Now logged in as: " ++ username

{- MaybeT Reference

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Monad m) => Monad (MaybeT m) where
  return = lift . return
  x >>= f = MaybeT $ do
    v <- runMaybeT x
    case v of
      Nothing -> return Nothing
      Just y  -> runMaybeT (f y)

-}

readUserName' :: MaybeT IO String
readUserName' = MaybeT $ do
  putStrLn "Please enter your Username!"
  str <- getLine
  if length str > 5
    then return $ Just str
    else return Nothing

readEmail' :: MaybeT IO String
readEmail' = MaybeT $ do
  putStrLn "Please enter your Email!"
  str <- getLine
  if length str > 5
    then return $ Just str
    else return Nothing

readPassword' :: MaybeT IO String
readPassword' = MaybeT $ do
  putStrLn "Please enter your Password!"
  str <- getLine
  if length str < 8 || null (filter isUpper str) || null (filter isLower str)
    then return Nothing
    else return $ Just str

main2 :: IO ()
main2 = do
  maybeCreds <- runMaybeT $ do
    usr <- readUserName'
    email <- readEmail'
    pass <- readPassword'
    return (usr, email, pass)
  case maybeCreds of
    Nothing -> print "Couldn't login!"
    Just (u, e, p) -> login u e p

-- Lifting

type Env = (Maybe String, Maybe String, Maybe String)

readUserName'' :: MaybeT (ReaderT Env IO) String
readUserName'' = MaybeT $ do
  (maybeOldUser, _, _) <- ask
  case maybeOldUser of
    Just str -> return $ Just str
    Nothing -> do
      -- lift allows normal IO functions from inside ReaderT Env IO!
      lift $ putStrLn "Please enter your Username!"
      input <- lift getLine
      if length input > 5
        then return (Just input)
        else return Nothing

type TripleMonad a = MaybeT (ReaderT Env IO) a

performReader :: ReaderT Env IO a -> TripleMonad a
performReader = lift

performIO :: IO a -> TripleMonad a
performIO = lift . lift

{- MonadTrans and MonadIO Reference

class MonadTrans t where
  lift :: (Monad m) => m a -> t m a

class (Monad m) => MonadIO m where
  liftIO :: IO a -> m a

-}

debugFunc :: (MonadIO m) => String -> m ()
debugFunc input = liftIO $ putStrLn ("Successfully produced input: " ++ input)

main3 :: IO ()
main3 = do
  maybeCreds <- runMaybeT $ do
    usr <- readUserName'
    debugFunc usr
    email <- readEmail'
    debugFunc email
    pass <- readPassword'
    debugFunc pass
    return (usr, email, pass)
  case maybeCreds of
    Nothing -> print "Couldn't login!"
    Just (u, e, p) -> login u e p
