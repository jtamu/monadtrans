{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Data.Map qualified as Map
import Data.Text (Text, append, pack, splitOn)
import Data.Text.IO qualified as T

main :: IO ()
main = void $ runExceptT loginDialog

loginDialog :: ExceptT IO LoginError ()
loginDialog = do
  let retry = userLogin `catchE` wrongPasswordHandler
  token <- retry `catchE` printError
  lift $ T.putStrLn (append "Logged in with token: " token)

data LoginError = InvalidEmail | NoSuchUser | WrongPassword deriving (Show)

getDomain :: Text -> Either LoginError Text
getDomain email =
  case splitOn "@" email of
    [_, domain] -> Right domain
    _ -> Left InvalidEmail

users :: Map.Map Text Text
users = Map.fromList [("example.com", "password123"), ("localhost", "password")]

printResult' :: Either LoginError Text -> IO ()
printResult' (Right text) = T.putStrLn (append "Domain: " text)
printResult' (Left a) = T.putStrLn $ append "Error: " (pack $ show a)

loginEitherToText :: Either LoginError Text -> Text
loginEitherToText = either (const "Invalid Email") (append "Domain: ")

printResult :: ExceptT IO LoginError Text -> IO ()
printResult input = do
  e <- runExceptT input
  case e of
    Right domain -> T.putStrLn $ append "Domain: " domain
    Left InvalidEmail -> T.putStrLn "Invalid Email"
    Left NoSuchUser -> T.putStrLn "No Such User"
    Left WrongPassword -> T.putStrLn "Wrong Password. No more chances."

printError :: LoginError -> ExceptT IO LoginError a
printError e = do
  lift $ T.putStrLn $ case e of
    InvalidEmail -> "Invalid Email"
    NoSuchUser -> "No Such User"
    WrongPassword -> "Wrong Password. No more chances."
  throwE e

getToken :: ExceptT IO LoginError Text
getToken = do
  lift $ T.putStrLn "Please enter your email:"
  text <- lift T.getLine
  liftEither $ getDomain text

userLogin :: ExceptT IO LoginError Text
userLogin = do
  domain <- getToken
  userpw <- maybe (throwE NoSuchUser) return (Map.lookup domain users)
  input <- lift $ T.putStrLn "Please enter your password:" >> T.getLine
  if input == userpw
    then return domain
    else throwE WrongPassword

newtype ExceptT m e a = ExceptT {runExceptT :: m (Either e a)}

instance (Functor m) => Functor (ExceptT m e) where
  fmap f x =
    let ioe = runExceptT x
     in ExceptT $ fmap (fmap f) ioe

instance (Monad m) => Applicative (ExceptT m e) where
  pure x = ExceptT $ return $ Right x
  f <*> x =
    let ioef = runExceptT f
        ioex = runExceptT x
     in ExceptT $ do
          ef <- ioef
          ex <- ioex
          return $ ef <*> ex

instance (Monad m) => Monad (ExceptT m e) where
  return = pure
  x >>= f =
    let iox = runExceptT x
     in ExceptT $ do
          res <- iox
          case res of
            Left err -> return $ Left err
            Right val -> runExceptT (f val)

liftEither :: (Monad m) => Either a b -> ExceptT m a b
liftEither e = ExceptT $ return e

lift :: (Functor m) => m b -> ExceptT m a b
lift io = ExceptT $ fmap Right io

throwE :: (Monad m) => e -> ExceptT m e a
throwE err = liftEither $ Left err

catchE :: (Monad m) => ExceptT m e a -> (e -> ExceptT m e a) -> ExceptT m e a
catchE throwing handler =
  ExceptT $ do
    e <- runExceptT throwing
    case e of
      Left err -> runExceptT (handler err)
      success -> return success

wrongPasswordHandler :: LoginError -> ExceptT IO LoginError Text
wrongPasswordHandler WrongPassword = do
  lift $ T.putStrLn "Wrong password. One more chance."
  userLogin
wrongPasswordHandler err = throwE err
