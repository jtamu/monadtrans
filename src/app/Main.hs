{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Data.Map qualified as Map
import Data.Text (Text, append, pack, splitOn)
import Data.Text.IO qualified as T

main :: IO ()
main = void $ runExceptIO loginDialog

loginDialog :: ExceptIO LoginError ()
loginDialog = do
  let retry = userLogin `catchE` wrongPasswordHandler
  token <- retry `catchE` printError
  liftIO $ T.putStrLn (append "Logged in with token: " token)

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

printResult :: ExceptIO LoginError Text -> IO ()
printResult input = do
  e <- runExceptIO input
  case e of
    Right domain -> T.putStrLn $ append "Domain: " domain
    Left InvalidEmail -> T.putStrLn "Invalid Email"
    Left NoSuchUser -> T.putStrLn "No Such User"
    Left WrongPassword -> T.putStrLn "Wrong Password. No more chances."

printError :: LoginError -> ExceptIO LoginError a
printError e = do
  liftIO $ T.putStrLn $ case e of
    InvalidEmail -> "Invalid Email"
    NoSuchUser -> "No Such User"
    WrongPassword -> "Wrong Password. No more chances."
  throwE e

getToken :: ExceptIO LoginError Text
getToken = do
  liftIO $ T.putStrLn "Please enter your email:"
  text <- liftIO T.getLine
  liftEither $ getDomain text

userLogin :: ExceptIO LoginError Text
userLogin = do
  domain <- getToken
  userpw <- maybe (throwE NoSuchUser) return (Map.lookup domain users)
  input <- liftIO $ T.putStrLn "Please enter your password:" >> T.getLine
  if input == userpw
    then return domain
    else throwE WrongPassword

newtype ExceptIO e a = EitherIO {runExceptIO :: IO (Either e a)}

instance Functor (ExceptIO e) where
  fmap f x =
    let ioe = runExceptIO x
     in EitherIO $ fmap (fmap f) ioe

instance Applicative (ExceptIO e) where
  pure x = EitherIO $ return $ Right x
  f <*> x =
    let ioef = runExceptIO f
        ioex = runExceptIO x
     in EitherIO $ do
          ef <- ioef
          ex <- ioex
          return $ ef <*> ex

instance Monad (ExceptIO e) where
  return = pure
  x >>= f =
    let iox = runExceptIO x
     in EitherIO $ do
          res <- iox
          case res of
            Left err -> return $ Left err
            Right val -> runExceptIO (f val)

liftEither :: Either a b -> ExceptIO a b
liftEither e = EitherIO $ return e

liftIO :: IO b -> ExceptIO a b
liftIO io = EitherIO $ fmap Right io

throwE :: e -> ExceptIO e a
throwE err = liftEither $ Left err

catchE :: ExceptIO e a -> (e -> ExceptIO e a) -> ExceptIO e a
catchE throwing handler =
  EitherIO $ do
    e <- runExceptIO throwing
    case e of
      Left err -> runExceptIO (handler err)
      success -> return success

wrongPasswordHandler :: LoginError -> ExceptIO LoginError Text
wrongPasswordHandler WrongPassword = do
  liftIO $ T.putStrLn "Wrong password. One more chance."
  userLogin
wrongPasswordHandler err = throwE err
