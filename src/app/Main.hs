{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Map qualified as Map
import Data.Text (Text, append, pack, splitOn)
import Data.Text.IO qualified as T

main :: IO ()
main = print $ getDomain "test@example.com"

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

printResult :: EitherIO LoginError Text -> IO ()
printResult input = do
  e <- runEitherIO input
  case e of
    Right domain -> T.putStrLn $ append "Domain: " domain
    Left InvalidEmail -> T.putStrLn "Invalid Email"
    Left NoSuchUser -> T.putStrLn "No Such User"
    Left WrongPassword -> T.putStrLn "Wrong Password"

getToken :: EitherIO LoginError Text
getToken = do
  liftIO $ T.putStrLn "Please enter your email:"
  text <- liftIO T.getLine
  liftEither $ getDomain text

userLogin :: EitherIO LoginError Text
userLogin = do
  domain <- getToken
  userpw <- maybe (throwE NoSuchUser) return (Map.lookup domain users)
  input <- liftIO $ T.putStrLn "Please enter your password:" >> T.getLine
  if input == userpw
    then return domain
    else throwE WrongPassword

newtype EitherIO e a = EitherIO {runEitherIO :: IO (Either e a)}

instance Functor (EitherIO e) where
  fmap f x =
    let ioe = runEitherIO x
     in EitherIO $ fmap (fmap f) ioe

instance Applicative (EitherIO e) where
  pure x = EitherIO $ return $ Right x
  f <*> x =
    let ioef = runEitherIO f
        ioex = runEitherIO x
     in EitherIO $ do
          ef <- ioef
          ex <- ioex
          return $ ef <*> ex

instance Monad (EitherIO e) where
  return = pure
  x >>= f =
    let iox = runEitherIO x
     in EitherIO $ do
          res <- iox
          case res of
            Left err -> return $ Left err
            Right val -> runEitherIO (f val)

liftEither :: Either a b -> EitherIO a b
liftEither e = EitherIO $ return e

liftIO :: IO b -> EitherIO a b
liftIO io = EitherIO $ fmap Right io

throwE :: e -> EitherIO e a
throwE err = liftEither $ Left err
