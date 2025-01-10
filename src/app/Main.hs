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

printResult :: Either LoginError Text -> IO ()
printResult = T.putStrLn . loginEitherToText

getToken :: IO (Either LoginError Text)
getToken = do
  T.putStrLn "Please enter your email:"
  getDomain <$> T.getLine

userLogin :: IO (Either LoginError Text)
userLogin = do
  token <- getToken
  case token of
    left@(Left _) -> return left
    Right domain ->
      case Map.lookup domain users of
        Nothing -> return $ Left NoSuchUser
        Just userpw -> do
          T.putStrLn "Please enter your password:"
          input <- T.getLine
          if input == userpw
            then return $ Right (append "Domain: " domain)
            else return $ Left WrongPassword
