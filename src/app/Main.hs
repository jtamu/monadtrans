{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text, splitOn)

main :: IO ()
main = print $ getDomain "test@example.com"

data LoginError = InvalidEmail deriving (Show)

getDomain :: Text -> Either LoginError Text
getDomain email =
  case splitOn "@" email of
    [_, domain] -> Right domain
    _ -> Left InvalidEmail
