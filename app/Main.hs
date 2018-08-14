module Main where

import Bot (runBot)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Environment

import Telegram.Bot.API

defaultToken = "682161850:AAHs3tmYS8ejECf4IA7gvymEWq01_JAmOto" :: String

getToken :: IO String
getToken = fromMaybe defaultToken <$> lookupEnv "BOT_TOKEN"

main :: IO ()
main = do putStrLn "Running bot"
          token <- Token . Text.pack <$> getToken
          runBot token
