-- allows "string literals" to be Text
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void, when)
import Data.Text (Text, isPrefixOf, toLower)
import qualified Data.Text.IO as TIO
import Discord
import qualified Discord.Requests as R
import Discord.Types
import System.Environment (getEnv)
import qualified Data.Text as T

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

goalDetected :: Message -> Bool
goalDetected = ("conquista" `isPrefixOf`) . toLower . messageContent

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
  MessageCreate m -> when (not (fromBot m) && goalDetected m) $ do
    void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
    void $ restCall (R.CreateMessage (messageChannelId m) "Meu objetivo é a conquista!!!")
  _ -> return ()

main :: IO ()
main = do
  putStrLn "Let it rip"
  token <- getEnv "BOT_TOKEN"
  userFacingError <-
    runDiscord $
      def
        { discordToken = T.pack token,
          discordOnEvent = eventHandler,
          discordOnLog = TIO.putStrLn,
          discordForkThreadForEvents = True
        }
  TIO.putStrLn userFacingError