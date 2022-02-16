{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE BlockArguments #-}

{-# LANGUAGE TypeOperators #-}

module Main where

import           Calamity
import           Calamity.Cache.InMemory
import Calamity.Commands
import Calamity.Commands.Context (FullContext, useFullContext)
import           Calamity.Metrics.Noop

import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Lens
import           Control.Monad

import qualified Data.Text as T

import qualified Di
import qualified DiPolysemy                  as DiP

import qualified Polysemy                    as P
import qualified Polysemy.Async              as P
import qualified Polysemy.AtomicState        as P
import qualified Polysemy.Embed              as P
import qualified Polysemy.Fail               as P

import           Prelude                     hiding ( error )

import           System.Environment          (getEnv)

import           TextShow

data Counter m a where
  GetCounter :: Counter m Int

P.makeSem ''Counter

runCounterAtomic :: P.Member (P.Embed IO) r => P.Sem (Counter ': r) a -> P.Sem r a
runCounterAtomic m = do
  var <- P.embed $ newTVarIO (0 :: Int)
  P.runAtomicStateTVar var $ P.reinterpret (\case
                                              GetCounter -> P.atomicState (\v -> (v + 1, v))) m

handleFailByLogging m = do
  r <- P.runFail m
  case r of
    Left e -> DiP.error $ T.pack e
    _      -> pure ()

info, debug :: BotC r => T.Text -> P.Sem r ()
info = DiP.info
debug = DiP.info

tellt :: (BotC r, Tellable t) => t -> T.Text -> P.Sem r (Either RestError Message)
tellt t m = tell t $ T.toStrict m

data MyCustomEvt = MyCustomEvt T.Text Message

main :: IO ()
main = do
  token <- T.pack <$> getEnv "BOT_TOKEN"
  Di.new $ \di ->
    void . P.runFinal . P.embedToFinal . DiP.runDiToIO di . runCounterAtomic
         . runCacheInMemory . runMetricsNoop . useConstantPrefix "!" . useFullContext
      $ runBotIO (BotToken token) defaultIntents $ do
      addCommands $ do
        helpCommand
        command @'[User] "utest" $ \ctx u -> do
          void $ tellt ctx $ "got user: " <> showt u
        command @'[Named "u" User, Named "u1" User] "utest2" $ \ctx u u1 -> do
          void $ tellt ctx $ "got user: " <> showt u <> "\nand: " <> showt u1
        command @'[T.Text, Snowflake User] "test" $ \ctx something aUser -> do
          info $ "something = " <> showt something <> ", aUser = " <> showt aUser
        command @'[] "hello" $ \ctx -> do
          void $ tellt ctx "heya"
        group "testgroup" $ do
          command @'[[T.Text]] "test" $ \ctx l -> do
            void $ tellt ctx ("you sent: " <> showt l)
          command @'[] "count" $ \ctx -> do
            val <- getCounter
            void $ tellt ctx ("The value is: " <> showt val)
          group "say" $ do
            command @'[KleenePlusConcat T.Text] "this" $ \ctx msg -> do
              void $ tellt ctx msg
        command @'[Snowflake Emoji] "etest" $ \ctx e -> do
          void $ tellt ctx $ "got emoji: " <> showt e
        command @'[] "explode" $ \ctx -> do
          Just x <- pure Nothing
          debug "unreachable!"
        command @'[] "bye" $ \ctx -> do
          void $ tellt ctx "bye!"
          stopBot
        command @'[] "fire-evt" $ \ctx -> do
          fire . customEvt $ MyCustomEvt "aha" (ctx ^. #message)
        command @'[T.Text] "wait-for" $ \ctx s -> do
          void $ tellt ctx ("waiting for !" <> s)
          waitUntil @'MessageCreateEvt (\msg -> msg ^. #content == ("!" <> s))
          void $ tellt ctx ("got !" <> s)
      react @'MessageCreateEvt $ \msg -> handleFailByLogging $ case msg ^. #content of
        "!say hi" -> replicateM_ 3 . P.async $ do
          info "saying heya"
          Right msg' <- tellt msg "heya"
          info "sleeping"
          P.embed $ threadDelay (5 * 1000 * 1000)
          info "slept"
          void . invoke $ EditMessage (msg ^. #channelID) msg' (editMessageContent $ Just "lol")
          info "edited"
        _ -> pure ()
      react @('CustomEvt (CtxCommandError FullContext)) \(CtxCommandError ctx e) -> do
        info $ "Command failed with reason: " <> showt e
        case e of
          ParseError n r -> void . tellt ctx $ "Failed to parse parameter: `" <> T.fromStrict n <> "`, with reason: ```\n" <> r <> "```"
      react @('CustomEvt MyCustomEvt) $ \(MyCustomEvt s m) ->
        void $ tellt m ("Somebody told me to tell you about: " <> s)

-- import Control.Monad (void, when, unless)
-- import Data.Text (Text, isPrefixOf, toLower)
-- import qualified Data.Text.IO as TIO
-- import Discord
-- import qualified Discord.Requests as R
-- import Discord.Types
-- import System.Environment (getEnv)
-- import qualified Data.Text as T

-- fromBot :: Message -> Bool
-- fromBot = userIsBot . messageAuthor

-- goalDetected :: Message -> Bool
-- goalDetected = ("objetivo" `isPrefixOf`) . toLower . messageContent

-- helloThereDetected :: Message -> Bool
-- helloThereDetected = ("hello there" `isPrefixOf`) . toLower . messageContent

-- eventHandler :: Event -> DiscordHandler ()
-- eventHandler event = case event of
--   MessageCreate m -> unless (fromBot m) $ do
--     when (goalDetected m) $ do
--       void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
--       void $ restCall (R.CreateMessage (messageChannelId m) "Meu objetivo é a conquista!!!")
--     when (helloThereDetected m) $ do
--       void $ restCall (R.CreateMessage (messageChannelId m) "General Kenobi!")
--   _ -> return ()

-- main :: IO ()
-- main = do
--   putStrLn "Let it rip"
--   token <- getEnv "BOT_TOKEN"
--   userFacingError <-
--     runDiscord $
--       def
--         { discordToken = T.pack token,
--           discordOnEvent = eventHandler,
--           discordOnLog = TIO.putStrLn,
--           discordForkThreadForEvents = True
--         }
--   TIO.putStrLn userFacingError

