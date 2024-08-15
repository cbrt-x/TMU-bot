{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord
import Discord.Types
import qualified Discord.Requests as R

import System.IO (stderr)
import Control.Monad (when, void)

import Data.Word
import Data.Char (GeneralCategory(..), generalCategory)

main :: IO ()
main = do
  -- TODO error handling
  -- TODO state ADT
  token    <- T.strip <$> TIO.readFile "config/token"
  response <- TIO.readFile "config/response"
  hatGuy   <- mkId . read . T.unpack . T.strip <$> TIO.readFile "config/hat_guy"
  fatalError <- runBot token response hatGuy
  TIO.hPutStrLn stderr fatalError
  -- TODO exit with non-zero

runBot :: Text -> Text -> UserId -> IO Text
runBot token response hatGuy = runDiscord $ RunDiscordOpts
  { discordToken               = token
  , discordOnStart             = pure ()
  , discordOnEnd               = pure ()
  , discordOnEvent             = handleEvent response hatGuy
  , discordOnLog               = writeLog
  , discordGatewayIntent       = def
  , discordForkThreadForEvents = False
  , discordEnableCache         = False
  }

writeLog :: Text -> IO ()
writeLog = TIO.putStrLn

handleEvent :: Text -> UserId -> Event -> DiscordHandler ()
handleEvent response hatGuy = \case
  MessageCreate m -> when (shouldRespondToHatGuy hatGuy m) $ do
        void $ restCall (R.CreateMessage (messageChannelId m) response)
  _ -> pure ()

shouldRespondToHatGuy :: UserId -> Message -> Bool
shouldRespondToHatGuy hatGuy message =
  userId (messageAuthor message) == hatGuy &&
  isLikelyHatDraw (messageContent message)

isLikelyHatDraw :: Text -> Bool
isLikelyHatDraw text
  | Just (x, xs) <- T.uncons rest
  , Just _ <- T.uncons (T.stripStart . snd $ T.break (== x) xs) = True
  | otherwise = False
  where (_, rest) = T.break isUnicodeEmoji text

isUnicodeEmoji :: Char -> Bool
isUnicodeEmoji c = generalCategory c == OtherSymbol

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

mkId :: Word64 -> UserId
mkId = DiscordId . Snowflake
