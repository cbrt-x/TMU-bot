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

import Data.Bifunctor

import Debug.Trace

data Config = MkConfig
  { hatGuyConfig :: HatGuyConfig }

data HatGuyConfig = MkHatGuyConfig
  { hatGuyUserId   :: UserId
  , hatGuyResponse :: Text
  }

main :: IO ()
main = do
  -- TODO error handling
  token        <- T.strip <$> TIO.readFile "config/token.secret"
  hatGuyConfig <- MkHatGuyConfig <$> (mkId . read . T.unpack . T.strip <$> TIO.readFile "config/hat_guy")
                                 <*> (TIO.readFile "config/response")
  fatalError   <- runBot token (MkConfig hatGuyConfig)
  TIO.hPutStrLn stderr fatalError
  -- TODO exit with non-zero

runBot :: Text -> Config -> IO Text
runBot token config = runDiscord $ RunDiscordOpts
  { discordToken               = token
  , discordOnStart             = pure ()
  , discordOnEnd               = pure ()
  , discordOnEvent             = handleEvent config
  , discordOnLog               = writeLog
  , discordGatewayIntent       = def
  , discordForkThreadForEvents = False
  , discordEnableCache         = False
  }

writeLog :: Text -> IO ()
writeLog = TIO.putStrLn

handleEvent :: Config -> Event -> DiscordHandler ()
handleEvent (MkConfig (MkHatGuyConfig hatGuy response)) = \case
  MessageCreate m -> when (shouldRespondToHatGuy hatGuy m) (respond m response)
  _ -> pure ()

respond :: Message -> Text -> DiscordHandler ()
respond m = void . restCall . R.CreateMessage (messageChannelId m)

shouldRespondToHatGuy :: UserId -> Message -> Bool
shouldRespondToHatGuy hatGuy message =
  userId (messageAuthor message) == hatGuy &&
  isLikelyHatDraw (messageContent message)

isLikelyHatDraw :: Text -> Bool
isLikelyHatDraw text
  | Just (x, xs)    <- traceShowId (getFirstEmoji text)
  , Just in_between <- findBetween x xs = T.toLower in_between == "next"
  | otherwise = False

findBetween :: Text -> Text -> Maybe Text
findBetween emoji text
  | T.null end  = Nothing
  | otherwise = Just $ T.strip between
  where (between, end) = T.breakOn emoji text

getFirstEmoji :: Text -> Maybe (Text, Text)
getFirstEmoji text = T.uncons rest >>= getEmoji
  where
    (_, rest) = T.break (\c -> isUnicodeEmoji c || c == '<') text

    getEmoji ('<', _) = getUntilClosing rest
    getEmoji (c,  re) = Just (T.singleton c, re)

getUntilClosing :: Text -> Maybe (Text, Text)
getUntilClosing text = 
  let (inside, rest) = T.break (== '>') text
  in  first (T.snoc inside) <$> T.uncons rest

isUnicodeEmoji :: Char -> Bool
isUnicodeEmoji c = generalCategory c == OtherSymbol

mkId :: Word64 -> UserId
mkId = DiscordId . Snowflake
