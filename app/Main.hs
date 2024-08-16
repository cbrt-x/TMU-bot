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
  | Just (x, xs) <- getFirstEmoji text
    = not $ T.null (findBetweenChar x xs)
  | otherwise = False

findBetweenChar :: Char -> Text -> Text
findBetweenChar char text = 
  let (between, _) = T.break (== char) text
  in  T.strip between

getFirstEmoji :: Text -> Maybe (Char, Text)
getFirstEmoji text = T.uncons rest
  where
    (_, rest) = T.break isUnicodeEmoji text

isUnicodeEmoji :: Char -> Bool
isUnicodeEmoji c = generalCategory c == OtherSymbol

mkId :: Word64 -> UserId
mkId = DiscordId . Snowflake
