{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord
import Discord.Types
import qualified Discord.Requests as R

import Control.Monad (void)

import Data.Word
import Data.Char (GeneralCategory(..), generalCategory)

import Data.Bifunctor

import GHC.Generics
import Data.Aeson

import System.Exit

data Config = MkConfig
  { hatGuy :: HatGuyConfig
  , token  :: Text
  } deriving Generic

data HatGuyConfig = MkHatGuyConfig
  { hatGuyUserId   :: UserId
  , hatGuyResponse :: Text
  } deriving Generic

instance FromJSON Config
instance FromJSON HatGuyConfig

main :: IO ()
main = do
  conf  <- eitherDecodeFileStrict "config/config.json"
  case conf of
    Left err -> die err
    Right ok -> do 
      fatalError <- runBot ok
      die (T.unpack fatalError)

runBot :: Config -> IO Text
runBot (MkConfig config token) = runDiscord $ RunDiscordOpts
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

handleEvent :: HatGuyConfig -> Event -> DiscordHandler ()
handleEvent (MkHatGuyConfig hatGuy response) = \case
  MessageCreate m 
    | Just emoji <- shouldRespondToHatGuy hatGuy m -> respond m (replaceAll emoji (mention hatGuy) response)
  _ -> pure ()

mention :: UserId -> Text
mention uid = T.concat ["<@", T.pack (show uid), ">"]

replaceAll :: Text -> Text -> Text -> Text
replaceAll emoji author text = T.replace "{emoji}" emoji (T.replace "{mention}" author text)

respond :: Message -> Text -> DiscordHandler ()
respond m = void . restCall . R.CreateMessage (messageChannelId m)

shouldRespondToHatGuy :: UserId -> Message -> Maybe Text
shouldRespondToHatGuy hatGuy message
  | userId (messageAuthor message) == hatGuy = isLikelyHatDraw (messageContent message)
  | otherwise = Nothing

isLikelyHatDraw :: Text -> Maybe Text
isLikelyHatDraw text
  | Just (x, xs)    <- getFirstEmoji text
  , Just in_between <- findBetween x xs 
  , "next" `T.isInfixOf` T.toLower in_between = Just x
  | otherwise = Nothing

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
    getEmoji _ = Just $ T.span isUnicodeEmoji rest

getUntilClosing :: Text -> Maybe (Text, Text)
getUntilClosing text = 
  let (inside, rest) = T.break (== '>') text
  in  first (T.snoc inside) <$> T.uncons rest

isUnicodeEmoji :: Char -> Bool
isUnicodeEmoji c = generalCategory c `elem` [OtherSymbol, NonSpacingMark]

mkId :: Word64 -> UserId
mkId = DiscordId . Snowflake
