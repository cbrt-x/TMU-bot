{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

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

import System.Environment.XDG.BaseDir
import System.FilePath
import Control.Monad.IO.Class
import System.IO

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

data LogLevel = LogInfo | LogError

logMsg :: MonadIO m => LogLevel -> String -> m ()
logMsg ll str = liftIO $ hPutStrLn hdl (concat ["[", llPpr, "] ", str])
  where
    llPpr = case ll of
      LogInfo -> "INFO"
      LogError -> "ERROR"
    hdl = case ll of
      LogError -> stderr
      _        -> stdout


main :: IO ()
main = do
  logMsg LogInfo "starting bot"
  conf_dir <- getUserConfigDir "tmu-bot"
  let conf_file = conf_dir </> "config.json"
  logMsg LogInfo $ "attempting to load config from: " ++ conf_file
  conf  <- eitherDecodeFileStrict conf_file
  case conf of
    Left err -> logMsg LogError err
    Right ok -> do
      logMsg LogInfo "successfully loaded config"
      fatalError <- runBot ok
      logMsg LogError (T.unpack fatalError)


runBot :: Config -> IO Text
runBot (MkConfig config token) = runDiscord $ RunDiscordOpts
  { discordToken               = token
  , discordOnStart             = pure ()
  , discordOnEnd               = pure ()
  , discordOnEvent             = handleEvent config
  , discordOnLog               = TIO.putStrLn
  , discordGatewayIntent       = def
  , discordForkThreadForEvents = False
  , discordEnableCache         = False
  }

handleEvent :: HatGuyConfig -> Event -> DiscordHandler ()
handleEvent (MkHatGuyConfig hatGuy response) = \case
  MessageCreate m
    | Just emoji <- shouldRespondToHatGuy hatGuy m -> do
        logMsg LogInfo "responding to hat-guy"
        respond m (replaceAll emoji (mention hatGuy) response)
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
