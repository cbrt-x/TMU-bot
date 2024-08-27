{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord
    ( restCall,
      runDiscord,
      FromJSON,
      def,
      DiscordHandler,
      RestCallErrorCode(RestCallErrorCode),
      RunDiscordOpts(discordEnableCache, RunDiscordOpts, discordToken,
                     discordOnStart, discordOnEnd, discordOnEvent, discordOnLog,
                     discordGatewayIntent, discordForkThreadForEvents) )
import Discord.Types
    ( Event(MessageUpdate, MessageCreate),
      User(userId),
      Message(messageContent, messageId, messageChannelId,
              messageAuthor),
      MessageReference(referenceMessageId),
      UserId )
import qualified Discord.Requests as R

import Control.Monad (void)

import Data.Function ( applyWhen )
import Data.Maybe ( fromMaybe )

import Data.Char (GeneralCategory(..), generalCategory)

import Data.Bifunctor ( Bifunctor(first) )

import GHC.Generics ( Generic )
import Data.Aeson ( eitherDecodeFileStrict )


import System.Environment.XDG.BaseDir ( getUserConfigDir )
import System.FilePath ( (</>) )
import Control.Monad.IO.Class ( MonadIO(..) )
import System.IO ( Handle, stdout, hPutStrLn, stderr )

data Config = MkConfig
  { hatGuy :: HatGuyConfig
  , token  :: Text
  } deriving Generic

data HatGuyConfig = MkHatGuyConfig
  { hatGuyUserId       :: UserId
  , hatGuyResponse     :: Text
  , hatGuyResponseEdit :: Maybe Text
  } deriving Generic

instance FromJSON Config
instance FromJSON HatGuyConfig

data LogLevel = LogInfo | LogError

logMsg :: MonadIO m => LogLevel -> String -> m ()
logMsg (logConfig -> (ll, hdl)) str = liftIO $ hPutStrLn hdl (concat ["[", ll, "] ", str])

logInfo :: MonadIO m => String -> m ()
logInfo = logMsg LogInfo

logConfig :: LogLevel -> (String, Handle)
logConfig ll = (ppr, hdl)
  where
    ppr = case ll of
      LogInfo -> "INFO"
      LogError -> "ERROR"
    hdl = case ll of
      LogError -> stderr
      _        -> stdout

main :: IO ()
main = do
  logInfo "starting bot"
  conf_dir <- getUserConfigDir "tmu-bot"
  let conf_file = conf_dir </> "config.json"
  logInfo $ "attempting to load config from: " ++ conf_file
  conf  <- eitherDecodeFileStrict conf_file
  case conf of
    Left err -> logMsg LogError err
    Right ok -> do
      logInfo "successfully loaded config"
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
handleEvent hatGuyConfig = \case
  MessageCreate m -> handleMessageCreate hatGuyConfig m
  MessageUpdate cid mid -> do 
    mm <- restCall (R.GetChannelMessage (cid, mid)) 
    case mm of 
      Left (RestCallErrorCode code a b) -> logMsg LogError (T.unpack $ T.concat [T.pack $ show code, ": ", a, " ", b])
      Right ok -> handleMessageUpdate hatGuyConfig ok
  _ -> pure ()

handleMessageCreate :: HatGuyConfig -> Message -> DiscordHandler ()
handleMessageCreate = handleHatGuyMessage False

handleHatGuyMessage :: Bool -> HatGuyConfig -> Message -> DiscordHandler ()
handleHatGuyMessage edit (MkHatGuyConfig hatGuy response edit_response) m
  | Just emoji <- shouldRespondToHatGuy hatGuy m = do
        logInfo "responding to hat-guy"
        respond m $ def
          { R.messageDetailedContent = applyWhen edit ((fromMaybe defaultEditResponse edit_response <> "\n") <>) $ replaceAll emoji (mention hatGuy) response
          , R.messageDetailedReference = Just (def { referenceMessageId = Just (messageId m) })
          }
  | otherwise = pure ()

defaultEditResponse :: Text
defaultEditResponse = "Editing your messages in order to surpass the rules is not allowed!"

handleMessageUpdate :: HatGuyConfig -> Message -> DiscordHandler ()
handleMessageUpdate = handleHatGuyMessage True

mention :: UserId -> Text
mention uid = T.concat ["<@", T.pack (show uid), ">"]

replaceAll :: Text -> Text -> Text -> Text
replaceAll emoji author text = T.replace "{emoji}" emoji (T.replace "{mention}" author text)

respond :: Message -> R.MessageDetailedOpts -> DiscordHandler ()
respond m = void . restCall . R.CreateMessageDetailed (messageChannelId m)

shouldRespondToHatGuy :: UserId -> Message -> Maybe Text
shouldRespondToHatGuy hatGuy message
  | userId (messageAuthor message) == hatGuy = isLikelyHatDraw (messageContent message)
  | otherwise = Nothing

isLikelyHatDraw :: Text -> Maybe Text
isLikelyHatDraw text
  | Just (x, xs)    <- getFirstEmoji text
  , Just in_between <- findBetween x xs 
  , in_between `containsAnyOf` [ "next", "band" ] = Just x
  | otherwise = Nothing
  where containsAnyOf haystack = any (`T.isInfixOf` haystack)

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
