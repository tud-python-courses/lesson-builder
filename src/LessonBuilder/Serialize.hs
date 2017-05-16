module LessonBuilder.Serialize where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import qualified Data.ByteString.Char8  as BS
import qualified Data.ByteString.Lazy   as B
import           Data.Text              (Text, pack)
import qualified Data.Yaml              as Yaml
import           LessonBuilder.Types
import           System.FilePath
import           Util


readConf :: MonadIO m => FilePath -> m (Either Text WatchConf)
readConf fp = mapLeft pack . reader <$> liftIO (B.readFile fp)
  where
    reader
        | takeExtension fp `elem` ([".yaml", ".yml"] :: [String]) = Yaml.decodeEither . B.toStrict
        | takeExtension fp == ".json" = eitherDecode
        | otherwise = const $ Left "Unknown Extension"


writeHook :: MonadIO m => FilePath -> Value -> m ()
writeHook path = liftIO . B.writeFile path . encode


readBuildConfig :: MonadIO m => FilePath -> m (Either String BuildConf)
readBuildConfig = fmap eitherDecode . liftIO . B.readFile


decodeEvent :: BS.ByteString -> B.ByteString -> Either String Event
decodeEvent "push" body = PushEvent <$> eitherDecode body
decodeEvent "ping" body = PingEvent <$> eitherDecode body
decodeEvent a _         = Left $ "unknown event type " ++ BS.unpack a
