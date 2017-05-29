{-|
Module      : $Header$
Description : Reading, writing and serializing data.
Copyright   : (c) Justus Adam 2017.
License     : MIT
Maintainer  : dev@justus.science
Stability   : experimental
Portability : portable
-}
module LessonBuilder.Serialize where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import qualified Data.ByteString.Char8  as BS
import qualified Data.ByteString.Lazy   as B
import           Data.Text              (Text)
import qualified Data.Yaml              as Yaml
import           Lens.Micro.Platform
import           LessonBuilder.Types
import           System.FilePath
import           Util


readConf :: MonadIO m => FilePath -> m (Either Text WatchConf)
readConf fp = mapLeft (^.packed) . reader <$> liftIO (B.readFile fp)
  where
    reader
        | ext == ".yaml" || ext == ".yml" = Yaml.decodeEither . B.toStrict
        | ext == ".json" = eitherDecode
        | otherwise = const $ Left "Unknown Extension"
    ext = takeExtension fp


writeHook :: MonadIO m => FilePath -> Value -> m ()
writeHook path = liftIO . B.writeFile path . encode


readBuildConfig :: MonadIO m => FilePath -> m (Either String BuildConf)
readBuildConfig = fmap eitherDecode . liftIO . B.readFile


decodeEvent :: BS.ByteString -> B.ByteString -> Either String Event
decodeEvent "push" body = PushEvent <$> eitherDecode body
decodeEvent "ping" body = PingEvent <$> eitherDecode body
decodeEvent a _         = Left $ "unknown event type " ++ BS.unpack a
