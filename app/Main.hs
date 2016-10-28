{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           ClassyPrelude
import           Common
import           Control.Monad.Except
import           Data.Aeson
import           LessonBuilder
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Options.Applicative
import           System.Directory
import           System.Log.Formatter
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Logger
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL


prepareLogger :: FilePath -> IO ()
prepareLogger targetFile = do
    updateGlobalLogger rootLoggerName (setLevel DEBUG)
    fHandler <- flip setFormatter formatter <$> fileHandler targetFile DEBUG
    updateGlobalLogger rootLoggerName $ setHandlers [fHandler, cmdHandler]
  where
    formatter = simpleLogFormatter "$time [$prio:$loggername] $msg"
    cmdHandler = GenericHandler { priority = DEBUG
                                , formatter = formatter
                                , privData = stderr
                                , writeFunc = hPutStrLn
                                , closeFunc = const $ return ()
                                }


readWatchConf :: (MonadIO m, MonadError BL.ByteString m) => FilePath -> m WatchConf 
readWatchConf watchConfLoc = do
    raw <- readFile watchConfLoc
    either (throwError . BL.pack) return $ eitherDecode raw


app :: FilePath -> FilePath -> Application
app logLocation confLocation request respond = do
    infoM "server" "received request"
    body <- lazyRequestBody request
    res <- runExceptT $ do
        watchConf <- readWatchConf confLocation
        eventHeader <- getHeader "X-GitHub-Event"
        userAgent <- maybe (throwError "No user agent found") return $ requestHeaderUserAgent request
        handleCommon logLocation watchConf body userAgent eventHeader (B.unpack <$> signature)
    case res of
        Left err -> do
            liftIO $ infoM "server" "Responding error" 
            respond $ responseLBS badRequest400 [] err
        Right v -> do
            liftIO $ infoM "server" "Responding okay" 
            respond $ responseLBS ok200 [] v
  where
    signature = lookup "X-Hub-Signature" $ requestHeaders request
    getHeader h = maybe (throwError "Missing header") return $ lookup h $ requestHeaders request

main :: IO ()
main = do
    Opts{..} <- execParser optsParser
    raw <- readFile watchConf
    case eitherDecode raw of
        Left err -> putStrLn (pack err)
        Right conf -> do
            prepareLogger logLocation
            maybe (return ()) (createDirectoryIfMissing True) (dataDirectory conf)
            absLogLoc <- makeAbsolute logLocation
            run port $ app absLogLoc watchConf

