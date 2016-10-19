{-# LANGUAGE RecordWildCards #-}
module Main where

import           ClassyPrelude
import           Common
import           Control.Monad.Except
import           Data.Aeson
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           LessonBuilder
import           Network.CGI
import           Options.Applicative
import           System.Directory
import           System.Log.Formatter
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Logger


prepareLogger :: FilePath -> IO ()
prepareLogger targetFile = do
    updateGlobalLogger rootLoggerName (setLevel DEBUG)
    fHandler <- flip setFormatter formatter <$> fileHandler targetFile DEBUG
    updateGlobalLogger rootLoggerName $ setHandlers [fHandler, cmdHandler]
  where
    formatter = simpleLogFormatter "$time [$prio:$loggername] $msg"
    cmdHandler = GenericHandler { priority = ERROR
                                , formatter = formatter
                                , privData = stderr
                                , writeFunc = const logCGI
                                , closeFunc = const $ return ()
                                }


main :: IO ()
main = do
    Opts{..} <- execParser optsParser
    runCGI $ handleErrors $ do
        raw <- liftIO $ readFile watchConf
        case eitherDecode raw of
            Left err -> error err
            Right conf -> do
                liftIO $ prepareLogger logLocation
                liftIO $ maybe (return ()) (createDirectoryIfMissing True) (dataDirectory conf)
                absLogLoc <- liftIO $  makeAbsolute logLocation

                body <- getBodyFPS
                userAgent <- B.pack . fromMaybe (error "No user agent") <$> getVar "HTTP_USER_AGENT"
                eventHeader <- B.pack . fromMaybe (error "No event header") <$> getVar "HTTP_X_GITHUB_EVENT"
                signature <- getVar "HTTP_SIGNATURE"
                res <- liftIO $ runExceptT $ handleCommon absLogLoc conf body userAgent eventHeader signature
                case res of
                    Left err -> outputError 400 "Invalid Request" $ return $ BL.unpack err
                    Right v -> outputFPS v
