{-# LANGUAGE RecordWildCards #-}
module Main where

import           ClassyPrelude
import           Common
import           Control.Monad.Except
import           Control.Monad.Logger
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           LessonBuilder
import           Network.CGI
import           Options.Applicative
import           System.Directory
import System.Posix.Daemonize


main :: IO ()
main = do
    Opts{..} <- execParser optsParser
    runCGI $ handleErrors $ do
        method <- requestMethod
        case method of
            "GET" -> output "Hello"
            "POST" -> do
                raw <- readConf watchConf
                case raw of
                    Left err -> error $ unpack err
                    Right conf -> do
                        liftIO $ maybe (return ()) (createDirectoryIfMissing True) (dataDirectory conf)

                        body <- getBodyFPS
                        userAgent <- B.pack . fromMaybe (error "No user agent") <$> getVar "HTTP_USER_AGENT"
                        eventHeader <- B.pack . fromMaybe (error "No event header") <$> getVar "HTTP_X_GITHUB_EVENT"
                        signature <- getVar "HTTP_SIGNATURE"
                        res <- liftIO $ runStderrLoggingT $ runExceptT $ handleCommon conf body userAgent eventHeader signature
                        case res of
                            Left err -> outputError 400 "Invalid Request" $ return $ unpack err
                            Right (msg, action) -> do 
                                
                                liftIO $ daemonize $ runStderrLoggingT $ do 
                                    runExceptT action >>= either logErrorN return
                                    logErrorN "Finished"
                                outputFPS $ encodeUtf8 $ fromStrict msg
            _ -> outputMethodNotAllowed ["GET", "POST"]
