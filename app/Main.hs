{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           ClassyPrelude
import           Control.Monad.Except
import           Control.Monad.Logger
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           LessonBuilder
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           System.Directory
import           Options.Applicative


data Opts = Opts { logLocation :: FilePath
                 , watchConf   :: FilePath
                 , port        :: Int
                 }


optsParser :: ParserInfo Opts
optsParser = info (helper <*> struct) frame
  where
    frame = header "lesson-builder, a webhook endpoint" ++ fullDesc
    struct = Opts
        <$> strOption
                (  long "log-location"
                ++ short 'l'
                ++ metavar "PATH"
                ++ help "where to write the log to"
                )
        <*> strOption
                (  long "watch-conf"
                ++ short 'w'
                ++ metavar "PATH"
                ++ help "location of the watch config"
                ++ showDefault
                ++ value "watch_conf.json"
                )
        <*> option auto
                (  long "port"
                ++ short 'p'
                ++ metavar "INTEGER"
                ++ help "port to bind to"
                ++ showDefault
                ++ value 8000
                )



app :: FilePath -> FilePath -> Application
app logLocation confLocation request respond = runStderrLoggingT $ do
    logInfoNS "server" "received request"
    body <- liftIO $ lazyRequestBody request
    res <- runExceptT $ do
        watchConf <- ExceptT $ readConf confLocation
        eventHeader <- getHeader "X-GitHub-Event"
        userAgent <- maybe (throwError "No user agent found") return $ requestHeaderUserAgent request
        handleCommon watchConf body userAgent eventHeader (B.unpack <$> signature)
    case res of
        Left err -> do
            logInfoNS "server" "Responding error"
            liftIO $ respond $ responseLBS badRequest400 [] (encodeUtf8 $ fromStrict err)
        Right (msg, action) -> do
            logInfoNS "server" "Responding okay"
            void $ async $ runExceptT action >>= either logErrorN return
            liftIO $ respond $ responseLBS ok200 [] (encodeUtf8 $ fromStrict msg)
  where
    signature = lookup "X-Hub-Signature" $ requestHeaders request
    getHeader h = maybe (throwError "Missing header") return $ lookup h $ requestHeaders request

main :: IO ()
main = do
    Opts{..} <- execParser optsParser
    raw <- readConf watchConf
    case raw of
        Left err -> putStrLn err
        Right conf -> do
            maybe (return ()) (createDirectoryIfMissing True) (dataDirectory conf)
            absLogLoc <- makeAbsolute logLocation
            run port $ app absLogLoc watchConf

