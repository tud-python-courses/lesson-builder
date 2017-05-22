{-|
Module      : $Header$
Description : The server.
Copyright   : (c) Justus Adam 2017.
License     : MIT
Maintainer  : dev@justus.science
Stability   : experimental
Portability : portable
-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Control.Concurrent.Async.Lifted
import           Control.Monad.Except
import           Control.Monad.Logger
import qualified Data.ByteString.Char8           as B
import qualified Data.ByteString.Lazy.Char8      as BL
import           Data.Monoid                     ((<>))
import qualified Data.Text.IO                    as T
import qualified Data.Text.Lazy                  as L
import qualified Data.Text.Lazy.Encoding         as L
import           Lens.Micro
import           LessonBuilder
import           LessonBuilder.Serialize
import           LessonBuilder.Types
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Options.Applicative
import           System.Directory


data Opts = Opts { watchConf   :: FilePath
                 , port        :: Int
                 , logLevel    :: LogLevel
                 }


optsParser :: ParserInfo Opts
optsParser = info (helper <*> struct) frame
  where
    frame = header "lesson-builder, a webhook endpoint" <> fullDesc
    struct = Opts
        <$> strOption
                (  long "watch-conf"
                <> short 'w'
                <> metavar "PATH"
                <> help "location of the watch config"
                <> showDefault
                <> value "watch_conf.json"
                )
        <*> option auto
                (  long "port"
                <> short 'p'
                <> metavar "INTEGER"
                <> help "port to bind to"
                <> showDefault
                <> value 8000
                )
        <*> (    flag' LevelDebug
                (  long "debug"
                <> help "log debug messages")
            <|> flag LevelError LevelWarn
                (  long "verbose"
                <> short 'v'
                <> help "log warnings in addition to errors")
            )



app :: FilePath -> LogLevel -> Application
app confLocation setLevel request respond =
    runStderrLoggingT $ filterLogger filterFunc $ do
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
                liftIO $ respond $ responseLBS badRequest400 [] (L.encodeUtf8 $ L.fromStrict err)
            Right (msg, action) -> do
                logInfoNS "server" "Responding okay"
                void $ async $ runExceptT action >>= either logErrorN return
                liftIO $ respond $ responseLBS ok200 [] (L.encodeUtf8 $ L.fromStrict msg)
  where
    signature = lookup "X-Hub-Signature" $ requestHeaders request
    getHeader h = maybe (throwError "Missing header") return $ lookup h $ requestHeaders request
    filterFunc _ level = level >= setLevel

main :: IO ()
main = do
    Opts{..} <- execParser optsParser
    raw <- readConf watchConf
    case raw of
        Left err -> T.putStrLn err
        Right conf -> do
            maybe (return ()) (createDirectoryIfMissing True) (conf^.dataDirectory)
            run port $ app watchConf logLevel

