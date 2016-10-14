{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
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



app :: FilePath -> WatchConf -> Application
app logLocation watchConf request respond = do
    body <- lazyRequestBody request
    res <- runExceptT $ do
        eventHeader <- getHeader "X-GitHub-Event"
        userAgent <- maybe (throwError "No user agent found") return $ requestHeaderUserAgent request
        handleCommon logLocation watchConf body userAgent eventHeader signature
    case res of
        Left err -> respond $ responseLBS badRequest400 [] err
        Right v -> respond $ responseLBS ok200 [] v
  where
    signature = lookup "Signature" $ requestHeaders request
    getHeader h = maybe (throwError "Missing header") return $ lookup h $ requestHeaders request

-- TODO configure log
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
            run port $ app absLogLoc conf

