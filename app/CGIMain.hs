{-# LANGUAGE RecordWildCards #-}
module Main where

import Network.CGI
import ClassyPrelude
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import LessonBuilder
import Control.Monad.Except
import Options.Applicative
import Data.Aeson
import System.Directory
import Common


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
                signature <- fmap B.pack <$> getVar "HTTP_SIGNATURE" 
                res <- liftIO $ runExceptT $ handleCommon logLocation conf body userAgent eventHeader signature
                case res of
                    Left err -> outputError 400 "Invalid Request" $ return $ BL.unpack err
                    Right v -> outputFPS v
