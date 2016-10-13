{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           ClassyPrelude
import           Data.Aeson
import           LessonBuilder
import           Network.Wai.Handler.Warp
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
                ++ help "location of the watch config (defaults to watch_conf.json)" 
                ++ value "watch_conf.json"
                )
        <*> option auto 
                (  long "port" 
                ++ short 'p' 
                ++ metavar "INTEGER" 
                ++ help "port to bind to (defaults to 8000)" 
                ++ value 8000
                )


main :: IO ()
main = do
    Opts{..} <- execParser optsParser
    raw <- readFile watchConf
    case eitherDecode raw of
        Left err -> putStrLn (pack err)
        Right conf -> run port $ app logLocation conf

