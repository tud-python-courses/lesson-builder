module Common where



import           ClassyPrelude
import           Data.Aeson
import           LessonBuilder
import           Options.Applicative
import           System.Log.Formatter
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Logger


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


prepareLogger :: FilePath -> IO ()
prepareLogger targetFile = do
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
