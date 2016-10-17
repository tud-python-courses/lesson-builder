module Common where



import           ClassyPrelude
import           Data.Aeson
import           LessonBuilder
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
