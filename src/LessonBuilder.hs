{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}
module LessonBuilder where


import           ClassyPrelude              hiding (async)
import           Control.Concurrent.Async.Lifted
import           Control.Monad.Except       (ExceptT (..), MonadError,
                                             runExceptT, throwError)
import           Crypto.Hash
import           Crypto.MAC.HMAC
import           Data.Aeson as Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Foldable              as F
import           Data.Vector                (Vector)
import           System.Directory
import           System.Exit
import           System.FilePath
import Control.Monad.Logger
import           System.Process
import           Text.Printf
import qualified Data.Yaml as Yaml
import qualified System.Posix.Process as Proc
import Marvin.Interpolate.All


data Include = Include
    { includeRepository :: !String
    , includeDirectory  :: !FilePath
    }


data BuildConf = BuildConf
    { builds   :: !(HashMap String Build)
    , includes :: !(Vector Include)
    }


defaultDataDirectory :: FilePath
defaultDataDirectory = ".data"


buildConfigName :: FilePath
buildConfigName = "build_conf.json"


skipStrings :: [String]
skipStrings = ["[skip build]", "[build skip]"]


additionalCommandOptions :: HashMap Command [String]
additionalCommandOptions =
    [ (HtLatex , ["-halt-on-error", "-interaction=nonstopmode"])
    , (Pdflatex, ["-halt-on-error", "-interaction=nonstopmode"])
    ]


texOutExt :: HashMap Command String
texOutExt =
    [ (HtLatex, "html")
    , (Pdflatex, "pdf")
    , (Xelatex, "pdf")
    , (Hevea, "html")
    ]


data Command
    = HtLatex
    | Pdflatex
    | Hevea
    | Xelatex
    | Latexmk
    deriving (Show, Eq, Ord, Generic, Hashable)


instance ToJSON Command where
    toJSON = String . toLower . pack . show


instance FromJSON Command where
    parseJSON (String "htlatex") = return HtLatex
    parseJSON (String "pdflatex") = return Pdflatex
    parseJSON (String "hevea") = return Hevea
    parseJSON (String "xelatex") = return Xelatex
    parseJSON (String "latexmk") = return Latexmk
    parseJSON _ = mzero


data Build = Build
    { command   :: !Command
    , targetDir :: !FilePath
    , sourceDir :: !FilePath
    , files     :: !(Vector FilePath)
    }


data Watch = Watch
    { watchName :: !String
    , directory :: !FilePath
    }


data WatchConf = WatchConf
    { dataDirectory  :: !(Maybe FilePath)
    , watched        :: !(Vector Watch)
    , secret         :: !(Maybe String)
    , reposDirectory :: !(Maybe FilePath)
    }


data CommitData = CommitData { commitMessage :: !String }


data Event
    = PushEvent !Push
    | PingEvent !Ping


data Push = Push
    { pushRepository :: !Repo
    , pushHeadCommit :: !CommitData
    }


data Ping = Ping
    { pingRepository :: !Repo
    , hookId         :: !Int
    , hook           :: !Value
    }


data Repo = Repo
    { repoName :: !String
    , repoId   :: !Int
    , apiUrl   :: !String
    }

instance FromJSON Repo where
    parseJSON (Object o) =
        Repo
            <$> o .: "full_name"
            <*> o .: "id"
            <*> o .: "url"
    parseJSON _ = mzero

instance ToJSON Repo where
    toJSON Repo{repoName, repoId, apiUrl} = object ["full_name".=repoName, "id".=repoId, "url".=apiUrl]


let dropPrefix p t = fromMaybe t $ stripPrefix p t
    prefixOpts prefix = defaultOptions { fieldLabelModifier = camelTo2 '_' . dropPrefix prefix } in
    join <$> sequence
        [ deriveJSON (prefixOpts "commit" ) ''CommitData
        , deriveJSON (prefixOpts "include") ''Include
        , deriveJSON (prefixOpts "build"  ) ''Build
        , deriveJSON (prefixOpts "ping"   ) ''Ping
        , deriveJSON (prefixOpts "push"   ) ''Push
        , deriveJSON (prefixOpts "watch"  ) ''Watch
        , deriveJSON defaultOptions { fieldLabelModifier = camelTo2 '_' } ''WatchConf
        ]


instance FromJSON BuildConf where
    parseJSON (Object o) = BuildConf
        <$> o .: "builds"
        <*> o .:? "includes" .!= mempty
    parseJSON _ = mzero


type LBuilder = ExceptT Text (LoggingT IO)


mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left err) = Left $ f err
mapLeft _ (Right v) = Right v


ensureTargetDir :: FilePath -> LBuilder ()
ensureTargetDir = liftIO . createDirectoryIfMissing True


shellBuildWithRepeat :: Command -> CreateProcess -> Int -> LBuilder ()
shellBuildWithRepeat command process repeats = do
    res <- lastMay <$> (replicateM repeats (liftIO $ readCreateProcessWithExitCode process "") :: LBuilder [(ExitCode, String, String)])
    case res of
        Nothing -> throwError $(isT "Unexpected number of repeats: #{repeats}")
        Just (ExitSuccess, _, _) -> logDebugNS "build" $(isT "Successfully executed #{command}")
        Just (ExitFailure i, _, _) -> do
            let str = $(isT "Build failed with #{i} for command #{command} (#{repeats} repeats)")
            logErrorNS "worker" str
            throwError str


outputToDirectory :: Build -> FilePath -> [String]
outputToDirectory Build{command=Hevea, targetDir} file = ["-o", targetDir </> file -<.> "html"]
outputToDirectory Build{targetDir} _ = ["-output-directory", targetDir]


buildIt :: FilePath -> Build -> String -> LBuilder ()
buildIt wd b@Build{command, targetDir, sourceDir} file = do
    logDebugNS "worker" $(isT "Executing #{command} in #{sourceDir} to #{targetDir}")
    absTargetDir <- liftIO $ makeAbsolute targetDir
    let process = (proc commandStr (outputToDirectory b { targetDir = absTargetDir } file ++ fromMaybe [] (lookup command additionalCommandOptions) ++ [file])) { cwd = Just $ wd </> sourceDir }
    shellBuildWithRepeat command process repeats
  where
    commandStr = toLower $ show command
    repeats = 2


buildAll :: FilePath -> Build -> LBuilder (Vector (Async (Either Text ())))
buildAll wd b@Build{targetDir} = do
    logDebugNS "worker" $(isT "Checking target directory #{targetDir}")
    ensureTargetDir (wd </> targetDir)
    mapM (async . buildIt wd b) (files b)


waitForBuilders :: Foldable f => f (Async (Either Text ())) -> LBuilder ()
waitForBuilders = F.traverse_ wait


runBuildersAndWait :: Traversable f => f (LBuilder ()) -> LBuilder ()
runBuildersAndWait = traverse async >=> waitForBuilders


verifyUAgent :: (MonadIO m, MonadError Text m, MonadLogger m)
             => WatchConf -> ByteString -> ByteString -> Maybe String -> m ()
verifyUAgent WatchConf{secret = Nothing} _ _ _ = return ()
verifyUAgent _ _ _ Nothing = do
    logErrorNS "verify" "Missing signature"
    throwError "Missing signature"
verifyUAgent WatchConf{secret = Just secret'} payload userAgent (Just signature) = do
    unless ("GitHub-Hookshot/" `isPrefixOf` userAgent) $ do
        logErrorNS "verify" $ $(isT "Weird user agent #{BS.unpack userAgent}")
        throwError "Wrong user agent"
    unless (sig == show computed) $ do
        logErrorNS "verify" "Digests do not match"
        throwError "Digests do not match"
  where
    sig = fromMaybe sig $ stripPrefix "sha1=" signature
    computed = hmacGetDigest $ hmac (BS.pack secret') payload :: Digest SHA1


gitRefresh :: String -> FilePath -> LBuilder ()
gitRefresh url targetDir = do
    exists <- liftIO $ doesDirectoryExist targetDir
    let process = if exists
                    then proc "git" ["-C", targetDir, "pull"]
                    else proc "git" ["clone", url, targetDir]
    (code, stdout, stderr) <- liftIO $ readCreateProcessWithExitCode process ""
    case code of
        ExitSuccess -> logDebugNS "worker" $(isT "git #{if exists then \"pull\" :: Text else \"clone\"} succeeded")
        ExitFailure _ -> do
            logDebugNS "worker" "git failed" 
            throwError $(isT "Git process failed with \n#{stdout}\n#{stderr}")


makeInclude :: Include -> LBuilder ()
makeInclude Include{includeRepository, includeDirectory} = do
    abs <- liftIO $ makeAbsolute includeDirectory
    logDebugNS "worker" $(isT "Refreshing repository in #{abs}")
    gitRefresh includeRepository includeDirectory
    logDebugNS "worker" "Building project"
    buildProject includeDirectory


buildProject :: FilePath -> LBuilder ()
buildProject directory = do
    raw <- readFile $ directory </> buildConfigName
    BuildConf{includes, builds} <- either (\err -> throwError $(isT "Unreadable build configuration: #{err}")) return $ eitherDecode raw

    let relativeIncludes = map (\i -> i {includeDirectory = directory </> includeDirectory i}) includes

    logDebugNS "worker" $(isT "Found #{length relativeIncludes} includes")

    runBuildersAndWait $ map makeInclude relativeIncludes

    let relativeBuilds = map snd $ fromList $ mapToList builds

    logDebugNS "worker" $(isT "Found #{length relativeBuilds} builds")
    logDebugNS "worker" $(isT "Found #{length (relativeBuilds >>= files)} files")

    started <- traverse (buildAll directory) relativeBuilds

    logDebugNS "worker" "Builds started"

    waitForBuilders $ join started

    logDebugNS "worker" "Builds finished"

    return ()


repoToUrl :: Repo -> String
    -- TODO Use host
repoToUrl Repo{repoName} = "https://github.com/" ++ repoName


handleEvent :: WatchConf -> Event -> LBuilder ()
handleEvent WatchConf{watched, reposDirectory} = handle
  where
    handle (PingEvent Ping{}) = logErrorNS "worker" "Ping event should not be handeled in worker"
    handle (PushEvent Push{pushRepository, pushHeadCommit}) =
        flip catch onExcept $
            -- TODO handle special events (push to own repo)
            case lookup (repoName pushRepository) watchMap of
                Nothing -> throwError "Unrecognized repository"
                _ | any (`isInfixOf` commitMessage pushHeadCommit) skipStrings -> do
                    logInfoNS "worker" "skipping commit due to skip message"
                    return ()
                Just Watch{directory} -> do
                    wd <- liftIO getCurrentDirectory
                    logDebugNS "worker" $(isT "Found watch targeting directory #{directory}")
                    makeInclude Include
                        { includeDirectory = wd </> fromMaybe "." reposDirectory </> directory
                        , includeRepository = repoToUrl pushRepository
                        }

    watchMap :: HashMap String Watch
    watchMap = mapFromList $ map (watchName &&& id) $ toList watched
    onExcept e = throwError $(isT "#{e :: SomeException}")


readConf :: MonadIO m => FilePath -> m (Either Text WatchConf)
readConf fp = mapLeft pack . reader <$> liftIO (readFile fp)
  where
    reader
        | takeExtension fp `elem` ([".yaml", ".yml"] :: [String]) = Aeson.eitherDecode
        | takeExtension fp == ".json" = Yaml.decodeEither . toStrict
        | otherwise = const $ Left "Unknown Extension"



handleCommon ::WatchConf
             -> B.ByteString -- ^ Request body
             -> ByteString -- ^ User Agent string
             -> ByteString -- ^ Event type
             -> Maybe String -- ^ Signature
             -> LBuilder Text
handleCommon watchConf body userAgent eventHeader signature = do
    verifyUAgent watchConf (toStrict body) userAgent signature
    logDebugNS "receiver" "Verification successful"
    let ev = case eventHeader of
                "push" -> PushEvent <$> eitherDecode body
                "ping" -> PingEvent <$> eitherDecode body
                a -> Left $ "unknown event type " ++ BS.unpack a
    case ev of
        Left err -> do
            logErrorNS "receiver" "Unparseable json"
            logErrorNS "receiver" $(isT "#{err}")
            throwError $(isT "Unparseable json. For details on the error refer to the log.")
        Right (PingEvent Ping{hookId, hook}) -> do
            logInfoNS "receiver" "Ping received"
            let fileName = $(isS "hook_#{hookId}.conf.json")
                filePath = directory </> fileName
            isDir <- liftIO $ doesDirectoryExist filePath
            exists <- liftIO $ doesFileExist filePath
            if isDir
                then do
                    logErrorNS "receiver" "Hook config location is directory"
                    throwError $(isT "Ping error. For error details refer top the log.")
                else do
                    when exists $ logInfoNS "receiver" "Identically named hook config present, overwriting"
                    liftIO $ writeFile filePath $ encode hook
                    return $(isT "Ping received. Data saved in #{filePath}")
        Right event -> do
            logDebugNS "receiver" "Event parsed, starting execution"
            void $ async $ handleEvent watchConf event
            return $(isT "Hook received. For build results refer to the log.")
  where
    directory = fromMaybe defaultDataDirectory $ dataDirectory watchConf

