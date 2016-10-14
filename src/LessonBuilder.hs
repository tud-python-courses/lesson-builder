{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}
module LessonBuilder where


import           ClassyPrelude              hiding (async)
import           Control.Concurrent.Async
import           Control.Monad.Except       (ExceptT (..), MonadError,
                                             runExceptT, throwError)
import           Crypto.Hash
import           Crypto.Hash.Algorithms
import           Crypto.MAC.HMAC
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Foldable              as F
import           Data.Vector                (Vector)
import           GHC.Generics
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Log.Logger
import           System.Process
import           Text.Printf


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
additionalCommandOptions = mapFromList
    [ (HtLatex, ["-halt-on-error", "-interaction=nonstopmode"])
    , (Pdflatex, ["-halt-on-error", "-interaction=nonstopmode"])
    ]


texOutExt :: HashMap Command String
texOutExt = mapFromList
    [ (HtLatex,  "html")
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

instance ToJSON Repo where
    toJSON Repo{repoName, repoId, apiUrl} = object ["full_name".=repoName, "id".=repoId, "url".=apiUrl]


let dropPrefix p t = fromMaybe t $ stripPrefix p t
    prefixOpts prefix = defaultOptions { fieldLabelModifier = camelTo2 '_' . dropPrefix prefix } in
    join <$> sequence
        [ deriveJSON (prefixOpts "commit") ''CommitData
        , deriveJSON (prefixOpts "include") ''Include
        , deriveJSON defaultOptions { constructorTagModifier = camelTo2 '_', sumEncoding = UntaggedValue } ''Command
        , deriveJSON (prefixOpts "build") ''Build
        , deriveJSON (prefixOpts "ping") ''Ping
        , deriveJSON (prefixOpts "push") ''Push
        , deriveJSON (prefixOpts "watch") ''Watch
        , deriveJSON defaultOptions { fieldLabelModifier = camelTo2 '_' } ''WatchConf
        ]


instance FromJSON BuildConf where
    parseJSON (Object o) = BuildConf
        <$> o .: "builds"
        <*> o .:? "includes" .!= mempty
    parseJSON _ = mzero


type LBuilder = ExceptT String IO


ensureTargetDir :: FilePath -> LBuilder ()
ensureTargetDir targetDir = do
    isDir <- liftIO $ doesDirectoryExist targetDir
    unless isDir $ do
        isFile <- liftIO $ doesFileExist targetDir
        if isFile
            then throwError $ printf "Target directory '%s' is file" targetDir
            else liftIO $ createDirectory targetDir


shellBuildWithRepeat :: Command -> CreateProcess -> Int -> LBuilder ()
shellBuildWithRepeat command process repeats = do
    res <- lastMay <$> (replicateM repeats (liftIO $ readCreateProcessWithExitCode process "") :: LBuilder [(ExitCode, String, String)])
    case res of
        Nothing -> throwError $ printf "Unexpected number of repeats: %d" repeats
        Just (ExitSuccess, _, _) -> return ()
        Just (ExitFailure i, stdout, stderr) -> do
            liftIO $ errorM "worker" $ printf "Build failed with %d for command %s (%d repeats)\n------stderr------\n%s------stdout------\n%s\n" i (show command) repeats stderr stdout
            throwError stderr


buildIt :: Build -> String -> LBuilder ()
buildIt Build{command, targetDir, sourceDir} file = do
    ensureTargetDir targetDir
    shellBuildWithRepeat command process repeats
  where
    commandStr = toLower $ show command
    repeats = 2
    process = (proc commandStr (["-output-directory", targetDir] ++ fromMaybe [] (lookup command additionalCommandOptions))) { cwd = Just sourceDir }


asyncBuilder :: LBuilder a -> LBuilder (Async (Either String a))
asyncBuilder = liftIO . async . runExceptT


waitBuilder :: Async (Either String a) -> LBuilder a
waitBuilder = ExceptT . wait


buildAll :: Build -> LBuilder (Vector (Async (Either String ())))
buildAll b = mapM (asyncBuilder . buildIt b) (files b)


waitForBuilders :: Foldable f => f (Async (Either String ())) -> LBuilder ()
waitForBuilders = F.traverse_ waitBuilder


runBuildersAndWait :: Traversable f => f (LBuilder ()) -> LBuilder ()
runBuildersAndWait = traverse asyncBuilder >=> waitForBuilders


verifyUAgent :: (MonadIO m, MonadError B.ByteString m)
             => WatchConf -> ByteString -> ByteString -> Maybe ByteString -> m ()
verifyUAgent WatchConf{secret = Nothing} _ _ _ = return ()
verifyUAgent WatchConf{secret = Just secret'} payload userAgent signature =
    if "Github-Hookshot/" `isPrefixOf` userAgent
        then case signature >>= digestFromByteString of
                    Nothing -> throwError "Uncomputable digest"
                    Just real | real == computed -> return ()
                    _ -> throwError "Digests do not match"
        else throwError "Wrong user agent"
  where
    computed = hmacGetDigest $ hmac (BS.pack secret') payload :: Digest SHA1


gitRefresh :: String -> FilePath -> LBuilder ()
gitRefresh url targetDir = do
    exists <- liftIO $ doesDirectoryExist targetDir
    let process = if exists
                  then proc "git" ["-C", targetDir, "pull"]
                  else proc "git" ["clone", url, targetDir]
    (code, stdout, stderr) <- liftIO $ readCreateProcessWithExitCode process ""
    case code of
        ExitSuccess -> return ()
        ExitFailure c -> throwError $ "Git process failed with" ++ stdout ++ "\n" ++ stderr


makeInclude :: Include -> LBuilder ()
makeInclude Include{includeRepository, includeDirectory} = do
    gitRefresh includeRepository includeDirectory
    buildProject includeDirectory


buildProject :: FilePath -> LBuilder ()
buildProject directory = do
    raw <- readFile $ directory </> buildConfigName
    buildConf <- either (throwError . ("Unreadable build configuration: " ++)) return $ eitherDecode raw

    let relativeIncludes = map (\i -> i {includeDirectory = directory </> includeDirectory i}) (includes buildConf)

    runBuildersAndWait $ map makeInclude relativeIncludes

    let relativeBuilds = map (relativize . snd) $ fromList $ mapToList (builds buildConf)
          where relativize build = build { sourceDir = directory </> sourceDir build
                                         , targetDir = directory </> targetDir build }

    traverse buildAll relativeBuilds >>= waitForBuilders . join
    return ()


repoToUrl :: Repo -> String
    -- TODO Use host
repoToUrl Repo{repoName} = "https://github.com/" ++ repoName


handleEvent :: WatchConf -> Event -> IO ()
handleEvent WatchConf{watched, reposDirectory} = handle
  where
    handle (PingEvent Ping{}) = errorM "worker" "Ping event should not be handeled in worker"
    handle (PushEvent Push{pushRepository, pushHeadCommit}) = do
        res <- runExceptT $
                    -- TODO handle special events (push to own repo)
                    case lookup (repoName pushRepository) watchMap of
                        Nothing -> throwError "Unrecognized repository"
                        _ | any (`isInfixOf` commitMessage pushHeadCommit) skipStrings -> do
                            liftIO $ infoM "worker" "skipping commit due to skip message"
                            return ()
                        Just Watch{directory} -> do
                            wd <- liftIO getCurrentDirectory
                            makeInclude Include
                                { includeDirectory = wd </> fromMaybe "." reposDirectory </> directory
                                , includeRepository = repoToUrl pushRepository
                                }

        either (errorM "worker") return res
    watchMap = mapFromList $ map (watchName &&& id) $ toList watched :: HashMap String Watch


handleCommon :: MonadIO m
             => FilePath -- ^ The location of the log
             -> WatchConf
             -> B.ByteString -- ^ Request body
             -> ByteString -- ^ User Agent string
             -> ByteString -- ^ Event type
             -> Maybe ByteString -- ^ Signature
             -> ExceptT B.ByteString m B.ByteString
handleCommon logLocation watchConf body userAgent eventHeader signature = do
    verifyUAgent watchConf (toStrict body) userAgent signature
    let ev = case eventHeader of
                "push" -> PushEvent <$> eitherDecode body
                "ping" -> PingEvent <$> eitherDecode body
                a -> Left $ "unknown event type " ++ BS.unpack a
    case ev of
        Left err -> do
            liftIO $ errorM "receiver" "Unparseable json"
            liftIO $ errorM "receiver" err
            throwError $ "Unparseable json. For details on the error refer to the log at " ++ bsLogLoc
        Right (PingEvent Ping{hookId, hook}) -> do
            liftIO $ infoM "receiver" "Ping received"
            let fileName = printf "hook_%d.conf.json" hookId
                filePath = directory </> fileName
            isDir <- liftIO $ doesDirectoryExist filePath
            exists <- liftIO $ doesFileExist filePath
            if isDir
                then do
                    liftIO $ errorM "receiver" "Hook config location is directory"
                    throwError $ "Ping error. For error details refer top the log at " ++ bsLogLoc
                else do
                    liftIO $ writeFile filePath $ encode hook
                    return $ "Ping received. Data saved in " ++ B.pack filePath
        Right event -> do
            void $ liftIO $ async $ handleEvent watchConf event
            return $ "Hook received. For build results refer to the log at " ++ bsLogLoc
  where
    directory = fromMaybe defaultDataDirectory $ dataDirectory watchConf
    bsLogLoc = B.pack logLocation

