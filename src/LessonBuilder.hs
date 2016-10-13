{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies #-}
module LessonBuilder where


import           ClassyPrelude              hiding (async)
import           Control.Concurrent.Async
import           Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import           Crypto.MAC.HMAC
import Crypto.Hash.Algorithms
import Crypto.Hash
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BS
import           Network.HTTP.Types
import           Network.HTTP.Types.Header
import           Network.URI
import           Network.Wai
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Log.Logger
import           System.Process
import           Text.Printf
import qualified Data.Foldable as F


data Include = Include
    { includeRepository :: String
    , includeDirectory  :: FilePath
    }


data BuildConf = BuildConf
    { builds   :: HashMap String Build
    , includes :: [Include]
    }


defaultDataDirectory :: FilePath
defaultDataDirectory = ".data"


buildConfigName :: FilePath
buildConfigName = "build_conf.json"


skipStrings :: [String]
skipStrings = ["[skip build]", "[build skip]"]


additionalCommandOptions :: HashMap String [String]
additionalCommandOptions = mapFromList
    [ ("htlatex", ["-halt-on-error", "-interaction=nonstopmode"])
    , ("pdflatex", ["-halt-on-error", "-interaction=nonstopmode"])
    ]


texOutExt :: HashMap String String
texOutExt = mapFromList
    [ ("htlatex",  "html")
    , ("pdflatex", "pdf")
    , ("xelatex", "pdf")
    , ("hevea", "html")
    ]


repeatsMap :: HashMap String Int
repeatsMap = mapFromList
    [ ("htlatex", 2)
    , ("pdflatex", 2)
    , ("xelatex", 2)
    , ("hevea", 2)
    ]


data Build = Build
    { buildName :: String
    , command   :: String
    , targetDir :: FilePath
    , sourceDir :: FilePath
    , files     :: [FilePath]
    }


data Watch = Watch
    { watchName :: String
    , directory :: FilePath
    }


data WatchConf = WatchConf
    { dataDirectory  :: Maybe FilePath
    , watched        :: [Watch]
    , secret         :: Maybe String
    , reposDirectory :: FilePath
    }


data CommitData = CommitData { commitMessage :: String }


data Event
    = Push
        { eventRepo       :: Repo
        , eventHeadCommit :: CommitData
        }
    | Ping
        { eventRepo :: Repo
        , hookId    :: Int
        , hook      :: Value
        }


data Repo = Repo
    { repoName :: String
    , repoId   :: String
    , apiUrl   :: String
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
        , deriveJSON (prefixOpts "build") ''Build
        , deriveJSON (prefixOpts "buildConf") ''BuildConf
        , deriveJSON (prefixOpts "event") ''Event
        , deriveJSON (prefixOpts "watch") ''Watch
        , deriveJSON defaultOptions { fieldLabelModifier = camelTo2 '_' } ''WatchConf
        ]


type LBuilder = ExceptT String IO


buildIt :: Build -> String -> LBuilder ()
buildIt Build{command, targetDir, sourceDir} file = do
    isDir <- liftIO $ doesDirectoryExist targetDir
    unless isDir $ do
        isFile <- liftIO $ doesFileExist targetDir
        if isFile
            then throwError $ printf "Target directory '%s' is file" targetDir
            else liftIO $ createDirectory targetDir

    res <- lastMay <$> (replicateM repeats (liftIO $ readCreateProcessWithExitCode process "") :: LBuilder [(ExitCode, String, String)])
    case res of
        Nothing -> throwError $ printf "Unexpected number of repeats: %d" repeats
        Just (ExitSuccess, _, _) -> return ()
        Just (ExitFailure i, stdout, stderr) -> do
            liftIO $ errorM "worker" $ printf "Build failed with %d for command %s (%d repeats)\n------stderr------\n%s------stdout------\n%s\n" i command repeats stderr stdout
            throwError $ stderr
  where
    repeats = fromMaybe 1 $ lookup command repeatsMap
    process = (proc command (["-output-directory", targetDir] ++ fromMaybe [] (lookup command additionalCommandOptions))) { cwd = Just sourceDir }


asyncBuilder :: LBuilder a -> LBuilder (Async (Either String a))
asyncBuilder = liftIO . async . runExceptT


waitBuilder :: Async (Either String a) -> LBuilder a
waitBuilder = ExceptT . wait 


buildAll :: Build -> LBuilder [Async (Either String ())]
buildAll b = mapM (asyncBuilder . buildIt b) (files b)


waitForBuilders :: Foldable f => f (Async (Either String ())) -> LBuilder ()
waitForBuilders = F.traverse_ waitBuilder


runBuildersAndWait :: Traversable f => f (LBuilder ()) -> LBuilder ()
runBuildersAndWait = traverse asyncBuilder >=> waitForBuilders


verifyUAgent :: WatchConf -> ByteString -> Request -> IO Bool
verifyUAgent WatchConf{secret = Nothing} _ request = return True
verifyUAgent WatchConf{secret = Just secret'} payload request =
    case (,) <$> lookup "signature" (requestHeaders request) <*> requestHeaderUserAgent request of
        Just (signature, userAgent)
            | "Github-Hookshot/" `isPrefixOf` userAgent ->
                case digestFromByteString signature of
                    Nothing -> err "Uncomputable digest"
                    Just expected | expected == computed -> return True
                    _ -> err "Digests do not match"
        Nothing -> err "Missing header"
        _ -> err "Wrong user agent"
  where
    err msg = errorM "verification" msg >> return False
    computed = hmacGetDigest $ hmac (BS.pack secret') payload :: Digest SHA1


gitRefresh :: String -> FilePath -> LBuilder ()
gitRefresh url targetDir = do
    exists <- liftIO $ doesDirectoryExist targetDir
    let process = if exists
                  then proc "git" ["clone", url, targetDir]
                  else proc "git" ["-C", targetDir, "pull"]
    (code, stdout, stderr) <- liftIO $ readCreateProcessWithExitCode process ""
    case code of
        ExitSuccess -> return ()
        ExitFailure c -> throwError $ "Git process failed with" ++ stdout


makeInclude :: Include -> LBuilder ()
makeInclude Include{includeRepository, includeDirectory} = do
    gitRefresh includeRepository includeDirectory
    buildProject includeDirectory


buildProject :: FilePath -> LBuilder ()
buildProject directory = do
    raw <- readFile $ directory </> buildConfigName
    buildConf <- either (const $ throwError "Unreadable build configuration") return $ eitherDecode raw

    let relativeIncludes = map (\i -> i {includeDirectory = directory </> includeDirectory i}) (includes buildConf)

    runBuildersAndWait $ map makeInclude relativeIncludes

    let relativeBuilds = map (relativize . snd) $ mapToList (builds buildConf)
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
    handle Ping{} = errorM "worker" "Ping event should not be handeled in worker"
    handle Push{eventRepo, eventHeadCommit} = do
        res <- runExceptT $
                    -- TODO handle special events (push to own repo)
                    case lookup (repoName eventRepo) watchMap of
                        Nothing -> throwError "Unrecognized repository"
                        _ | any (`isInfixOf` commitMessage eventHeadCommit) skipStrings -> do
                            liftIO $ infoM "worker" "skipping commit due to skip message"
                            return ()
                        Just Watch{directory} -> do
                            wd <- liftIO $ getCurrentDirectory
                            makeInclude $ Include 
                                { includeDirectory = wd </> reposDirectory </> directory
                                , includeRepository = repoToUrl eventRepo
                                }

        either (errorM "worker") return res
    watchMap = mapFromList $ map (watchName &&& id) watched :: HashMap String Watch


app :: FilePath -> WatchConf -> Application
app logLocation watchConf request respond = do
    body <- lazyRequestBody request
    -- If the PING event does not send the signature this verification
    -- needs to be moved to after the payload has been parsed and the event type determined
    verified <- verifyUAgent watchConf (toStrict body) request
    if verified
        then
            case eitherDecode' body of
                Left err -> do
                    errorM "receiver" "Unparseable json"
                    errorM "receiver" err
                    respond $ responseLBS badRequest400 [] $ "Unparseable json. For details on the error refer to the log at " ++ bsLogLoc
                Right Ping{hookId, hook} -> do
                    infoM "receiver" "Ping received"
                    let fileName = printf "hook_%d.conf.json" hookId
                        filePath = directory </> fileName
                    isDir <- doesDirectoryExist filePath
                    exists <- doesFileExist filePath
                    if isDir
                        then do
                            errorM "receiver" "Hook config location is directory"
                            respond $ responseLBS badRequest400 [] $ "Ping error. For error details refer top the log at " ++ bsLogLoc
                        else do
                            writeFile filePath $ encode hook
                            respond $ responseLBS ok200 [] $ "Ping received. Data saved in " ++ B.pack filePath
                Right event -> do
                    void $ async $ handleEvent watchConf event
                    respond $ responseLBS ok200 [] $ "Hook received. For build results refer to the log at " ++ bsLogLoc
        else respond $ responseLBS badRequest400 [] $ "Unverifiable. For details on the error refer to the log at " ++ bsLogLoc
  where
    directory = fromMaybe defaultDataDirectory $ dataDirectory watchConf
    headers = requestHeaders request
    bsLogLoc = B.pack logLocation
