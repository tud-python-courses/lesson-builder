{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}
module LessonBuilder where


import           ClassyPrelude
import           Crypto.MAC.HMAC
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as B
import           Network.HTTP.Types.Header
import           Network.URI
import           Network.Wai
import           System.Logger.Log


type BuildConf = HashMap String Build


defaultDataDirectory :: FilePath
defaultDataDirectory = ".data"


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
    { dataDirectory :: FilePath
    , watched       :: [Watch]
    , secret        :: Maybe ByteString
    }

data Event a
    = Push
        { eventRepo :: Repo
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


let dropPrefix p t = fromMaybe t $ stripPrefix p t
    prefixOpts prefix = defaultOptions { fieldLabelModifier = camelTo2 '_' . dropPrefix prefix } in
    deriveJSON (prefixOpts "build") ''Build
    deriveJSON (prefixOpts "event") ''Event
    deriveJSON (prefixOpts "watch") ''Watch
    deriveJSON defaultOptions { fieldLabelModifier = camelTo2 '_' } ''WatchConf


buildIt :: Build -> String -> IO Bool
buildIt Build{command, targetDir, sourceDir} file = do
    exists <- do
        isDir <- doesDirectoryExist targetDir
        if isDir
            then return True
            else do
                isFile <- doesFileExist targetDir
                if isFile
                    then do
                        errorM "worker" $ printf "Target directory '%s' is file" targetDir
                        return False
                    else do
                        createDirectory targetDir
                        return True

    when exists $ do
        res <- lastMay <$> replicate repeats (readCreateProcessWithExitCode process "")
        case res of
            Nothing -> do
                errorM "worker" $ printf "Unexpected number of repeats: %d" repeats
                return False
            Just (ExitSuccess, _, _) -> return True
            Just (code, stdout, stderr) -> do
                errorM "worker" $ printf "Build failed with %d for command %s (%d repeats)" code command repeats
                errorM "worker" "------stderr------"
                errorM "worker" stderr
                errorM "worker" "------stdout------"
                errorM "worker" stdout
                return False
  where
    repeats = fromMaybe 1 $ lookup repeatsMap command
    process = proc command (["-output-directory", targetDir] ++ fromMaybe [] (lookup command additionalCommandOptions)) { cwd = Just sourceDir }


buildAll :: Build -> IO [Async Bool]
buildAll b = mapM (async . buildIt b) (files b)


verifyUAgent :: WatchConf -> ByteString -> Request -> IO Bool
verifyUAgent WatchConf{secret = Nothing} _ request = True
verifyUAgent WatchConf{secret = Just secret'} payload headers =
    case (,) <$> lookup "signature" (requestHeaders request) <*> requestHeaderUserAgent request of
        Just (signature, userAgent)
            | "Github-Hookshot/" `isPrefixOf` userAgent ->
                maybe (err "Uncomputable digest") (return . (== digestFromByteString signature)) computed
        Nothing -> do
            err "Missing header"
            return False
        _ -> do
            err "Wrong user agent"
            return False
  where
    err = errorM "verify"
    computed = hmacGetDigest $ hmac secret' payload :: Digest SHA1


handleEvent :: Event -> IO ()
handleEvent Ping{} = errorM "worker" "Ping event should not be handeled in worker"
handleEvent Push{} = return ()


app :: FilePath -> WatchConf -> Application
app logLocation watchConf request respond = do
    body <- lazyRequestBody request
    -- If the PING event does not send the signature this verification
    -- needs to be moved to after the payload has been parsed and the event type determined
    verified <- verifyUAgent watchConf body request
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
                    void $ async $ handleEvent p event
                    respond $ responseLBS ok200 [] $ "Hook received. For build results refer to the log at " ++ bsLogLoc
        else respond $ responseLBS badRequest400 [] $ "Unverifiable. For details on the error refer to the log at " ++ bsLogLoc
  where
    directory = fromMaybe defaultDataDirectory $ dataDirectory watchConf
    headers = requestHeaders request
    bsLogLoc = B.pack logLocation
