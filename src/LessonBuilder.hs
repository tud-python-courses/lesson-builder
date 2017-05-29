{-|
Module      : $Header$
Description : The build logic. Async and hook verification.
Copyright   : (c) Justus Adam 2017.
License     : MIT
Maintainer  : dev@justus.science
Stability   : experimental
§-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module LessonBuilder where


import           Control.Arrow                   ((&&&))
import           Control.Concurrent.Async.Lifted
import           Control.Monad.Except            (ExceptT (..), MonadError,
                                                  runExceptT, throwError)
import           Control.Monad.Logger
import           Crypto.Hash
import           Crypto.MAC.HMAC

import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8           as BS
import qualified Data.ByteString.Lazy.Char8      as B
import           Data.Char
import           Data.Foldable                   (toList)
import qualified Data.Foldable                   as F
import           Data.Hashable
import           Data.HashMap.Strict             (HashMap)
import qualified Data.HashMap.Strict             as HM
import           Data.List                       (isInfixOf, isPrefixOf,
                                                  stripPrefix)
import           Data.Maybe                      (fromMaybe)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Text.Format.Simple
import qualified Data.Vector                     as V
import           GHC.Generics
import           Lens.Micro.Platform
import           LessonBuilder.Serialize
import           LessonBuilder.Types
import           Marvin.Interpolate.All
import           System.Directory
import           System.Exit
import           System.FilePath
import qualified System.Posix.Process            as Proc
import           System.Process
import           Text.Printf
import           Util


defaultDataDirectory :: FilePath
defaultDataDirectory = ".builder-data"


defaultBuildConfigName :: FilePath
defaultBuildConfigName = "build_conf.json"


skipStrings :: [String]
skipStrings = ["[skip build]", "[build skip]"]


additionalCommandOptions :: Command -> [String]
additionalCommandOptions HtLatex = ["-halt-on-error", "-interaction=nonstopmode"]
additionalCommandOptions Pdflatex = ["-halt-on-error", "-interaction=nonstopmode"]
additionalCommandOptions _ = []


-- texOutExt :: Command -> Maybe String
-- texOutExt (HtLatex _) = Just "html"
-- texOutExt (Pdflatex _) = Just "pdf"
-- texOutExt (Xelatex _) = Just "pdf"
-- texOutExt (Hevea _) = Just "html"
-- texOutExt _ = Nothing


type LBuilder = ExceptT Text (LoggingT IO)


ensureTargetDir :: FilePath -> LBuilder ()
ensureTargetDir = liftIO . createDirectoryIfMissing True


shellBuildWithRepeat :: Command -> CreateProcess -> Int -> LBuilder ()
shellBuildWithRepeat command process repeats = do
    res <- replicateM repeats (liftIO $ readCreateProcessWithExitCode process "") `catch` \e -> throwError $(isT "IOException: #{displayException (e :: IOException)}")
    case res ^? _last of
        Nothing -> throwError $(isT "Unexpected number of repeats: #{repeats}")
        Just (ExitSuccess, _, _) -> logDebugNS "build" $(isT "Successfully executed #{command}")
        Just (ExitFailure i, stdout, stderr) -> do
            let str = $(isT "Build failed with #{i} for command #{command}   (#{repeats} repeats)")
            logErrorNS "worker" str
            logErrorNS "worker" (stderr^.packed)
            logDebugNS "worker" (stdout^.packed)


outputToDirectory :: Build -> FilePath -> [String]
outputToDirectory b@Build{buildCommand=Hevea} file = ["-o", b ^. targetDir </> file -<.> "html"]
outputToDirectory Build{buildCommand=Custom _} _ = []
outputToDirectory b _ = ["-output-directory", b ^. targetDir]


repeatsFromCommand :: Command -> Int
repeatsFromCommand HtLatex  = 2
repeatsFromCommand Pdflatex = 2
repeatsFromCommand Hevea    = 2
repeatsFromCommand Xelatex  = 2
repeatsFromCommand _        = 1


buildIt :: FilePath -> Build -> String -> LBuilder ()
buildIt wd b file = do
    logDebugNS "worker" $(isT "Executing #{b^.command} in #{b^.sourceDir} to #{b^.targetDir}")
    absTargetDir <- liftIO $ makeAbsolute $ wd </> b^.targetDir
    logDebugNS "worker" $(isT "Building with target dir #{absTargetDir}")
    let process = proc (commandStr ^. unpacked)
            $ outputToDirectory (b & targetDir .~ absTargetDir) file
            ++ additionalCommandOptions (b^.command) ++ [file]
            ++ toList (fmap (^.unpacked) otherArgs)
    shellBuildWithRepeat (b^.command) (process { cwd = Just $ wd </> b^.sourceDir }) repeats_
  where
    commandStr = commandToStr $ b^.command
    repeats_ = fromMaybe (repeatsFromCommand $ b^.command) $ b^.repeats
    lookupFunc "targetDir"        = Just $ b^.targetDir.packed
    lookupFunc "sourceDir"        = Just $ b^.sourceDir.packed
    lookupFunc "file"             = Just $ file^.packed
    lookupFunc "workingDirectory" = Just $ wd^.packed
    lookupFunc _                  = Nothing
    otherArgs = fmap f $ b^.args
    f arg = either (const arg) fst $ format arg lookupFunc


buildAll :: FilePath -> Build -> LBuilder ()
buildAll wd b = do
    logDebugNS "worker" $(isT "Checking target directory #{b^.targetDir}")
    ensureTargetDir (wd </> b^.targetDir)
    mapConcurrently_ (buildIt wd b) (b ^. files)


verifyUAgent :: (MonadIO m, MonadError Text m, MonadLogger m)
             => WatchConf -> BS.ByteString -> BS.ByteString -> Maybe String -> m ()
verifyUAgent WatchConf{watchConfSecret = Nothing} _ _ _ = return ()
verifyUAgent _ _ _ Nothing = do
    logErrorNS "verify" "Missing signature"
    throwError "Missing signature"
verifyUAgent WatchConf{watchConfSecret = Just secret'} payload userAgent (Just signature) = do
    unless ("GitHub-Hookshot/" `BS.isPrefixOf` userAgent) $ do
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
        ExitSuccess -> logDebugNS "worker" $(is "git #{if exists then \"pull\" else \"clone\"} succeeded")
        ExitFailure _ -> do
            logDebugNS "worker" "git failed"
            throwError $(isT "Git process failed with \n#{stdout}\n#{stderr}")


makeInclude :: Include -> LBuilder ()
makeInclude include = do
    abs <- liftIO $ makeAbsolute $ include^.directory
    logDebugNS "worker" $(isT "Refreshing repository in #{abs}")
    gitRefresh (include^.repository) (include^.directory)
    logDebugNS "worker" "Building project"
    buildProject (include^.directory) (fromMaybe defaultBuildConfigName $ include^.configFile)


buildProject :: FilePath -> FilePath -> LBuilder ()
buildProject directory_ buildConfigName = do
    bc <- either (\err -> throwError $(isT "Unreadable build configuration: #{err}")) return =<< readBuildConfig (directory_ </> buildConfigName)

    let relativeIncludes = fmap (directory %~ (directory_ </>)) (bc^.includes)

    logDebugNS "worker" $(isT "Found #{length relativeIncludes} includes")

    mapConcurrently_ makeInclude relativeIncludes

    let relativeBuilds = fmap snd $ V.fromList $ HM.toList (bc^.builds)

    logDebugNS "worker" $(isT "Found #{length relativeBuilds} builds")
    logDebugNS "worker" $(isT "Found #{length (relativeBuilds >>= (^.files))} files")

    logDebugNS "worker" "Starting builds"

    mapConcurrently_ (buildAll directory_) relativeBuilds


repoToUrl :: Repo -> String
    -- TODO Use host
repoToUrl Repo{repoName} = "https://github.com/" ++ repoName


handleEvent :: WatchConf -> Event -> LBuilder ()
handleEvent wc = handle
  where
    handle (PingEvent Ping{}) = logErrorNS "worker" "Ping event should not be handeled in worker"
    handle (PushEvent push) =
        flip catch onExcept $
            -- TODO handle special events (push to own repo)
            case watchMap ^? ix (push ^. repository . name) of
                Nothing -> throwError "Unrecognized repository"
                _ | any (`isInfixOf` (push ^. headCommit . message)) skipStrings -> do
                    logInfoNS "worker" "skipping commit due to skip message"
                Just w -> do
                    wd <- liftIO getCurrentDirectory
                    logDebugNS "worker" $(isT "Found watch targeting directory #{w^.directory}")
                    makeInclude Include
                        { includeDirectory = wd </> fromMaybe "." (wc^.reposDirectory) </> (w^.directory)
                        , includeRepository = repoToUrl (push^.repository)
                        , includeConfigFile = w^.configFile
                        }

    watchMap :: HashMap String Watch
    watchMap = HM.fromList $ map (watchName &&& id) $ V.toList (wc^.watched)
    onExcept e = throwError $(isT "#{e :: SomeException}")



handleCommon :: WatchConf
             -> B.ByteString -- ^ Request body
             -> BS.ByteString -- ^ User Agent string
             -> BS.ByteString -- ^ Event type
             -> Maybe String -- ^ Signature
             -> LBuilder (Text, LBuilder ())
handleCommon watchConf body userAgent eventHeader signature = do
    verifyUAgent watchConf (B.toStrict body) userAgent signature
    logDebugNS "receiver" "Verification successful"
    let ev = decodeEvent eventHeader body
    case ev of
        Left err -> do
            logErrorNS "receiver" "Unparseable json"
            logErrorNS "receiver" $(isT "#{err}")
            throwError $(isT "Unparseable json. For details on the error refer to the log.")
        Right (PingEvent ping) -> do
            logInfoNS "receiver" "Ping received"
            let fileName = $(isS "hook_#{ping^.hookId}.conf.json")
                filePath = directory </> fileName
            isDir <- liftIO $ doesDirectoryExist filePath
            exists <- liftIO $ doesFileExist filePath
            if isDir
                then do
                    logErrorNS "receiver" "Hook config location is directory"
                    throwError $(isT "Ping error. For error details refer top the log.")
                else do
                    when exists $ logInfoNS "receiver" "Identically named hook config present, overwriting"
                    return ( $(isT "Ping received. Data saved in #{filePath}")
                           , writeHook filePath $ ping^.hook
                           )
        Right event -> do
            logDebugNS "receiver" "Event parsed, starting execution"
            return ( $(isT "Hook received. For build results refer to the log.")
                   , handleEvent watchConf event
                   )
  where
    directory = fromMaybe defaultDataDirectory $ watchConf ^. dataDirectory

