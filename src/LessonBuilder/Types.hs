{-|
Module      : $Header$
Description : Types, lenses, to and from json instances.
Copyright   : (c) Justus Adam 2017.
License     : MIT
Maintainer  : dev@justus.science
Stability   : experimental
Portability : portable
-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module LessonBuilder.Types where


import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types       (camelTo2, fieldLabelModifier)
import           Data.Hashable
import           Data.HashMap.Strict    (HashMap)
import           Data.List              (stripPrefix)
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import           Data.Vector            (Vector)
import           GHC.Generics
import           Lens.Micro.Platform    hiding ((.=))
import           Marvin.Interpolate.All


data Include = Include
    { includeRepository :: !String
    , includeDirectory  :: !FilePath
    , includeConfigFile :: Maybe FilePath
    }


data BuildConf = BuildConf
    { buildConfBuilds   :: !(HashMap String Build)
    , buildConfIncludes :: !(Vector Include)
    }


data Command
    = HtLatex  { commandArgs    :: [Text] }
    | Pdflatex { commandArgs    :: [Text] }
    | Hevea    { commandArgs    :: [Text] }
    | Xelatex  { commandArgs    :: [Text] }
    | Latexmk  { commandArgs    :: [Text] }
    | Custom   { commandCommand :: Text
               , commandArgs    :: [Text]
               }
    deriving (Show, Eq, Ord, Generic, Hashable)


data Build = Build
    { buildCommand   :: !Command
    , buildTargetDir :: !FilePath
    , buildSourceDir :: !FilePath
    , buildFiles     :: !(Vector FilePath)
    }


data Watch = Watch
    { watchName       :: !String
    , watchDirectory  :: !FilePath
    , watchConfigFile :: Maybe FilePath
    }


data WatchConf = WatchConf
    { watchConfDataDirectory  :: !(Maybe FilePath)
    , watchConfWatched        :: !(Vector Watch)
    , watchConfSecret         :: !(Maybe String)
    , watchConfReposDirectory :: !(Maybe FilePath)
    }


newtype CommitData = CommitData { commitDataMessage :: String }


data Event
    = PushEvent !Push
    | PingEvent !Ping


data Push = Push
    { pushRepository :: !Repo
    , pushHeadCommit :: !CommitData
    }


data Ping = Ping
    { pingRepository :: !Repo
    , pingHookId     :: !Int
    , pingHook       :: !Value
    }


data Repo = Repo
    { repoName    :: !String
    , repoIdField :: !Int
    , repoApiUrl  :: !String
    }

makeFields ''Include
makeFields ''BuildConf
makeFields ''Build
makeFields ''Watch
makeFields ''WatchConf
makeFields ''CommitData
makeFields ''Push
makeFields ''Ping
makeFields ''Repo


commandToStr (Custom c _) = c
commandToStr (HtLatex _)  = "htlatex"
commandToStr (Pdflatex _) = "pdflatex"
commandToStr (Hevea _)    = "hevea"
commandToStr (Xelatex _)  = "xelatex"
commandToStr (Latexmk _)  = "latexmk"


instance ToJSON Command where
    toJSON cmd = object ["command" .= commandToStr cmd, "args" .= commandArgs cmd]

cmdFromString "htlatex"  = HtLatex
cmdFromString "pdflatex" = Pdflatex
cmdFromString "hevea"    = Hevea
cmdFromString "xelatex"  = Xelatex
cmdFromString "latexmk"  = Latexmk
cmdFromString str        = Custom str

instance FromJSON Command where
    parseJSON v@(Array _) = do
        l <- parseJSON v
        case l of
            (cmd:args) -> return $ cmdFromString cmd args
            []         -> fail "Need list with at least one element"
    parseJSON (Object o) = cmdFromString <$> o .: "command" <*> o .:? "args" .!= []
    parseJSON (String t) = return $ cmdFromString t []
    parseJSON _ = fail "Expected object, text or array"


instance FromJSON Repo where
    parseJSON = withObject "expected object" $ \o ->
        Repo
            <$> o .: "full_name"
            <*> o .: "id"
            <*> o .: "url"

instance ToJSON Repo where
    toJSON r =
        object ["full_name".= (r ^. name), "id".= (r ^. idField), "url".= (r ^. apiUrl)]



instance FromJSON BuildConf where
    parseJSON = withObject "expected object" $ \o ->
        BuildConf
            <$> o .: "builds"
            <*> o .:? "includes" .!= mempty




let dropPrefix p t = fromMaybe t $ stripPrefix p t
    prefixOpts prefix = defaultOptions { fieldLabelModifier = camelTo2 '_' . dropPrefix prefix } in
    join <$> sequence
        [ deriveJSON (prefixOpts "commitData" ) ''CommitData
        , deriveJSON (prefixOpts "include") ''Include
        , deriveJSON (prefixOpts "build"  ) ''Build
        , deriveJSON (prefixOpts "ping"   ) ''Ping
        , deriveJSON (prefixOpts "push"   ) ''Push
        , deriveJSON (prefixOpts "watch"  ) ''Watch
        , deriveJSON (prefixOpts "watchConf") ''WatchConf
        ]
