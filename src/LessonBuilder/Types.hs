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
    = HtLatex
    | Pdflatex
    | Hevea
    | Xelatex
    | Latexmk
    | Custom Text
    deriving (Show, Eq, Ord, Generic, Hashable)


data Build = Build
    { buildCommand   :: !Command
    , buildArgs      :: (Vector Text)
    , buildTargetDir :: !FilePath
    , buildSourceDir :: !FilePath
    , buildFiles     :: !(Vector FilePath)
    , buildRepeats   :: Maybe Int
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


commandToStr (Custom c) = c
commandToStr HtLatex  = "htlatex"
commandToStr Pdflatex = "pdflatex"
commandToStr Hevea    = "hevea"
commandToStr Xelatex  = "xelatex"
commandToStr Latexmk  = "latexmk"


instance ToJSON Command where
    toJSON = String . commandToStr

cmdFromString "htlatex"  = HtLatex
cmdFromString "pdflatex" = Pdflatex
cmdFromString "hevea"    = Hevea
cmdFromString "xelatex"  = Xelatex
cmdFromString "latexmk"  = Latexmk
cmdFromString str        = Custom str

instance FromJSON Command where
    parseJSON = withText "Expected object, text or array" $ return . cmdFromString


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
        , deriveJSON (prefixOpts "ping"   ) ''Ping
        , deriveJSON (prefixOpts "push"   ) ''Push
        , deriveJSON (prefixOpts "watch"  ) ''Watch
        , deriveJSON (prefixOpts "watchConf") ''WatchConf
        ]


instance FromJSON Build where
    parseJSON = withObject "expected object" $ \o -> Build
        <$> o .: "command"
        <*> o .:? "args" .!= mempty
        <*> o .:? "target_dir" .!= "."
        <*> o .:? "source_dir" .!= "."
        <*> o .: "files"
        <*> o .: "repeats"


instance ToJSON Build where
    toJSON b = object $
        [ "command" .= (b^.command)
        , "target_dir" .= (b^.targetDir)
        , "source_dir" .= (b^.sourceDir)
        , "files" .= (b^.files)
        , "repeats" .= (b^.repeats)
        ] ++ if null (b^.args) then [] else [ "args" .= (b^.args) ]
