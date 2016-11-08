#!/usr/local/bin/runhaskell
{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client
import Network.HTTP.Types
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS
import Data.Aeson
import Data.Maybe (fromMaybe)
import System.Environment


main :: IO ()
main = do
    [url] <- getArgs
    manager <- newManager defaultManagerSettings
    file <- B.readFile "req.json"

    let minified = encode (fromMaybe (error "json parsing failed") (decode file) :: Value) 

    -- Create the request
    initialRequest <- parseRequest $ "http://" ++ url
    let request = initialRequest { method = "POST"
                                 , requestBody = RequestBodyLBS minified
                                 , requestHeaders = requestHeaders initialRequest 
                                    ++ [("User-Agent", "GitHub-Hookshot/6e58126")
                                       , ("X-GitHub-Event", "push")
                                       , ("X-Hub-Signature", "sha1=1b5d3cec61d46977cabd9df18634eb0494b6c23d")
                                       , ("content-type", "application/json")
                                       ] 
                                 }

    response <- httpLbs request manager
    putStrLn $ show (statusCode $ responseStatus response) ++ " " ++ BS.unpack (statusMessage (responseStatus response))
    print $ responseBody response