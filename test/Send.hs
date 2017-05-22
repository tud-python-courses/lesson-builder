#!/usr/local/bin/runhaskell
{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as B
import           Data.Maybe            (fromMaybe)
import           Network.HTTP.Client
import           Network.HTTP.Types
import           System.Environment


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
                                    ++ [("User-Agent", "GitHub-Hookshot/3fbb3c7")
                                       , ("X-GitHub-Event", "push")
                                       , ("X-GitHub-Delivery", "5c5f4180-28f3-11e7-8211-ea0c2399c3bd")
                                       , ("content-type", "application/json")
                                       ]
                                 }

    response <- httpLbs request manager
    putStrLn $ show (statusCode $ responseStatus response) ++ " " ++ BS.unpack (statusMessage (responseStatus response))
    print $ responseBody response
