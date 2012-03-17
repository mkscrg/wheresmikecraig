{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (putStrLn)
import Control.Applicative ((<$>))
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Char8 (append, pack)
import Data.ByteString.Lazy (putStrLn)
import Data.Maybe (fromJust)
import Network.HTTP.Conduit
import Network.HTTP.Types
import System.Environment (getEnv)

main :: IO ()
main = do
  t <- getToken
  r <- withManager $ httpLbs $ req t
  let v = fromJust $ decode' $ responseBody r :: Value
  putStrLn $ encodePretty v

req :: Ascii -> Request IO
req token = (fromJust $ parseUrl "https://api.geoloqi.com/1/location/history")
  { requestHeaders = [headerAuthorization $ "OAuth " `append` token] }

getToken :: IO Ascii
getToken = pack <$> getEnv "GEOLOQI_OAUTH"
