{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (lookup)
import Data.Aeson (decode')
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Maybe (fromJust)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (POSIXTime)
import Database.MongoDB
  ( Query (..), Value (..)
  , (=:), findOne, insertAll_, lookup, select )
import Network.HTTP.Conduit
  ( Request (..), Response (..)
  , httpLbs, parseUrl, withManager )
import Network.HTTP.Types

import Data.Bson.Aeson ()
import Web.WheresMikeCraig.Config


getLastTime :: Config -> IO (Maybe POSIXTime)
getLastTime cfg = do
  Right mdoc <- cfgAccess cfg $ findOne
    (select [] $ cfgPointsColl cfg) { sort = ["date_ts" =: (-1 :: Int)]
                                    , project = ["date_ts" =: (1 :: Int)] }
  return $ mdoc >>= lookup "date_ts"

insertPoints :: Config -> L8.ByteString -> IO ()
insertPoints cfg bs = do
  case decode' bs of
    Just (Doc doc) -> case lookup "points" doc of
      Just docs -> do
        Right () <- cfgAccess cfg $ insertAll_ (cfgPointsColl cfg) docs
        return ()
      Nothing -> fail "Unexpected Geoloqi response"
    _ -> fail "Unexpected Geoloqi response"


geoloqiUrl :: String
geoloqiUrl = "https://api.geoloqi.com/1/location/history"

geoloqiReq :: Config -> Maybe POSIXTime -> Request IO
geoloqiReq cfg mtime =
  let opts = "?ignore_gaps=1&count=100000" ++ case mtime of
        Just time -> "&after=" ++ show (truncate time :: Int)
        Nothing -> ""
      token = "OAuth " `B8.append` cfgGeoloqiToken cfg
  in (fromJust $ parseUrl $ geoloqiUrl ++ opts)
    { requestHeaders = [headerAuthorization token] }


main :: IO ()
main = do
  getCurrentTime >>= putStrLn . ("Started recorder at " ++) . show
  cfg <- getConfig
  mtime <- getLastTime cfg
  r <- withManager $ httpLbs $ geoloqiReq cfg mtime
  insertPoints cfg $ responseBody r
  getCurrentTime >>= putStrLn . ("Finished recorder at " ++) . show
