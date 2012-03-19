{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Web.WheresMikeCraig.Config
  ( Config (..)
  , getConfig
  ) where

import Control.Applicative ((<$>))
import Data.ByteString.Char8 (pack)
import Data.CompactString.Internal (CompactString (CS))
import Data.Yaml ((.:), Value (Object), decodeFile, parseMonad)
import Database.MongoDB
  ( AccessMode (..), Action, Collection, Failure
  , (=:), access, connect, host, ensureIndex, index, iUnique, runIOE )
import Network.HTTP.Types (Ascii)
import System.Environment (getEnv)

data Config = Config
  { cfgAccess :: forall a. Action IO a -> IO (Either Failure a)
  , cfgPointsColl :: Collection
  , cfgGeoloqiToken :: Ascii }

getConfig :: IO Config
getConfig = do
  token <- pack <$> getEnv "GEOLOQI_TOKEN"
  mval <- decodeFile "config.yml"
  case mval of
    Just (Object v) -> do
      db <- CS <$> parseMonad (.: "database") v
      points <- CS <$> parseMonad (.: "points_collection") v
      pipe <- runIOE $ connect $ host "127.0.0.1"
      let access' = access pipe UnconfirmedWrites db
      Right () <- access' $
        ensureIndex $ index points ["date_ts" =: (1 :: Int)]
      Right () <- access' $
        ensureIndex $ (index points ["uuid" =: (1 :: Int)]) { iUnique = True }
      return $ Config access' points token
    _ -> fail "Invalid config file!"
