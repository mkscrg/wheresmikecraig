module Main where

import Blaze.ByteString.Builder.ByteString (fromByteString)
import Control.Monad.Trans.Resource (withIO)
import Data.ByteString.Char8 (pack)
import Database.MongoDB
import Network.HTTP.Types
import Network.Wai.Handler.Warp
import Network.Wai

import Web.WheresMikeCraig.Config


getPoints :: Config -> IO [Document]
getPoints cfg = do
  Right curs <- cfgAccess cfg $ find $ select [] (cfgPointsColl cfg)
  Right docs <- cfgAccess cfg $ rest curs
  Right () <- cfgAccess cfg $ closeCursor curs
  return docs

server :: Config -> Application
server cfg _ = do
  (_, points) <- withIO (getPoints cfg) $ const (return ())
  let rb = fromByteString $ pack $ show points
  return $ ResponseBuilder status200 [] rb

main :: IO ()
main = do
  cfg <- getConfig
  run 3000 $ server cfg
