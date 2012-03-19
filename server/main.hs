{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Main where

import Control.Monad.Trans.Resource (ResourceT, withIO)
import Data.Aeson (encode)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Database.MongoDB
import Network.HTTP.Types
import Network.Wai.Handler.Warp
import Network.Wai
import Text.Blaze
import Text.Blaze.Renderer.Utf8
import Text.Hamlet

import Data.Bson.Aeson ()
import Web.WheresMikeCraig.Config


getPoints :: Config -> ResourceT IO [Document]
getPoints cfg = do
  (_, Right curs) <- withIO
    (cfgAccess cfg $ find $ select [] $ cfgPointsColl cfg)
    (\(Right curs) -> cfgAccess cfg (closeCursor curs) >> return ())
  (_, Right points) <- withIO (cfgAccess cfg $ rest curs) $ const (return ())
  return points

html :: Html -> Html
html points = $(shamletFile "index.hamlet")

server :: Config -> Application
server cfg _ = do
  points <- getPoints cfg
  return $ ResponseBuilder status200 [] $ renderHtmlBuilder $
    html $ preEscapedLazyText $ decodeUtf8 $ encode $ Array $ map Doc points

main :: IO ()
main = do
  cfg <- getConfig
  run 3000 $ server cfg
