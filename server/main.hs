{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

module Main where

import Prelude hiding (lookup)
import Blaze.ByteString.Builder.ByteString (fromLazyByteString)
import Control.Monad.Trans.Resource (withIO)
import Data.Aeson (encode)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Database.MongoDB
  ( Document, Query (sort), Value (Doc)
  , (=:), findOne, select )
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)
import Network.Wai
  ( Application, Response (ResponseBuilder), Request (pathInfo) )
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)
import Text.Blaze (Html, preEscapedLazyText, preEscapedText)
import Text.Blaze.Renderer.Utf8 (renderHtmlBuilder)
import Text.Hamlet (shamletFile)

import Data.Bson.Aeson ()
import Web.WheresMikeCraig.Config


getPoint :: Config -> IO Document
getPoint cfg = do
  Right (Just doc) <- cfgAccess cfg $ findOne
    (select [] $ cfgPointsColl cfg) { sort = ["date_ts" =: (-1 :: Int)] }
  return doc

html :: Config -> Html -> Html
html cfg geoloqiPoint =
  let dataUrl = preEscapedText $ cfgDataUrl cfg
  in $(shamletFile "server/index.hamlet")


index :: Config -> Application
index cfg _ = do
  (_, pt) <- withIO (getPoint cfg) $ const (return ())
  return $ ResponseBuilder status200 [] $ renderHtmlBuilder $ html cfg $
    preEscapedLazyText $ decodeUtf8 $ encode $ Doc pt

point :: Config -> Application
point cfg _ = do
  (_, pt) <- withIO (getPoint cfg) $ const (return ())
  return $ ResponseBuilder status200 [] $
    fromLazyByteString $ encode $ Doc pt

static :: Application
static = staticApp defaultWebAppSettings

switch :: Config -> Application
switch cfg req
  | pathInfo req == [] = index cfg req
  | pathInfo req == ["point.json"] = point cfg req
  | otherwise = static req


main :: IO ()
main = do
  cfg <- getConfig
  run (cfgServerPort cfg) $ switch cfg
