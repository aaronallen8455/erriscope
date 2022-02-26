{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Concurrent.MVar
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.FileEmbed (embedFile, embedStringFile)
import qualified Data.Map.Strict as M
import           System.Environment (getArgs)
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp as Wai
import qualified Network.Wai.Handler.WebSockets as Wai
import qualified Network.WebSockets as WS

import           Erriscope.Html (ErrorCache(..), emptyErrorCache, parseErrorId)
import           Erriscope.Sockets (socketServer)
import qualified Erriscope.Types as ET

main :: IO ()
main = do
  htmlCache <- newMVar emptyErrorCache
  clients <- newMVar mempty
  args <- getArgs
  let port = ET.getPortFromArgs args
  startingHerald port
  Wai.run port $
    Wai.websocketsOr
      WS.defaultConnectionOptions
      (socketServer htmlCache clients)
      (app port htmlCache)

startingHerald :: ET.Port -> IO ()
startingHerald port = do
  let herald = "= Starting Erriscope Server on Port " <> show port <> " ="
  putStrLn $ replicate (length herald) '='
  putStrLn herald
  putStrLn $ replicate (length herald) '='

app :: ET.Port -> MVar ErrorCache -> Application
app port errorsMVar request respond = do
  case pathInfo request of
    [] -> do
      let lbs = $(embedStringFile "assets/index.html")
      respond $
        responseLBS
          status200
          [("Content-Type", "text/html")]
          lbs

    ["assets", "script.js"] -> do
      let lbs = $(embedStringFile "assets/script.js")
          portBinding = "const socketPort = " <> LBS.pack (show port) <> ";"
      respond $
        responseLBS
          status200
          [("Content-Type", "application/javascript")]
          (portBinding <> lbs)

    ["assets", "style.css"] -> do
      let lbs = $(embedStringFile "assets/style.css")
      respond $
        responseLBS
          status200
          [("Content-Type", "text/css")]
          lbs

    ["favicon.ico"] -> do
      let bs = $(embedFile "assets/favicon.ico")
      respond $
        responseLBS
          status200
          [("Content-Type", "image/png")]
          (LBS.fromStrict bs)

    ["error"] -> do
      reqBody <- getRequestBodyChunk request
      case parseErrorId reqBody of
        Just (ix, file) -> do
          errorCache <- readMVar errorsMVar
          case M.lookup file (fileErrors errorCache) of
            Just errors
              | (_, (_, html) : _) <- splitAt (fromIntegral ix) errors ->
              respond $
                responseLBS
                  status200
                  [("Content-Type", "text/html")]
                  html
            _ ->
              respond $
                responseLBS
                  status404
                  [("Content-Type", "text/plain")]
                  "Error index doesn't exist"
        Nothing ->
          respond $
            responseLBS
              status404
              [("Content-Type", "text/plain")]
              "Invalid request"

    _ ->
      respond $
        responseLBS
          status404
          [("Content-Type", "text/plain")]
          "Not found."
