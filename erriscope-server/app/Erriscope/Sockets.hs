{-# LANGUAGE TypeApplications #-}
module Erriscope.Sockets
  ( runWebsocket
  ) where

import           Control.Concurrent.MVar
import           Control.Exception (finally)
import           Control.Monad
import qualified Data.ByteString as BS
import           Data.Foldable
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Network.WebSockets as WS

import           Erriscope.Html
import qualified Erriscope.Types as ET

type ClientId = Int
type Client = WS.Connection
type Clients = IM.IntMap Client

runWebsocket :: MVar ErrorCache -> IO ()
runWebsocket errorsMVar = do
  clients <- newMVar mempty
  WS.runServer "127.0.0.1" 9160 $ serverApp errorsMVar clients

serverApp :: MVar ErrorCache -> MVar Clients -> WS.ServerApp
serverApp errorsMVar clientsMVar pending = do
  conn <- WS.acceptRequest pending
  connType <- WS.receiveData conn :: IO BS.ByteString
  case connType of
    "plugin" -> WS.withPingThread conn 30 (pure ())
              . (`finally` pure ())
              . forever $ do
      msg <- WS.receiveData conn
      case ET.decodeEnvelope msg of
        Left err -> putStrLn $ "Decoding error: " <> err
        Right (ET.MkEnvelope _ m) -> handleMessage errorsMVar clientsMVar m

    "frontend" -> do
      clientId <- addClient conn clientsMVar
      -- send current sidebar html to new client
      sendSidebarHtmlToClient errorsMVar conn
      let disconnect = removeClient clientId clientsMVar
      WS.withPingThread conn 30 (pure ()) . (`finally` disconnect) $
        forever . void $ WS.receiveData @BS.ByteString conn

    _ -> WS.sendClose conn ("invalid connection type" :: BS.ByteString)

handleMessage :: MVar ErrorCache -> MVar Clients -> ET.Message -> IO ()
handleMessage errorsMVar clientsMVar message = do
  modifyMVar_ errorsMVar $ \errors ->
    case message of
      ET.AddError fileError -> do
        let html = renderViewport fileError
            entry = (fileError, html)
        pure errors
          { fileErrors =
              M.alter (Just . maybe [entry] (++ [entry]))
                      (ET.filepath fileError)
                      (fileErrors errors)
          }
      ET.DeleteFile filePath ->
        pure errors
          { fileErrors = M.delete filePath $ fileErrors errors }
      ET.DeleteAll ->
        pure errors
          { fileErrors = mempty
          , selectedError = Nothing
          }

  -- transmit to all clients
  withMVar clientsMVar . traverse_
    $ sendSidebarHtmlToClient errorsMVar

-- | Renders the sidebar content and sends it to all clients
sendSidebarHtmlToClient :: MVar ErrorCache -> WS.Connection -> IO ()
sendSidebarHtmlToClient errorsMVar conn =
  withMVar errorsMVar $ \errors ->
    WS.sendTextData conn $ renderSidebar errors

addClient :: WS.Connection -> MVar Clients -> IO ClientId
addClient conn clientsMVar =
  modifyMVar clientsMVar $ \clients ->
    let clientId = maybe 0 fst $ IM.lookupMax clients
     in pure (IM.insert clientId conn clients, clientId)

removeClient :: ClientId -> MVar Clients -> IO ()
removeClient clientId clientsMVar =
  modifyMVar_ clientsMVar $ pure . IM.delete clientId
