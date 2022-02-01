{-# LANGUAGE TypeApplications #-}
module Erriscope.Sockets
  ( runWebsocket
  ) where

import           Control.Concurrent.MVar
import           Control.Exception (finally)
import           Control.Monad
import qualified Data.ByteString as BS
import           Data.Foldable
import           Data.Maybe
import           Data.Traversable
import qualified Data.IntMap.Strict as IM
import qualified Network.WebSockets as WS

import           Erriscope.Html
import qualified Erriscope.Types as ET

type ClientId = Int
type Client = WS.Connection
type Clients = IM.IntMap Client

-- | Renders the HTML for a payload and distributes it to the clients.
broadcastPayload :: ET.Payload -> MVar HtmlCache -> MVar Clients -> IO ()
broadcastPayload payload htmlCacheMVar clientsMVar = do
  let newHtmlCache = renderPayload payload
  putMVar htmlCacheMVar newHtmlCache

  withMVar clientsMVar $ \clients -> do
    for_ clients $ \conn ->
      WS.sendTextData conn (sidebarHtml newHtmlCache)

runWebsocket :: MVar HtmlCache -> IO ()
runWebsocket htmlMVar = do
  clients <- newMVar mempty
  WS.runServer "127.0.0.1" 9160 $ serverApp htmlMVar clients

serverApp :: MVar HtmlCache -> MVar Clients -> WS.ServerApp
serverApp htmlMVar clientsMVar pending = do
  conn <- WS.acceptRequest pending
  connType <- WS.receiveData conn :: IO BS.ByteString
  case connType of
    "plugin" -> WS.withPingThread conn 30 (pure ())
              . (`finally` pure ())
              . forever $ do
      msg <- WS.receiveData conn
      case ET.decodePayload msg of
        Left err -> putStrLn $ "Decoding error: " <> err
        Right payload -> broadcastPayload payload htmlMVar clientsMVar

    "frontend" -> do
      clientId <- addClient conn clientsMVar
      -- send current sidebar html to new client
      sendHtmlToClient htmlMVar clientsMVar clientId
      let disconnect = removeClient clientId clientsMVar
      WS.withPingThread conn 30 (pure ()) . (`finally` disconnect) $
        forever . void $ WS.receiveData @BS.ByteString conn

    _ -> WS.sendClose conn ("invalid connection type" :: BS.ByteString)

addClient :: WS.Connection -> MVar Clients -> IO ClientId
addClient conn clientsMVar =
  modifyMVar clientsMVar $ \clients ->
    let clientId = maybe 0 fst $ IM.lookupMax clients
     in pure (IM.insert clientId conn clients, clientId)

removeClient :: ClientId -> MVar Clients -> IO ()
removeClient clientId clientsMVar =
  modifyMVar_ clientsMVar $ pure . IM.delete clientId

sendHtmlToClient :: MVar HtmlCache -> MVar Clients -> ClientId -> IO ()
sendHtmlToClient htmlMVar clientsMVar clientId =
  withMVar htmlMVar $ \htmlCache ->
    withMVar clientsMVar $ \clients ->
      for_ (IM.lookup clientId clients) $ \conn ->
        WS.sendTextData conn $ sidebarHtml htmlCache
