{-# LANGUAGE TypeApplications #-}
module Erriscope.Sockets
  ( socketServer
  ) where

import           Control.Concurrent.MVar
import           Control.Exception (finally, handle, try)
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

socketServer :: MVar ErrorCache -> MVar Clients -> WS.ServerApp
socketServer errorsMVar clientsMVar pending = do
  conn <- WS.acceptRequest pending
  connType <- WS.receiveData conn :: IO BS.ByteString
  case connType of
    "plugin" -> WS.withPingThread conn 30 (pure ())
              . (`finally` pure ())
              . forever $ do
      eMsg <- try @WS.ConnectionException $ WS.receiveData conn
      case eMsg of
        Left _err -> pure ()
        Right msg ->
          case ET.decodeEnvelope msg of
            Left err -> putStrLn $ "Decoding error: " <> err
            Right (ET.MkEnvelope _ m) -> handleMessage errorsMVar clientsMVar m

    "frontend" -> do
      clientId <- addClient conn clientsMVar
      -- send current sidebar html to new client
      sendSidebarHtmlToClient errorsMVar conn
      let disconnect = removeClient conn clientId clientsMVar
          errHandler _ = pure ()
      WS.withPingThread conn 30 (pure ()) . (`finally` disconnect) $
        forever . handle @WS.ConnectionException errHandler
                . void $ WS.receiveData @BS.ByteString conn

    _ -> WS.sendClose conn ("invalid connection type" :: BS.ByteString)

handleMessage :: MVar ErrorCache -> MVar Clients -> ET.Message -> IO ()
handleMessage errorsMVar clientsMVar message = do
  shouldEmit <- modifyMVar errorsMVar $ \errors ->
    case message of
      ET.AddError fileError -> do
        let html = renderViewport fileError
            entry = (fileError, html)
        pure ( errors
                 { fileErrors =
                     M.alter (Just . maybe [entry] (++ [entry]))
                             (ET.filepath fileError)
                             (fileErrors errors)
                 }
             , True
             )
      ET.DeleteFile filePath ->
        pure ( errors
                 { fileErrors = M.delete filePath $ fileErrors errors }
             , M.member filePath $ fileErrors errors
             )

  -- transmit to all clients
  when shouldEmit . withMVar clientsMVar . traverse_
    $ sendSidebarHtmlToClient errorsMVar

-- | Renders the sidebar content and sends it to all clients
sendSidebarHtmlToClient :: MVar ErrorCache -> WS.Connection -> IO ()
sendSidebarHtmlToClient errorsMVar conn =
  withMVar errorsMVar $
    WS.sendTextData conn . renderSidebar

addClient :: WS.Connection -> MVar Clients -> IO ClientId
addClient conn clientsMVar =
  modifyMVar clientsMVar $ \clients ->
    let clientId = maybe 0 fst $ IM.lookupMax clients
     in pure (IM.insert clientId conn clients, clientId)

removeClient :: WS.Connection -> ClientId -> MVar Clients -> IO ()
removeClient conn clientId clientsMVar = do
  WS.sendClose conn ("Websocket disconnecting" :: BS.ByteString)
  modifyMVar_ clientsMVar $ pure . IM.delete clientId
