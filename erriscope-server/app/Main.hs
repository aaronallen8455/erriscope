{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar
import           Control.Monad
import           Data.FileEmbed (embedStringFile)
import qualified Data.Map.Strict as M
import           Network.Wai
import           Network.HTTP.Types
import           Network.Wai.Handler.Warp (run)

import           Erriscope.Html (ErrorCache(..), emptyErrorCache, parseErrorId)
import           Erriscope.Sockets (runWebsocket)
import           Paths_erriscope_server (getDataFileName)

-- Start a web socket server in conjunction with the file server
--
-- Js file will connect to the websocket and listen for messages containing
-- html to populate parts of the page with
--
-- The plugin also connects to the websocket server and sends it messages
-- containing the list of errors and warnings. Warnings are going to be sticky
-- ala pinned-warnings
--
-- The error viewport should have a hyperlink that copies a vim command
-- to the clipboard that goes to the error location:
-- e file/path | {lineNum}
--
-- tooltip on the errors should show filename + line number
--
-- Open questions:
-- - how to handle the errors being refreshed while an error is
-- currently displayed? Completely reset and focus the first error? Seems fairly
-- reasonable to do.
-- - would it be better to generate the HTML server side or send the standard
-- message and generate the dom elements in the JS script? This would require
-- the JS script being coupled to the message data type. Would be nice to only
-- have awareness of the data type on the haskell side. But then how will the
-- viewport be populated? Just reuse the contents of the preview? Send JSON
-- objects that have both the preview HTML as well as the viewport HTML for each
-- error? That seems pretty reasonable and minimizes the amount of logic that
-- will need to exist in JS.
-- - How to handle the port for both the web socket and the file server? command
-- line arguments and a plugin option?
--
-- A possible approach would be to send just the sidebar HTML where each sidebar
-- element has click handler that calls a function existing on frontend and the
-- html to populate the viewport with exists as a meta attribute in the HTML.
-- This makes the JS very light but also limits adding functionality such as
-- cycling through error messages using ctlr-arrow keys. It would be nice to
-- have the errors existing as an in-memory array in JS. This does entail
-- building the DOM on the JS side...
-- Perhaps the JS merely knows about the index of the currently selected error
-- message (and how many messages there are) and then sends requests for the
-- viewport HTML of the error at specific index. Could then cache a mapping of
-- indices to viewport HTML. Since the server is running locally the latency
-- is not really an issue.
-- This makes the server slightly more complex but keeps the javascript very
-- light - it only needs to track the currently selected error index.
-- Also means that no JSON needs to sent, only HTML.
-- The socket could be overloaded to also receive requests from the clients for
-- viewport HTML but it seems better to use the http server for this. The
-- consequence is that the var containing the most recent payload must be shared
-- between the websocket and the http server. Easy enough.
-- How would the ignore file functionality work though? Could that simply be
-- some CSS rules that get toggled?
-- Error could be identified by both file index and index within that file.
-- There is then handlers for ignoring a specific file index which also applies
-- the CSS rule and a 'show' inverse.
-- Note the target element of the event is available through ev.target (currentTarget instead?)


main :: IO ()
main = do
  htmlCache <- newMVar emptyErrorCache
  void . forkIO $ runWebsocket htmlCache
  run 8082 (app htmlCache)

app :: MVar ErrorCache -> Application
app errorsMVar request respond = do
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
      respond $
        responseLBS
          status200
          [("Content-Type", "application/javascript")]
          lbs

    ["assets", "style.css"] -> do
      let lbs = $(embedStringFile "assets/style.css")
      respond $
        responseLBS
          status200
          [("Content-Type", "text/css")]
          lbs

    ["favicon.ico"] -> do
      filePath <- getDataFileName "./assets/favicon.ico"
      respond $
        responseFile
          status200
          [("Content-Type", "image/png")]
          filePath
          Nothing

    ["error", errorId]
      | Just (ix, file) <- parseErrorId errorId
      -> do
        -- TODO update the currently selected error?
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
      | otherwise ->
          respond $
            responseLBS
              status200
              [("Content-Type", "text/plain")]
              ""

    _ ->
      respond $
        responseLBS
          status404
          [("Content-Type", "text/plain")]
          "Not found."
