module Erriscope
  ( plugin
  ) where

import           Control.Concurrent.MVar
import           Control.Monad.IO.Class
import qualified Network.WebSockets as WS
import           System.IO.Unsafe

import qualified Erriscope.Internal.GhcFacade as Ghc

{-# NOINLINE socketMVar #-}
socketMVar :: MVar (Maybe WS.Connection)
socketMVar = unsafePerformIO $ newMVar Nothing

plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
  { Ghc.dynflagsPlugin = driverPlugin
  , Ghc.pluginRecompile = Ghc.purePlugin
  , Ghc.typeCheckResultAction = const typeCheckResult
  , Ghc.parsedResultAction = const parsedResult
  }

driverPlugin :: [Ghc.CommandLineOption] -> Ghc.DynFlags -> IO Ghc.DynFlags
driverPlugin _opts dynFlags = do
  -- initialize websocket if needed

  putStrLn "Driver plugin"
  pure dynFlags
--     { -- Ghc.log_action = undefined -- broadcastError
--     }

typeCheckResult :: Ghc.ModSummary -> a -> Ghc.TcM a
typeCheckResult mod x = do
  liftIO $ putStrLn "Type Checking finished"
  pure x

parsedResult :: Ghc.ModSummary -> a -> Ghc.Hsc a
parsedResult mod x = do
  liftIO $ putStrLn "Parsing finished"
  pure x

-- since the log action deals with one error at a time, add them to a queue and
-- flush on some other compilation stage, although don't think such a phase
-- exists, they will all be per module.
-- type LogAction = DynFlags
--               -> WarnReason
--               -> Severity
--               -> SrcSpan
--               -> MsgDoc
--               -> IO ()
-- 
-- Can use a heuristic: we can know somehow when a module begins compilation
-- (using the parsing phase?) Can use the hscFrontendHook for this.
--
-- Here's what we know for certain:
-- - The module identifier if an error occurs during parsing.
-- - Module name when parsing completes
-- - When type checking completes
-- - We know when a new module compilation starts although not which module.
--
-- So when a module compilation starts, there should be some forthcoming signal.
-- Once such a signal is received, start a timer.
-- If another module begins compiling then stop the timer.
-- If timer expires then emit the errors to the server
-- Don't care about parsing completion, only when errors get emitted and when
-- type checking completes. (errors should all be emitted around the same time?)
-- ALSO want to hold onto warnings and clear them when an event occurs for a
-- given module. Do this when parsing completes for a module.
--
-- Why not send the errors one by one and then have a different message that
-- clears the errors for a given module (triggered by type check completion).
-- Parsing completion would trigger clearing the warnings for a given module.
-- This will change the way the data must be modeled on the server and is also
-- a problem for sending it to the frontend which expects a single chunk of
-- html. Instead, each error would be sent over as an individual chunk of html
-- that must be appended to the sidebar. Clearing the errors for a module would
-- entail doing a complete replace of the sidebar html.
--
-- Also need to clear the ERRORS when parsing completes because there may be
-- old errors that got fixed and we want to remove them so they don't persist.
-- In fact, as soon as either a parse error occurs or parsing completes, clear
-- the errors - but this isn't good enough because there could be multiple
-- parse errors and we don't want one to clear out the other. So back to some
-- timer based heuristic? Can you have more than one parse error for the same
-- module? seems like it would halt at that point?
-- Yes, seems like it's just one parse error at a time.
