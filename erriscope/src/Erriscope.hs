{-# LANGUAGE LambdaCase #-}
module Erriscope
  ( plugin
  ) where

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS8
import           Data.Foldable
import qualified Data.Map.Strict as M
import qualified Network.Socket as S
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as Stream
import           System.IO.Unsafe

import qualified Erriscope.Internal.GhcFacade as Ghc
import qualified Erriscope.Types as ET

{-# NOINLINE connMVar #-}
connMVar :: MVar (Maybe WS.Connection)
connMVar = unsafePerformIO $ newMVar Nothing

{-# NOINLINE knownFilesMVar #-}
knownFilesMVar :: MVar (M.Map ET.FilePath (Maybe ET.ModuleName))
knownFilesMVar = unsafePerformIO $ newMVar mempty

plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
  { Ghc.dynflagsPlugin = driverPlugin
  , Ghc.pluginRecompile = Ghc.purePlugin
  , Ghc.parsedResultAction = const parsedResult
  }

-- | Overrides the log action and the phase hook.
driverPlugin :: [Ghc.CommandLineOption] -> Ghc.DynFlags -> IO Ghc.DynFlags
driverPlugin _opts dynFlags = do
  -- TODO get port from opts
  initializeWebsocket 9160

  deleteAll

  putStrLn "Driver plugin"
  pure dynFlags
    { Ghc.log_action = \dflags warnReason severity srcSpan msgDoc -> do
        reportError dflags warnReason severity srcSpan msgDoc
        Ghc.log_action dynFlags dflags warnReason severity srcSpan msgDoc
    , Ghc.hooks = (Ghc.hooks dynFlags)
        { Ghc.runPhaseHook = Just $ \phase filePath dflags -> do
            case phase of
              Ghc.RealPhase (Ghc.Cpp hscSrc) ->
                -- The CPP phase runs at the beginning of compiling a module.
                liftIO $ beginCompilation (BS8.pack filePath) hscSrc
              _ -> pure ()
            Ghc.runPhase phase filePath dflags
        }
    }

-- | When a module starts compiling, delete all existing errors for the file
-- and also prune deleted files.
beginCompilation :: ET.FilePath -> Ghc.HscSource -> IO ()
beginCompilation modFile Ghc.HsSrcFile = do
  withMVar connMVar $ traverse_ $ \conn -> do
    let deleteMsg = ET.MkEnvelope
          { ET.version = 0
          , ET.message = ET.DeleteFile modFile
          }
    WS.sendBinaryData conn (ET.encodeEnvelope deleteMsg)
    modifyMVar_ knownFilesMVar $
      pure . M.insertWith (<|>) modFile Nothing
  pruneDeletedFiles
beginCompilation _ _ = pure ()

-- | Check for any known files that have been deleted and remove it from the
-- server and known files map.
pruneDeletedFiles :: IO ()
pruneDeletedFiles = do
  undefined
  -- Check if file exists
  -- send deleteAll message
  -- Remove from knownFiles

-- | Opens the websocket connection
initializeWebsocket :: Int -> IO ()
initializeWebsocket port =
  modifyMVar_ connMVar $ \case
    Just conn -> pure (Just conn)
    Nothing -> do
      -- Create and connect socket
      let hints = S.defaultHints {S.addrSocketType = S.Stream}
          -- Correct host and path.
          host = "localhost"
          fullHost = if port == 80 then host else host ++ ":" ++ show port
      addr:_ <- S.getAddrInfo (Just hints) (Just host) (Just $ show port)
      sock   <- S.socket (S.addrFamily addr) S.Stream S.defaultProtocol
      S.setSocketOption sock S.NoDelay 1
      -- TODO does the stream need to be closed when application shuts down?
      -- Could use `mkWeakMVar` if so
      stream <- Stream.makeSocketStream sock

      Just <$>
        WS.newClientConnection
          stream
          fullHost
          "/"
          WS.defaultConnectionOptions
          []

-- | If a module passes the parsing phase then we get its proper name
parsedResult :: MonadIO m
             => Ghc.ModSummary
             -> Ghc.HsParsedModule
             -> m Ghc.HsParsedModule
parsedResult modSum parsedMod
  | Just srcFilePath <- fmap BS8.pack . Ghc.ml_hs_file $ Ghc.ms_location modSum
  = do
    let mModName = fmap (Ghc.bytesFS . Ghc.moduleNameFS . Ghc.unLoc)
                 . Ghc.hsmodName . Ghc.unLoc
                 $ Ghc.hpm_module parsedMod

    liftIO . modifyMVar_ knownFilesMVar $
      pure . M.insert srcFilePath mModName
    pure parsedMod
  | otherwise = pure parsedMod

-- | Send error message to server
reportError :: Ghc.LogAction
reportError dynFlags _warnReason severity srcSpan msgDoc
  | Ghc.RealSrcSpan rss _ <- srcSpan
  , Just errType <- getErrorType severity
  = do
    let modFile = Ghc.bytesFS $ Ghc.srcSpanFile rss
    mModName <- fmap join . withMVar knownFilesMVar
              $ pure . M.lookup modFile
    let errBody = BS8.pack
                $ Ghc.showSDocForUser dynFlags Ghc.alwaysQualify msgDoc
        loc = ET.MkLocation
          { ET.lineNum = fromIntegral $ Ghc.srcSpanStartLine rss
          , ET.colNum = fromIntegral $ Ghc.srcSpanStartCol rss
          }
        fileErr = ET.MkFileError
          { ET.moduleName = mModName
          , ET.filepath = modFile
          , ET.errorMsg = ET.MkErrorMsg
            { ET.body = errBody
            , ET.errorType = errType
            , ET.fileLocation = loc
            }
          }
        errorMessage =
          ET.MkEnvelope
            { ET.version = 0
            , ET.message = ET.AddError fileErr
            }

    withMVar connMVar $ traverse_ $ \conn ->
      WS.sendBinaryData conn (ET.encodeEnvelope errorMessage)

  | otherwise = pure ()
  where
    getErrorType = \case
      Ghc.SevWarning -> Just ET.Warning
      Ghc.SevError   -> Just ET.Error
      _              -> Nothing

-- | Send a message to delete all existing errors on the server.
deleteAll :: IO ()
deleteAll = do
  let msg = ET.MkEnvelope
        { ET.version = 0
        , ET.message = ET.DeleteAll
        }

  withMVar connMVar $ \mConn -> for_ mConn $ \conn ->
    WS.sendBinaryData conn (ET.encodeEnvelope msg)

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
--
-- Need to check for files being deleted. How should this happen? The simple
-- solution to check for the existence of each known file during plugin
-- initilization. This would occur before compiling each module, which is wasteful
-- but the only other option is to have some heuristic on how much time has passed
-- since errors were added or removed.
--
-- What about this: When error is received for a known file, clear out all the
-- errors (so that there no other known files). The trick is that when a second
-- error is received for that file, we don't want to clear it again. This can
-- be achieved by checking if the same file for which the last error was received.
-- This breaks down in the case where errors for multiple files are emitted
-- concurrently, which can happen when compiling with -j. Do errors get interleaved
-- when compiling concurrently? I'm thinking they don't... This method doesn't
-- work anyways - new files can be added in such a way that the clearing would
-- happen in a poor way. Not necessarily, if we only clear the files that were
-- not compiled since the last time the known file was compiled then that should
-- be OK. This is all predicated on errors not being emitted concurrently - seems
-- like it could happen... Can just check if there are existing errors for that
-- file because existing errors would have been cleared out during the parsing
-- phase so if there is 1 or more error then don't clear other files.
-- Could use the file clear occuring at completion of parse phase or emission
-- of parse phase as the signal to clear other files.
--
-- Is there a way to tell that an error occurred during parsing? Could simply
-- check the error body for certain characteristics. Best option seems to be
-- inspecting the error message to see if it's a parse error.
--
-- So heres the outline of what can happen
--
-- - Error occurs during parsing: Check if the error is a parse failure. If
-- so, clear existing errors for that module then emit the new one.
-- - Module completes the parse phase: Clear existing errors for this module
-- - Error occurs during type checking: Check if error is a parse failure.
-- If not, don't clear errors for this module and emit the new error.
-- - Type checking completes: Clear errors for this module.
--
-- Now to take care of files being deleted:
-- Brute force option is to check for files that no longer exist at each plugin
-- initialization (start of a module compilation). This may be the only truly
-- reliable option given the complications arising from parallel compilation.
-- Since there aren't likely to be a huge number of files with errors, it seems
-- reasonable to prune deleted files each time the plugin initialization occurs.
--
-- When socket initializes, send a message to clear any existing errors on the
-- server.
