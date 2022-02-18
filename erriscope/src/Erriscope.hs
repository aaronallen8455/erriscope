{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
module Erriscope
  ( plugin
  ) where

import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Network.Socket as Sock
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as Stream
import qualified System.Directory as Dir
import           System.IO.Unsafe

import qualified Erriscope.Internal.GhcFacade as Ghc
import qualified Erriscope.Types as ET

{-# NOINLINE connMVar #-}
connMVar :: MVar (Maybe WS.Connection)
connMVar = unsafePerformIO $ newMVar Nothing

data KnownFile =
  MkKnownFile
    { modName :: !(Maybe ET.ModuleName)
    , emittedErrors :: !(S.Set ET.ErrorBody)
    -- ^ It is necessary to track the errors that have been emitted for a file
    -- because when compiling with multiple targets in stack, the log action
    -- is run for each target which means duplicate errors would be emitted.
    }

{-# NOINLINE knownFilesMVar #-}
knownFilesMVar :: MVar (M.Map ET.FilePath KnownFile)
-- The ModuleName is a Maybe because errors might occur during the parsing
-- phase in which case we may not know the module name but still want to have
-- the file registered so that deleted file pruning will work.
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
  initializeWebsocket 8083

  deleteAll

  pure dynFlags
#if MIN_VERSION_ghc(9,0,0)
    { Ghc.log_action = \dflags warnReason severity srcSpan msgDoc -> do
        reportError dflags severity srcSpan msgDoc
        Ghc.log_action dynFlags dflags warnReason severity srcSpan msgDoc
#else
    { Ghc.log_action = \dflags warnReason severity srcSpan pprStyle msgDoc -> do
        reportError dflags severity srcSpan msgDoc
        Ghc.log_action dynFlags dflags warnReason severity srcSpan pprStyle msgDoc
#endif
    , Ghc.hooks = (Ghc.hooks dynFlags)
        { Ghc.runPhaseHook = Just $ \phase filePath dflags -> do
            case phase of
              Ghc.RealPhase (Ghc.Cpp _) -> -- TODO do something with the file type?
                -- The CPP phase runs at the beginning of compiling a module.
                liftIO $ beginCompilation (BS8.pack filePath)
              _ -> pure ()
            Ghc.runPhase phase filePath dflags
        }
    }

-- | When a module starts compiling, delete all existing errors for the file
-- and also prune deleted files.
beginCompilation :: ET.FilePath -> IO ()
beginCompilation modFile = do
  deleteErrorsForFile modFile
  modifyMVar_ knownFilesMVar $
    pure . M.insert modFile
      MkKnownFile { modName = Nothing , emittedErrors = mempty }
  pruneDeletedFiles

-- | Check for any known files that have been deleted and remove it from the
-- server and known files map.
pruneDeletedFiles :: IO ()
pruneDeletedFiles = do
  knownFiles <- readMVar knownFilesMVar
  let go filePath _ = do
        Dir.doesFileExist (BS8.unpack filePath) >>= \case
          True  -> pure ()
          False -> do
            deleteErrorsForFile filePath
            modifyMVar_ knownFilesMVar $ pure . M.delete filePath
  void $ M.traverseWithKey go knownFiles

-- | Opens the websocket connection
initializeWebsocket :: Int -> IO ()
initializeWebsocket port =
  modifyMVar_ connMVar $ \case
    Just conn -> pure (Just conn)
    Nothing -> do
      -- Create and connect socket
      let hints = Sock.defaultHints {Sock.addrSocketType = Sock.Stream}
          -- Correct host and path.
          host = "127.0.0.1"
          fullHost = if port == 80 then host else host ++ ":" ++ show port
      addr:_ <- Sock.getAddrInfo (Just hints) (Just host) (Just $ show port)
      sock   <- Sock.socket (Sock.addrFamily addr) Sock.Stream Sock.defaultProtocol
      Sock.setSocketOption sock Sock.NoDelay 1
      Sock.connect sock (Sock.addrAddress addr)
      -- TODO does the stream need to be closed when application shuts down?
      -- Could use `mkWeakMVar` if so
      stream <- Stream.makeSocketStream sock

      -- TODO what if the server is unreachable?
      conn <-
        WS.newClientConnection
          stream
          fullHost
          "/"
          WS.defaultConnectionOptions
          []

      WS.sendTextData conn (BS8.pack "plugin")
      pure $ Just conn

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
      pure . M.insert srcFilePath
        MkKnownFile { modName = mModName, emittedErrors = mempty }
    pure parsedMod
  | otherwise = pure parsedMod

-- | Send error message to server
reportError :: Ghc.DynFlags -> Ghc.Severity -> Ghc.SrcSpan -> Ghc.SDoc -> IO ()
reportError dynFlags severity srcSpan msgDoc
  | Ghc.RealSrcSpan' rss <- srcSpan
  , Just errType <- getErrorType severity
  -- , modFile /= BS8.pack "<interactive>" -- ignore errors from within ghci session
  = do
    let modFile = Ghc.bytesFS $ Ghc.srcSpanFile rss
    mMsg <- modifyMVar knownFilesMVar $ \knownFiles -> do
      case M.lookup modFile knownFiles of
        Nothing -> pure (knownFiles, Nothing)
        Just knownFile -> do
          let errBody = BSL.toStrict . BSB.toLazyByteString . BSB.stringUtf8
                      $ Ghc.showSDocForUser dynFlags Ghc.alwaysQualify msgDoc
          if errBody `S.member` emittedErrors knownFile
             then pure (knownFiles, Nothing)
             else do
              let mModName = modName knownFile
              caret <- Ghc.showSDocForUser dynFlags Ghc.alwaysQualify
                   <$> Ghc.getCaretDiagnostic severity srcSpan
              let loc = ET.MkLocation
                    { ET.lineNum = fromIntegral $ Ghc.srcSpanStartLine rss
                    , ET.colNum = fromIntegral $ Ghc.srcSpanStartCol rss
                    }
                  fileErr = ET.MkFileError
                    { ET.moduleName = mModName
                    , ET.filepath = modFile
                    , ET.errorMsg = ET.MkErrorMsg
                      { ET.body = errBody
                      , ET.caret = BSL.toStrict . BSB.toLazyByteString
                                 $ BSB.stringUtf8 caret
                      , ET.errorType = errType
                      , ET.fileLocation = loc
                      }
                    }
                  errorMessage =
                    ET.MkEnvelope
                      { ET.version = 0
                      , ET.message = ET.AddError fileErr
                      }
                  addEmittedError kf =
                    kf { emittedErrors = S.insert errBody $ emittedErrors kf }
                  newKnownFiles = M.adjust addEmittedError modFile knownFiles
              pure (newKnownFiles, Just errorMessage)

    for_ mMsg $ \errorMessage ->
      withMVar connMVar . traverse_ $ \conn ->
        WS.sendBinaryData conn (ET.encodeEnvelope errorMessage)

  | otherwise = pure ()
  where
    getErrorType = \case
      Ghc.SevWarning -> Just ET.Warning
      Ghc.SevError   -> Just ET.Error
      _              -> Nothing

-- | Remove all errors on the server for a specific file
deleteErrorsForFile :: ET.FilePath -> IO ()
deleteErrorsForFile modFile = do
  withMVar connMVar . traverse_ $ \conn -> do
    let deleteMsg = ET.MkEnvelope
          { ET.version = 0
          , ET.message = ET.DeleteFile modFile
          }
    WS.sendBinaryData conn (ET.encodeEnvelope deleteMsg)

-- | Send a message to delete all existing errors on the server.
deleteAll :: IO ()
deleteAll = do
  let msg = ET.MkEnvelope
        { ET.version = 0
        , ET.message = ET.DeleteAll
        }

  withMVar connMVar $ \mConn -> for_ mConn $ \conn ->
    WS.sendBinaryData conn (ET.encodeEnvelope msg)

