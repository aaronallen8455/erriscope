{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
module Erriscope
  ( plugin
  ) where

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable
import qualified Data.Map.Strict as M
import qualified Network.Socket as S
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as Stream
import qualified System.Directory as Dir
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
  initializeWebsocket 8083

  deleteAll

  putStrLn "Driver plugin"
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
    pure . M.insertWith (<|>) modFile Nothing
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
      let hints = S.defaultHints {S.addrSocketType = S.Stream}
          -- Correct host and path.
          host = "127.0.0.1"
          fullHost = if port == 80 then host else host ++ ":" ++ show port
      addr:_ <- S.getAddrInfo (Just hints) (Just host) (Just $ show port)
      sock   <- S.socket (S.addrFamily addr) S.Stream S.defaultProtocol
      S.setSocketOption sock S.NoDelay 1
      S.connect sock (S.addrAddress addr)
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
      pure . M.insert srcFilePath mModName
    pure parsedMod
  | otherwise = pure parsedMod

-- | Send error message to server
reportError :: Ghc.DynFlags -> Ghc.Severity -> Ghc.SrcSpan -> Ghc.SDoc -> IO ()
reportError dynFlags severity srcSpan msgDoc
  | Ghc.RealSrcSpan' rss <- srcSpan
  , Just errType <- getErrorType severity
  = do
    let modFile = Ghc.bytesFS $ Ghc.srcSpanFile rss
    mModName <- fmap join . withMVar knownFilesMVar
              $ pure . M.lookup modFile
    caret <- Ghc.showSDocForUser dynFlags Ghc.alwaysQualify
         <$> Ghc.getCaretDiagnostic severity srcSpan
    let errBody = BSL.toStrict . BSB.toLazyByteString . BSB.stringUtf8
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

