{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
module Erriscope
  ( plugin
  ) where

import           Control.Concurrent.MVar
import           Control.Exception.Safe (throwIO, try, tryAny)
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable
import           Data.IORef
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set as S
import           Data.Time
import qualified Network.Socket as Sock
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as Stream
import qualified System.Directory as Dir
import           System.IO.Error (isResourceVanishedError)
import           System.IO.Unsafe

import qualified Erriscope.Internal.GhcFacade as Ghc
import qualified Erriscope.Types as ET

{-# NOINLINE connMVar #-}
connMVar :: MVar (Maybe WS.Connection)
connMVar = unsafePerformIO $ newMVar Nothing

data KnownFile =
  MkKnownFile
    { modName :: !(Maybe ET.ModuleName)
    -- ^ The ModuleName is a Maybe because errors might occur during the parsing
    -- phase in which case we may not know the module name but still want to have
    -- the file registered so that deleted file pruning will work.
    , emittedErrors :: !(S.Set (ET.ErrorBody, ET.Location))
    -- ^ It is necessary to track the errors that have been emitted for a file
    -- because when compiling with multiple targets in stack, the log action
    -- is run for each target which means duplicate errors would be emitted.
    }

{-# NOINLINE knownFilesMVar #-}
knownFilesMVar :: MVar (M.Map ET.FilePath KnownFile)
-- FilePath is a Maybe because fatal errors are not attached to a location
-- and are cleared at the start of any module compilation
knownFilesMVar = unsafePerformIO $ newMVar mempty

{-# NOINLINE lastDeletedFilePruning #-}
lastDeletedFilePruning :: IORef (Maybe UTCTime)
lastDeletedFilePruning = unsafePerformIO $ newIORef Nothing

{-# NOINLINE nonLocatedErrorsMVar #-}
nonLocatedErrorsMVar :: MVar (S.Set ET.ErrorBody)
nonLocatedErrorsMVar = unsafePerformIO $ newMVar mempty

plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
#if MIN_VERSION_ghc(9,2,0)
  { Ghc.driverPlugin = driverPlugin
#else
  { Ghc.dynflagsPlugin = driverPlugin
#endif
  , Ghc.pluginRecompile = Ghc.purePlugin
  , Ghc.parsedResultAction = parsedResult
  }

-- | Overrides the log action and the phase hook.
driverPlugin :: [Ghc.CommandLineOption]
             -> Ghc.HscEnv -> IO Ghc.HscEnv
driverPlugin opts env = do
  let ?port = ET.getPortFromArgs opts
  initializeWebsocket

  let hook filePath =
        beginCompilation (BS8.pack filePath)

  pure . Ghc.addCppPhaseHook hook
       $ Ghc.addLogAction (reportError $ Ghc.hsc_dflags env ) env

-- | When a module starts compiling, delete all existing errors for the file
-- and also prune deleted files.
beginCompilation :: (?port :: ET.Port) => ET.FilePath -> IO ()
beginCompilation modFile = do
  deleteErrorsForFile (Just modFile)
  modifyMVar_ knownFilesMVar $
    pure . M.insert modFile
      MkKnownFile { modName = Nothing , emittedErrors = mempty }
  pruneDeletedFiles
  deleteNonLocatedErrors

deleteNonLocatedErrors :: (?port :: ET.Port) => IO ()
deleteNonLocatedErrors = do
  modifyMVar_ nonLocatedErrorsMVar $ \nonLocatedErrors ->
    if S.null nonLocatedErrors
       then pure nonLocatedErrors
       else do
         deleteErrorsForFile Nothing
         pure mempty

-- | Check for any known files that have been deleted and remove it from the
-- server and known files map.
pruneDeletedFiles :: (?port :: ET.Port) => IO ()
pruneDeletedFiles = do
  now <- getCurrentTime
  mLastPruning <- readIORef lastDeletedFilePruning
  -- Only prune if 3 seconds have elapsed since the last time since it can be
  -- an expensive operation.
  when (maybe True ((> 3) . diffUTCTime now) mLastPruning) $ do
    writeIORef lastDeletedFilePruning (Just now)
    knownFiles <- readMVar knownFilesMVar
    let go filePath _ = do
          Dir.doesFileExist (BS8.unpack filePath) >>= \case
            True  -> pure ()
            False -> do
              deleteErrorsForFile (Just filePath)
              modifyMVar_ knownFilesMVar $ pure . M.delete filePath
    void $ M.traverseWithKey go knownFiles

-- | Opens the websocket connection
initializeWebsocket :: (?port :: ET.Port) => IO ()
initializeWebsocket =
  modifyMVar_ connMVar $ \case
    Just conn -> pure (Just conn)
    Nothing -> do
      eConn <- tryAny $ do
        -- Create and connect socket
        let hints = Sock.defaultHints {Sock.addrSocketType = Sock.Stream}
            -- Correct host and path.
            host = "127.0.0.1"
            fullHost = if ?port == 80 then host else host ++ ":" ++ show ?port
        addr:_ <- Sock.getAddrInfo (Just hints) (Just host) (Just $ show ?port)
        sock <- Sock.socket (Sock.addrFamily addr) Sock.Stream Sock.defaultProtocol
        Sock.setSocketOption sock Sock.NoDelay 1
        Sock.connect sock (Sock.addrAddress addr)
        -- TODO does the stream need to be closed when application shuts down?
        -- Could use `mkWeakMVar` if so
        stream <- Stream.makeSocketStream sock

        conn <-
          WS.newClientConnection
            stream
            fullHost
            "/"
            WS.defaultConnectionOptions
            []

        WS.sendTextData conn (BS8.pack "plugin")
        pure conn
      case eConn of
        Left _err -> do
            putStrLn "Unable to connect to erriscope server! Ensure you have 'erriscope-server' running."
            pure Nothing
        Right conn -> pure $ Just conn

-- | If a module passes the parsing phase then we get its proper name
parsedResult :: MonadIO m
             => [Ghc.CommandLineOption]
             -> Ghc.ModSummary
             -> Ghc.ParsedResult
             -> m Ghc.ParsedResult
parsedResult opts modSum parsed
  | Just srcFilePath <- fmap BS8.pack . Ghc.ml_hs_file $ Ghc.ms_location modSum
  = do
    let ?port = ET.getPortFromArgs opts
    let mModName = fmap (Ghc.bytesFS . Ghc.moduleNameFS . Ghc.unLoc)
                 . Ghc.hsmodName . Ghc.unLoc
                 $ Ghc.hpm_module (Ghc.parsedResultModule parsed)

    liftIO . modifyMVar_ knownFilesMVar $
      pure . M.insert srcFilePath
        MkKnownFile { modName = mModName, emittedErrors = mempty }
    -- It's necessary to clear errors here because the CPP phase doesn't run
    -- for modules that are re-compiled due to a change in another module.
    -- Note that parse errors cannot result from such a change, so they will
    -- not get duplicated on the server.
    liftIO $ deleteErrorsForFile (Just srcFilePath)
    pure parsed
  | otherwise = pure parsed

-- | Send error message to server
reportError :: (?port :: ET.Port)
            => Ghc.DynFlags -> Ghc.LogFlags -> Ghc.MessageClass -> Ghc.SrcSpan -> Ghc.SDoc -> IO ()
reportError dFlags _lFlags messageClass srcSpan msgDoc
  | Ghc.RealSrcSpan' rss <- srcSpan
  , Just errType <- getErrorType messageClass
  = do
    let modFile = Ghc.bytesFS $ Ghc.srcSpanFile rss
    mMsg <- modifyMVar knownFilesMVar $ \knownFiles -> do
      case M.lookup modFile knownFiles of
        Nothing -> pure (knownFiles, Nothing)
        Just knownFile -> do
          let errBody = BSL.toStrict . BSB.toLazyByteString . BSB.stringUtf8
                      $ Ghc.showSDocForUser' dFlags Ghc.neverQualify msgDoc
              loc = ET.MkLocation
                { ET.lineNum = fromIntegral $ Ghc.srcSpanStartLine rss
                , ET.colNum = fromIntegral $ Ghc.srcSpanStartCol rss
                }
          if (errBody, loc) `S.member` emittedErrors knownFile
             then pure (knownFiles, Nothing)
             else do
              let mModName = modName knownFile
              caret <- Ghc.showSDocForUser' dFlags Ghc.neverQualify
                   <$> Ghc.getCaretDiagnostic messageClass srcSpan
              let fileErr = ET.MkFileError
                    { ET.moduleName = mModName
                    , ET.filepath = Just modFile
                    , ET.errorMsg = ET.MkErrorMsg
                      { ET.body = errBody
                      , ET.caret = Just . BSL.toStrict . BSB.toLazyByteString
                                 $ BSB.stringUtf8 caret
                      , ET.errorType = errType
                      , ET.fileLocation = Just loc
                      }
                    }
                  errorMessage =
                    ET.MkEnvelope
                      { ET.version = 0
                      , ET.message = ET.AddError fileErr
                      }
                  addEmittedError kf =
                    kf { emittedErrors = S.insert (errBody, loc) $ emittedErrors kf }
                  newKnownFiles = M.adjust addEmittedError modFile knownFiles
              pure (newKnownFiles, Just errorMessage)

    traverse_ sendMessage mMsg

  -- Capture errors that don't have a location, such as cyclic imports.
  | Ghc.UnhelpfulSpan _ <- srcSpan
  , Just errType <- getErrorType messageClass
  = do
    let errBody = BSL.toStrict . BSB.toLazyByteString . BSB.stringUtf8
                  $ Ghc.showSDocForUser' dFlags Ghc.neverQualify msgDoc
    modifyMVar_ nonLocatedErrorsMVar $ \nonLocatedErrors ->
      if S.member errBody nonLocatedErrors
         then pure nonLocatedErrors
         else do
           let fileErr = ET.MkFileError
                 { ET.moduleName = Nothing
                 , ET.filepath = Nothing
                 , ET.errorMsg = ET.MkErrorMsg
                   { ET.body = errBody
                   , ET.caret = Nothing
                   , ET.errorType = errType
                   , ET.fileLocation = Nothing
                   }
                 }
               errorMessage =
                 ET.MkEnvelope
                   { ET.version = 0
                   , ET.message = ET.AddError fileErr
                   }
           sendMessage errorMessage
           pure (S.insert errBody nonLocatedErrors)

  | otherwise = pure ()
  where
    getErrorType = \case
      Ghc.MCDiagnostic sev _ _ -> case sev of
        Ghc.SevWarning -> Just ET.Warning
        Ghc.SevError   -> Just ET.Error
        _              -> Nothing
      Ghc.MCFatal -> Just ET.Error
      _ -> Nothing

-- | Remove all errors on the server for a specific file
deleteErrorsForFile :: (?port :: ET.Port) => Maybe ET.FilePath -> IO ()
deleteErrorsForFile mModFile = do
  let deleteMsg = ET.MkEnvelope
        { ET.version = 0
        , ET.message = ET.DeleteFile mModFile
        }
  sendMessage deleteMsg

sendMessage :: (?port :: ET.Port) => ET.Envelope -> IO ()
sendMessage msg = do
  isConnected <- withMVar connMVar $ pure . isJust
  unless isConnected initializeWebsocket

  eErr <- try . withMVar connMVar . traverse_ $ \conn ->
    WS.sendBinaryData conn (ET.encodeEnvelope msg)

  case eErr of
    Right _ -> pure ()
    Left err ->
      if isResourceVanishedError err
         then do
           _ <- swapMVar connMVar Nothing
           initializeWebsocket
           connected <- withMVar connMVar $ pure . isJust
           if connected
              then sendMessage msg
              else putStrLn "Unable to connect to erriscope server!"
         else throwIO err
