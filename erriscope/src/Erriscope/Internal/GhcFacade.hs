{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}
module Erriscope.Internal.GhcFacade
  ( module Ghc
  , pattern RealSrcSpan'
  , addLogAction
  , addCppPhaseHook
  , showSDocForUser'
  ) where

import GHC.Data.FastString as Ghc
import GHC.Driver.Env.Types as Ghc hiding (Hsc)
import GHC.Driver.Hooks as Ghc
import GHC.Driver.Phases as Ghc
import GHC.Driver.Pipeline as Ghc
import GHC.Driver.Pipeline.Phases as Ghc
import GHC.Driver.Plugins as Ghc
import GHC.Driver.Ppr as Ghc
import GHC.Driver.Session as Ghc
import GHC.Hs as Ghc
import GHC.Tc.Types as Ghc hiding (TcPlugin, DefaultingPlugin)
import GHC.Types.SrcLoc as Ghc
import GHC.Utils.Error as Ghc
import GHC.Utils.Logger as Ghc
import GHC.Utils.Outputable as Ghc
import GHC.Unit.Module as Ghc
import GHC.Unit.Module.ModSummary as Ghc
import GHC.Unit.State as Ghc

import           Control.Monad.IO.Class

pattern RealSrcSpan' :: RealSrcSpan -> SrcSpan
pattern RealSrcSpan' loc <- RealSrcSpan loc _

addLogAction :: (LogFlags -> MessageClass -> SrcSpan -> SDoc -> IO ())
             -> HscEnv -> HscEnv
addLogAction action env =
  let hook logAction lFlags messageClass srcSpan msgDoc = do
        action lFlags messageClass srcSpan msgDoc
        logAction lFlags messageClass srcSpan msgDoc
   in env { hsc_logger = pushLogHook hook $ hsc_logger env }

addCppPhaseHook :: (FilePath -> IO ())
             -> HscEnv -> HscEnv
addCppPhaseHook hook env =
  env
    { hsc_hooks = (hsc_hooks env)
      { runPhaseHook = Just $ Ghc.PhaseHook $ \tPhase -> do
          case tPhase of
            Ghc.T_Cpp _pipeEnv _hscEnv filePath -> liftIO $ hook filePath
            _ -> pure ()
          case runPhaseHook (hsc_hooks env) of
            Nothing -> runPhase tPhase
            Just (Ghc.PhaseHook h) -> h tPhase
      }
    }

showSDocForUser' :: DynFlags -> NamePprCtx -> SDoc -> String
showSDocForUser' dflags =
  showSDocForUser dflags emptyUnitState
