{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}
module Erriscope.Internal.GhcFacade
  ( module Ghc
  , pattern RealSrcSpan'
  , addLogAction
  , addPhaseHook
  , showSDocForUser'
  ) where

#if MIN_VERSION_ghc(9,2,0)
import GHC.Data.FastString as Ghc
import GHC.Driver.Env.Types as Ghc hiding (Hsc)
import GHC.Driver.Hooks as Ghc
import GHC.Driver.Phases as Ghc
import GHC.Driver.Pipeline as Ghc
import GHC.Driver.Plugins as Ghc
import GHC.Driver.Ppr as Ghc
import GHC.Driver.Session as Ghc
import GHC.Hs as Ghc
import GHC.Tc.Types as Ghc hiding (TcPlugin)
import GHC.Types.SrcLoc as Ghc
import GHC.Utils.Error as Ghc
import GHC.Utils.Logger as Ghc
import GHC.Utils.Outputable as Ghc
import GHC.Unit.Module as Ghc
import GHC.Unit.Module.ModSummary as Ghc
import GHC.Unit.State as Ghc
#elif MIN_VERSION_ghc(9,0,0)
import GHC.Data.FastString as Ghc
import GHC.Driver.Hooks as Ghc
import GHC.Driver.Phases as Ghc
import GHC.Driver.Pipeline as Ghc
import GHC.Driver.Plugins as Ghc
import GHC.Driver.Session as Ghc
import GHC.Driver.Types as Ghc hiding (Hsc)
import GHC.Hs as Ghc
import GHC.Tc.Types as Ghc hiding (TcPlugin)
import GHC.Types.SrcLoc as Ghc
import GHC.Utils.Error as Ghc
import GHC.Utils.Outputable as Ghc
import GHC.Unit.Module as Ghc
#else
import FastString as Ghc
import Hooks as Ghc
import Plugins as Ghc
import ErrUtils as Ghc
import SrcLoc as Ghc
import Outputable as Ghc
import DynFlags as Ghc
import HscTypes as Ghc
import GHC.Hs as Ghc
import Module as Ghc
import DriverPipeline as Ghc
import DriverPhases as Ghc hiding (Hsc)
#endif

import           Control.Monad.IO.Class

pattern RealSrcSpan' :: RealSrcSpan -> SrcSpan
#if MIN_VERSION_ghc(9,0,0)
pattern RealSrcSpan' loc <- RealSrcSpan loc _
#else
pattern RealSrcSpan' loc = RealSrcSpan loc
#endif

addLogAction :: (DynFlags -> Severity -> SrcSpan -> SDoc -> IO ())
#if MIN_VERSION_ghc(9,2,0)
             -> HscEnv -> HscEnv
#else
             -> DynFlags -> DynFlags
#endif
addLogAction action env =
#if MIN_VERSION_ghc(9,2,0)
  let hook logAction dflags warnReason severity srcSpan msgDoc = do
        action dflags severity srcSpan msgDoc
        logAction (hsc_dflags env) warnReason severity srcSpan msgDoc
   in env
     { hsc_logger = pushLogHook hook $ hsc_logger env }
#elif MIN_VERSION_ghc(9,0,0)
  env
    { log_action = \dflags warnReason severity srcSpan msgDoc -> do
        action dflags severity srcSpan msgDoc
        log_action env dflags warnReason severity srcSpan msgDoc
    }
#else
  env
    { log_action = \dflags warnReason severity srcSpan pprStyle msgDoc -> do
        action dflags severity srcSpan msgDoc
        log_action env dflags warnReason severity srcSpan pprStyle msgDoc
    }
#endif

addPhaseHook :: (FilePath -> PhasePlus -> IO ())
#if MIN_VERSION_ghc(9,2,0)
             -> HscEnv -> HscEnv
#else
             -> DynFlags -> DynFlags
#endif
addPhaseHook hook env =
  env
#if MIN_VERSION_ghc(9,2,0)
    { hsc_hooks = (hsc_hooks env)
      { runPhaseHook = Just $ \phase filePath ->
          case runPhaseHook (hsc_hooks env) of
            Nothing -> runPhase phase filePath
            Just h -> h phase filePath
      }
#else
    { hooks = (hooks env)
      { runPhaseHook = Just $ \phase filePath dflags -> do
          liftIO $ hook filePath phase
          case runPhaseHook (hooks env) of
            Nothing -> runPhase phase filePath dflags
            Just h -> h phase filePath dflags
      }
#endif
    }

showSDocForUser' :: DynFlags -> PrintUnqualified -> SDoc -> String
showSDocForUser' dflags =
#if MIN_VERSION_ghc(9,2,0)
  showSDocForUser dflags emptyUnitState
#else
  showSDocForUser dflags
#endif
