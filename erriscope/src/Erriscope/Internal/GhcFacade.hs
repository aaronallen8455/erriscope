{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}
module Erriscope.Internal.GhcFacade
  ( module Ghc
  , pattern RealSrcSpan'
  ) where

#if MIN_VERSION_ghc(9,0,0)
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

pattern RealSrcSpan' :: RealSrcSpan -> SrcSpan
#if MIN_VERSION_ghc(9,0,0)
pattern RealSrcSpan' loc <- RealSrcSpan loc _
#else
pattern RealSrcSpan' loc = RealSrcSpan loc
#endif
