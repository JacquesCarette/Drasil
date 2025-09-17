{-# LANGUAGE TypeFamilies #-}

module Drasil.GProc.RendererClassesProc (
  ProcRenderSym, RenderFile(..), RenderMod(..), ModuleElim(..),
  ProcRenderMethod(..)
) where

import Drasil.Shared.InterfaceCommon (Label, Method, Parameter,
  Body, BlockSym(..), VisibilitySym(..))
import qualified Drasil.GProc.InterfaceProc as IP (File, Module, FileSym(..),
  ModuleSym(..))
import Drasil.Shared.State (FS)

import Text.PrettyPrint.HughesPJ (Doc)

import Drasil.Shared.RendererClassesCommon (CommonRenderSym, BlockCommentSym(..),
  RenderMethod(..), MethodTypeSym(MethodType))

class (CommonRenderSym r, IP.FileSym r, RenderFile r, RenderMod r, ModuleElim r,
  ProcRenderMethod r
  ) => ProcRenderSym r

-- Procedural-Only Typeclasses --

class (BlockCommentSym r) => RenderFile r where
  -- top and bottom are only used for pre-processor guards for C++ header 
  -- files. FIXME: Remove them (generation of pre-processor guards can be 
  -- handled by fileDoc instead)
  top :: IP.Module r -> Block r 
  bottom :: Block r

  commentedMod :: IP.File r -> FS (BlockComment r) -> IP.File r

  fileFromData :: FilePath -> IP.Module r -> IP.File r

class RenderMod r where
  modFromData :: String -> FS Doc -> IP.Module r
  updateModuleDoc :: (Doc -> Doc) -> IP.Module r -> IP.Module r
  
class ModuleElim r where
  module' :: IP.Module r -> Doc

class (RenderMethod r) => ProcRenderMethod r where
  -- | Main method?, name, public/private,
  --   return type, parameters, body
  intFunc     :: Bool -> Label -> Visibility r -> MethodType r ->
    [Parameter r] -> Body r -> Method r
