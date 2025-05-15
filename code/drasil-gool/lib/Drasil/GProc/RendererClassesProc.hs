{-# LANGUAGE TypeFamilies #-}

module Drasil.GProc.RendererClassesProc (
  ProcRenderSym, RenderFile(..), RenderMod(..), ModuleElim(..),
  ProcRenderMethod(..)
) where

import Drasil.Shared.InterfaceCommon (Label, SMethod, MSParameter,
  MSBody, BlockSym(..), VisibilitySym(..))
import qualified Drasil.GProc.InterfaceProc as IP (SFile, FSModule, FileSym(..),
  ModuleSym(..))
import Drasil.Shared.State (FS)

import Text.PrettyPrint.HughesPJ (Doc)

import Drasil.Shared.RendererClassesCommon (CommonRenderSym, BlockCommentSym(..),
  RenderMethod(..), MSMthdType)

class (CommonRenderSym r, IP.FileSym r, RenderFile r, RenderMod r, ModuleElim r,
  ProcRenderMethod r
  ) => ProcRenderSym r

-- Procedural-Only Typeclasses --

class (BlockCommentSym r) => RenderFile r where
  -- top and bottom are only used for pre-processor guards for C++ header 
  -- files. FIXME: Remove them (generation of pre-processor guards can be 
  -- handled by fileDoc instead)
  top :: r (IP.Module r) -> r (Block r) 
  bottom :: r (Block r)

  commentedMod :: IP.SFile r -> FS (r (BlockComment r)) -> IP.SFile r

  fileFromData :: FilePath -> IP.FSModule r -> IP.SFile r

class RenderMod r where
  modFromData :: String -> FS Doc -> IP.FSModule r
  updateModuleDoc :: (Doc -> Doc) -> r (IP.Module r) -> r (IP.Module r)
  
class ModuleElim r where
  module' :: r (IP.Module r) -> Doc

class (RenderMethod r) => ProcRenderMethod r where
  -- | Main method?, name, public/private,
  --   return type, parameters, body
  intFunc     :: Bool -> Label -> r (Visibility r) -> MSMthdType r ->
    [MSParameter r] -> MSBody r -> SMethod r
