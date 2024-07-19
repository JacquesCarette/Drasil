{-# LANGUAGE TypeFamilies #-}

module Drasil.GOOL.RendererClassesProc (
  OORenderSym, RenderFile(..), RenderMod(..), ModuleElim(..)
) where

import Drasil.GOOL.InterfaceCommon (BlockSym(..))
import qualified Drasil.GOOL.InterfaceProc as IP (SFile, FSModule, FileSym(..),
  ModuleSym(..))
import Drasil.GOOL.State (FS)

import Text.PrettyPrint.HughesPJ (Doc)

import Drasil.GOOL.RendererClassesCommon (CommonRenderSym, BlockCommentSym(..))

class (CommonRenderSym r, IP.FileSym r, RenderFile r, RenderMod r, ModuleElim r
  ) => OORenderSym r

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
