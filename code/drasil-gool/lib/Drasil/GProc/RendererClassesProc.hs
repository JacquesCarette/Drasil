{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Drasil.GProc.RendererClassesProc (
  ProcRenderSym, RenderFile(..), RenderMod(..), ModuleElim(..),
  ProcRenderMethod(..)
) where

import Drasil.Shared.InterfaceCommon (Label, Block, Body)
import qualified Drasil.GProc.InterfaceProc as IP (File, Module, FileSym(..))
import Drasil.Shared.State (FS, MS)
import Drasil.Shared.AST (ParamData)

import Text.PrettyPrint.HughesPJ (Doc)

import Drasil.Shared.RendererClassesCommon (CommonRenderSym, BlockCommentSym(..),
  RenderMethod(..), MSMthdType)

class (CommonRenderSym r vis smt md, IP.FileSym r vis smt md, RenderFile r,
  RenderMod r, ModuleElim r, ProcRenderMethod r vis md
  ) => ProcRenderSym r vis smt md
-- Procedural-Only Typeclasses --

class (BlockCommentSym r) => RenderFile r where
  -- top and bottom are only used for pre-processor guards for C++ header
  -- files. FIXME: Remove them (generation of pre-processor guards can be
  -- handled by fileDoc instead)
  top :: r IP.Module -> r Block
  bottom :: r Block

  commentedMod :: FS (r IP.File) -> FS (r Doc) -> FS (r IP.File)

  fileFromData :: FilePath -> FS (r IP.Module) -> FS (r IP.File)

class RenderMod r where
  modFromData :: String -> FS Doc -> FS (r IP.Module)
  updateModuleDoc :: (Doc -> Doc) -> r IP.Module -> r IP.Module

class ModuleElim r where
  module' :: r IP.Module -> Doc

class (RenderMethod r md) => ProcRenderMethod r vis md | r -> vis where
  -- | Main method?, name, public/private,
  --   return type, parameters, body
  intFunc     :: Bool -> Label -> r vis -> MSMthdType r ->
    [MS (r ParamData)] -> MS (r Body) -> MS (r md)
