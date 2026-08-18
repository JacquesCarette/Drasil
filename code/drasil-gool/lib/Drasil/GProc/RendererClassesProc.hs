{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Drasil.GProc.RendererClassesProc (
  ProcRenderSym, RenderFile(..), RenderMod(..), ModuleElim(..),
  ProcRenderMethod(..)
) where

import Drasil.Shared.InterfaceCommon (Label, Block, Body)
import qualified Drasil.GProc.InterfaceProc as IP (Module, FileSym(..))
import Drasil.Shared.State (FS, MS)
import Drasil.Shared.AST (ParamData)

import Text.PrettyPrint.HughesPJ (Doc)

import Drasil.Shared.RendererClassesCommon (CommonRenderSym, BlockCommentSym(..),
  RenderMethod(..), MSMthdType)

class (CommonRenderSym r vis stmt mthd, IP.FileSym r vis stmt mthd file,
  RenderFile r file, RenderMod r, ModuleElim r, ProcRenderMethod r vis mthd
  ) => ProcRenderSym r vis stmt mthd file
-- Procedural-Only Typeclasses --

class (BlockCommentSym r) => RenderFile r file | r -> file where
  -- top and bottom are only used for pre-processor guards for C++ header
  -- files. FIXME: Remove them (generation of pre-processor guards can be
  -- handled by fileDoc instead)
  top :: r IP.Module -> r Block
  bottom :: r Block

  commentedMod :: FS (r file) -> FS (r Doc) -> FS (r file)

  fileFromData :: FilePath -> FS (r IP.Module) -> FS (r file)

class RenderMod r where
  modFromData :: String -> FS Doc -> FS (r IP.Module)
  updateModuleDoc :: (Doc -> Doc) -> r IP.Module -> r IP.Module

class ModuleElim r where
  module' :: r IP.Module -> Doc

class (RenderMethod r mthd) => ProcRenderMethod r vis mthd | r -> vis where
  -- | Main method?, name, public/private,
  --   return type, parameters, body
  intFunc     :: Bool -> Label -> r vis -> MSMthdType r ->
    [MS (r ParamData)] -> MS (r Body) -> MS (r mthd)
