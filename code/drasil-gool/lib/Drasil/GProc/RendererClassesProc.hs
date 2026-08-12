{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Drasil.GProc.RendererClassesProc (
  ProcRenderSym, RenderFile(..), RenderMod(..), ModuleElim(..),
  ProcRenderMethod(..)
) where

import Drasil.Shared.InterfaceCommon (Label, Block, Body, MethodSym, BodySym)
import qualified Drasil.GProc.InterfaceProc as IP (FileSym(..), ModuleSym)
import Drasil.Shared.State (FS, MS)
import Drasil.Shared.AST (ParamData)

import Text.PrettyPrint.HughesPJ (Doc)

import Drasil.Shared.RendererClassesCommon (CommonRenderSym, BlockCommentSym(..),
  RenderMethod(..), MSMthdType)

class (CommonRenderSym r vis stmt mthd, BodySym r stmt, MethodSym r vis mthd,
  IP.ModuleSym r mod mthd, IP.FileSym r file mod, RenderFile r file mod,
  RenderMod r mod, ModuleElim r mod, ProcRenderMethod r vis mthd
  ) => ProcRenderSym r vis stmt mthd file mod
-- Procedural-Only Typeclasses --

class (BlockCommentSym r) => RenderFile r file mod | r -> file mod where
  -- top and bottom are only used for pre-processor guards for C++ header
  -- files. FIXME: Remove them (generation of pre-processor guards can be
  -- handled by fileDoc instead)
  top :: r mod -> r Block
  bottom :: r Block

  commentedMod :: FS (r file) -> FS (r Doc) -> FS (r file)

  fileFromData :: FilePath -> FS (r mod) -> FS (r file)

class RenderMod r mod | r -> mod where
  modFromData :: String -> FS Doc -> FS (r mod)
  updateModuleDoc :: (Doc -> Doc) -> r mod -> r mod

class ModuleElim r mod | r -> mod where
  module' :: r mod -> Doc

class (RenderMethod r mthd) => ProcRenderMethod r vis mthd | r -> vis where
  -- | Main method?, name, public/private,
  --   return type, parameters, body
  intFunc     :: Bool -> Label -> r vis -> MSMthdType r ->
    [MS (r ParamData)] -> MS (r Body) -> MS (r mthd)
