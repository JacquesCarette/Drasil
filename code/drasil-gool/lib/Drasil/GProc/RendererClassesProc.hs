{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Drasil.GProc.RendererClassesProc (
  ProcRenderSym, RenderFile(..), RenderMod(..), ModuleElim(..),
  ProcRenderMethod(..)
) where

import Drasil.Shared.InterfaceCommon (Label, Block, ParameterSym, VisibilitySym,
  MethodSym)
import qualified Drasil.GProc.InterfaceProc as IP (FileSym(..), ModuleSym)
import Drasil.Shared.State (FS, MS)

import Text.PrettyPrint.HughesPJ (Doc)

import Drasil.Shared.RendererClassesCommon (CommonRenderSym, RenderMethod(..))

class (CommonRenderSym r vis scope typ param val stmt mthd bod block, ParameterSym r param,
  VisibilitySym r vis, MethodSym r vis typ param mthd bod, IP.ModuleSym r mod mthd,
  IP.FileSym r file mod, RenderFile r file mod, RenderMod r mod,
  ModuleElim r mod, RenderMethod r mthd, ProcRenderMethod r vis typ param mthd bod
  ) => ProcRenderSym r vis scope typ param val stmt mthd file mod bod block
-- Procedural-Only Typeclasses --

class RenderFile r file mod | r -> file mod where
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

class ProcRenderMethod r vis typ param mthd bod | r -> vis typ param bod where
  -- | Main method?, name, public/private,
  --   return type, parameters, body
  intFunc     :: Bool -> Label -> r vis -> MS (r typ) ->
    [MS (r param)] -> MS (r bod) -> MS (r mthd)
