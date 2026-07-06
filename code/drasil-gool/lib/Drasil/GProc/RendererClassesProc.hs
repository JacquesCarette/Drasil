{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Drasil.GProc.RendererClassesProc (
  ProcRenderSym, RenderFile(..), RenderMod(..), ModuleElim(..),
  ProcRenderMethod(..)
) where

import Drasil.Shared.InterfaceCommon (Label, SMethod, MSBody, BlockSym(..))
import qualified Drasil.GProc.InterfaceProc as IP (SFile, FSModule, FileSym(..),
  ModuleSym(..))
import Drasil.Shared.State (FS, MS)

import Text.PrettyPrint.HughesPJ (Doc)

import Drasil.Shared.RendererClassesCommon (CommonRenderSym, BlockCommentSym(..),
  RenderMethod(..), MSMthdType)

class (CommonRenderSym r tp vis smt par, IP.FileSym r tp vis smt par,
  RenderFile r, RenderMod r, ModuleElim r, ProcRenderMethod r tp vis par
  ) => ProcRenderSym r tp vis smt par

-- Procedural-Only Typeclasses --

class (BlockCommentSym r) => RenderFile r where
  -- top and bottom are only used for pre-processor guards for C++ header
  -- files. FIXME: Remove them (generation of pre-processor guards can be
  -- handled by fileDoc instead)
  top :: r (IP.Module r) -> r (Block r)
  bottom :: r (Block r)

  commentedMod :: IP.SFile r -> FS (r Doc) -> IP.SFile r

  fileFromData :: FilePath -> IP.FSModule r -> IP.SFile r

class RenderMod r where
  modFromData :: String -> FS Doc -> IP.FSModule r
  updateModuleDoc :: (Doc -> Doc) -> r (IP.Module r) -> r (IP.Module r)

class ModuleElim r where
  module' :: r (IP.Module r) -> Doc

class (RenderMethod r tp) => ProcRenderMethod r tp vis par | r -> vis par where
  -- | Main method?, name, public/private,
  --   return type, parameters, body
  intFunc     :: Bool -> Label -> r vis -> MSMthdType r ->
    [MS (r par)] -> MSBody r -> SMethod r
