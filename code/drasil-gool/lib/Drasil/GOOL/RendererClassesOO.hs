{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Drasil.GOOL.RendererClassesOO (
  OORenderSym, RenderFile(..), PermElim(..), InternalGetSet(..),
  StateVarElim(..), ParentSpec, RenderClass(..), ClassElim(..), RenderMod(..),
  ModuleElim(..), OORenderMethod(..), OOMethodTypeSym(..)
) where

import Drasil.Shared.InterfaceCommon (Label, Block, MSBody, SVariable, SValue)
import qualified Drasil.GOOL.InterfaceGOOL as IG (File, Module, FSModule, Class,
  SClass, CSStateVar, OOVariableValue, OOValueExpression(..),
  InternalValueExp(..), FileSym(..), GetSet(..), ObserverPattern(..),
  StrategyPattern(..))
import Drasil.Shared.AST (AttachmentTag, TypeData, ParamData, FuncData)
import Drasil.Shared.State (FS, CS, VS, MS)

import Text.PrettyPrint.HughesPJ (Doc)

import Drasil.Shared.RendererClassesCommon (MSMthdType, CommonRenderSym,
  BlockCommentSym(..), MethodTypeSym(..), RenderMethod(..))

class (CommonRenderSym r vis smt md, IG.FileSym r vis smt md svr att,
  IG.InternalValueExp r, IG.GetSet r, IG.ObserverPattern r smt,
  IG.StrategyPattern r smt, IG.OOVariableValue r,
  IG.OOValueExpression r, RenderClass r vis md svr, ClassElim r, RenderFile r,
  InternalGetSet r, OORenderMethod r vis md att, RenderMod r, ModuleElim r,
  StateVarElim r svr, PermElim r att
  ) => OORenderSym r vis smt md svr att

-- OO-Only Typeclasses --

class (BlockCommentSym r) => RenderFile r where
  -- top and bottom are only used for pre-processor guards for C++ header
  -- files. FIXME: Remove them (generation of pre-processor guards can be
  -- handled by fileDoc instead)
  top :: r IG.Module -> r Block
  bottom :: r Block

  commentedMod :: FS (r IG.File) -> FS (r Doc) -> FS (r IG.File)

  fileFromData :: FilePath -> IG.FSModule r -> FS (r IG.File)

class PermElim r att where
  perm :: r att -> Doc
  binding :: r att -> AttachmentTag

class InternalGetSet r where
  getFunc :: SVariable r -> VS (r FuncData)
  setFunc :: VS (r TypeData) -> SVariable r -> SValue r -> VS (r FuncData)

class (MethodTypeSym r) => OOMethodTypeSym r where
  construct :: Label -> MSMthdType r

class (RenderMethod r md, OOMethodTypeSym r) => OORenderMethod r vis md att | r -> vis att where
  -- | Main method?, name, public/private, classLevel/instanceLevel,
  --   return type, parameters, body
  intMethod     :: Bool -> Label -> r vis -> r att ->
    MSMthdType r -> [MS (r ParamData)] -> MSBody r -> MS (r md)
  -- | True for main function, name, public/private, classLevel/instanceLevel,
  --   return type, parameters, body
  intFunc       :: Bool -> Label -> r vis -> r att
    -> MSMthdType r -> [MS (r ParamData)] -> MSBody r -> MS (r md)

  destructor :: [IG.CSStateVar r svr] -> MS (r md)

class StateVarElim r svr | r -> svr where
  stateVar :: r svr -> Doc

type ParentSpec = Doc

class (BlockCommentSym r) => RenderClass r vis md svr | r -> vis md svr where
  -- class name, visibility, parent, state variables, constructor(s), methods
  intClass :: Label -> r vis -> r ParentSpec -> [IG.CSStateVar r svr]
    -> [MS (r md)] -> [MS (r md)] -> IG.SClass r

  inherit :: Maybe Label -> r ParentSpec
  implements :: [Label] -> r ParentSpec

  commentedClass :: CS (r Doc) -> IG.SClass r -> IG.SClass r

class ClassElim r where
  class' :: r IG.Class -> Doc

class RenderMod r where
  modFromData :: String -> FS Doc -> IG.FSModule r
  updateModuleDoc :: (Doc -> Doc) -> r IG.Module -> r IG.Module

class ModuleElim r where
  module' :: r IG.Module -> Doc
