{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Drasil.GOOL.RendererClassesOO (
  OORenderSym, RenderFile(..), PermElim(..), InternalGetSet(..),
  StateVarElim(..), ParentSpec, RenderClass(..), ClassElim(..), RenderMod(..),
  ModuleElim(..), OORenderMethod(..), OOMethodTypeSym(..)
) where

import Drasil.Shared.InterfaceCommon (Label, Block, SVariable, SValue, MethodSym)
import qualified Drasil.GOOL.InterfaceGOOL as IG (Class, CSStateVar,
  OOVariableValue, OOValueExpression(..), InternalValueExp(..), FileSym(..),
  GetSet(..), ObserverPattern(..), StrategyPattern(..), ModuleSym, OOMethodSym,
  ClassSym)
import Drasil.Shared.AST (AttachmentTag, TypeData, ParamData, FuncData)
import Drasil.Shared.State (FS, CS, VS, MS)

import Text.PrettyPrint.HughesPJ (Doc)

import Drasil.Shared.RendererClassesCommon (MSMthdType, CommonRenderSym,
  BlockCommentSym(..), MethodTypeSym(..), RenderMethod(..))

class (CommonRenderSym r vis stmt mthd bod block, MethodSym r vis mthd bod,
  IG.OOMethodSym r vis mthd attch bod, IG.ClassSym r vis mthd stvr attch,
  IG.ModuleSym r mod mthd, IG.FileSym r file mod, IG.InternalValueExp r,
  IG.GetSet r, IG.ObserverPattern r stmt, IG.StrategyPattern r bod block,
  IG.OOVariableValue r, IG.OOValueExpression r, RenderClass r vis mthd stvr,
  ClassElim r, RenderFile r file mod, InternalGetSet r,
  OORenderMethod r vis mthd attch bod, RenderMod r mod, ModuleElim r mod,
  StateVarElim r stvr, PermElim r attch
  ) => OORenderSym r vis stmt mthd stvr attch file mod bod block

-- OO-Only Typeclasses --

class (BlockCommentSym r) => RenderFile r file mod | r -> file mod where
  -- top and bottom are only used for pre-processor guards for C++ header
  -- files. FIXME: Remove them (generation of pre-processor guards can be
  -- handled by fileDoc instead)
  top :: r mod -> r Block
  bottom :: r Block

  commentedMod :: FS (r file) -> FS (r Doc) -> FS (r file)

  fileFromData :: FilePath -> FS (r mod) -> FS (r file)

class PermElim r attch where
  perm :: r attch -> Doc
  binding :: r attch -> AttachmentTag

class InternalGetSet r where
  getFunc :: SVariable r -> VS (r FuncData)
  setFunc :: VS (r TypeData) -> SVariable r -> SValue r -> VS (r FuncData)

class (MethodTypeSym r) => OOMethodTypeSym r where
  construct :: Label -> MSMthdType r

class (RenderMethod r mthd, OOMethodTypeSym r) => OORenderMethod r vis mthd attch bod | r -> vis attch bod where
  -- | Main method?, name, public/private, classLevel/instanceLevel,
  --   return type, parameters, body
  intMethod     :: Bool -> Label -> r vis -> r attch ->
    MSMthdType r -> [MS (r ParamData)] -> MS (r bod) -> MS (r mthd)
  -- | True for main function, name, public/private, classLevel/instanceLevel,
  --   return type, parameters, body
  intFunc       :: Bool -> Label -> r vis -> r attch
    -> MSMthdType r -> [MS (r ParamData)] -> MS (r bod) -> MS (r mthd)

  destructor :: [IG.CSStateVar r stvr] -> MS (r mthd)

class StateVarElim r stvr | r -> stvr where
  stateVar :: r stvr -> Doc

type ParentSpec = Doc

class (BlockCommentSym r) => RenderClass r vis mthd stvr | r -> vis mthd stvr where
  -- class name, visibility, parent, state variables, constructor(s), methods
  intClass :: Label -> r vis -> r ParentSpec -> [IG.CSStateVar r stvr]
    -> [MS (r mthd)] -> [MS (r mthd)] -> CS (r IG.Class)

  inherit :: Maybe Label -> r ParentSpec
  implements :: [Label] -> r ParentSpec

  commentedClass :: CS (r Doc) -> CS (r IG.Class) -> CS (r IG.Class)

class ClassElim r where
  class' :: r IG.Class -> Doc

class RenderMod r mod | r -> mod where
  modFromData :: String -> FS Doc -> FS (r mod)
  updateModuleDoc :: (Doc -> Doc) -> r mod -> r mod

class ModuleElim r mod | r -> mod where
  module' :: r mod -> Doc
