{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Drasil.GOOL.RendererClassesOO (
  OORenderSym, RenderFile(..), PermElim(..), InternalGetSet(..),
  StateVarElim(..), ParentSpec, RenderClass(..), ClassElim(..), RenderMod(..),
  ModuleElim(..), OORenderMethod(..), OOMethodTypeSym(..)
) where

import Drasil.Shared.InterfaceCommon (Label, Block, TypeSym, ParameterSym,
  MethodSym, ValueSym, VariableSym, VisibilitySym)
import qualified Drasil.GOOL.InterfaceGOOL as IG (CSStateVar, OOTypeSym,
  OOVariableSym, SelfSym, OOValueExpression(..), InternalValueExp(..),
  FileSym(..), GetSet(..), ObserverPattern(..), StrategyPattern(..), ModuleSym,
  OOMethodSym, AttachmentSym, StateVarSym, ClassSym)
import Drasil.Shared.AST (AttachmentTag, FuncData)
import Drasil.Shared.State (FS, CS, VS, MS)

import Text.PrettyPrint.HughesPJ (Doc)

import Drasil.Shared.RendererClassesCommon (CommonRenderSym, MethodTypeSym(..),
  RenderMethod(..))

class (CommonRenderSym r mthd vis param bod block stmt var scope val binder typ,
  ParameterSym r param var, MethodSym r mthd vis param bod var typ,
  IG.OOMethodSym r mthd attch vis param bod var val typ, VisibilitySym r vis,
  IG.AttachmentSym r attch, IG.StateVarSym r stvr attch vis var val,
  IG.ClassSym r cls stvr mthd, IG.ModuleSym r mod cls mthd, IG.FileSym r file mod,
  ValueSym r val typ, IG.InternalValueExp r var val typ, IG.GetSet r var val,
  IG.ObserverPattern r stmt typ, IG.StrategyPattern r bod block var val,
  VariableSym r var typ, TypeSym r typ, IG.OOTypeSym r typ,
  IG.OOVariableSym r var val typ, IG.SelfSym r var,
  IG.OOValueExpression r var val typ, RenderClass r cls stvr mthd vis,
  ClassElim r cls, RenderFile r file mod, InternalGetSet r var val typ,
  MethodTypeSym r typ, OOMethodTypeSym r typ, RenderMethod r mthd,
  OORenderMethod r mthd attch vis param bod typ, RenderMod r mod,
  ModuleElim r mod, StateVarElim r stvr, PermElim r attch
  ) => OORenderSym r file mod cls stvr mthd attch vis param bod block stmt var scope val binder typ

-- OO-Only Typeclasses --

class RenderFile r file mod | r -> file mod where
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

class InternalGetSet r var val typ | r -> var val typ where
  getFunc :: VS (r var) -> VS (r FuncData)
  setFunc :: VS (r typ) -> VS (r var) -> VS (r val) -> VS (r FuncData)

class OOMethodTypeSym r typ | r -> typ where
  construct :: Label -> MS (r typ)

class OORenderMethod r mthd attch vis param bod typ | r -> mthd attch vis param bod typ where
  -- | Main method?, name, public/private, classLevel/instanceLevel,
  --   return type, parameters, body
  intMethod     :: Bool -> Label -> r vis -> r attch ->
    MS (r typ) -> [MS (r param)] -> MS (r bod) -> MS (r mthd)
  -- | True for main function, name, public/private, classLevel/instanceLevel,
  --   return type, parameters, body
  intFunc       :: Bool -> Label -> r vis -> r attch
    -> MS (r typ) -> [MS (r param)] -> MS (r bod) -> MS (r mthd)

  destructor :: [IG.CSStateVar r stvr] -> MS (r mthd)

class StateVarElim r stvr | r -> stvr where
  stateVar :: r stvr -> Doc

type ParentSpec = Doc

class RenderClass r cls stvr mthd vis | r -> cls stvr mthd vis where
  -- class name, visibility, parent, state variables, constructor(s), methods
  intClass :: Label -> r vis -> r ParentSpec -> [IG.CSStateVar r stvr]
    -> [MS (r mthd)] -> [MS (r mthd)] -> CS (r cls)

  inherit :: Maybe Label -> r ParentSpec
  implements :: [Label] -> r ParentSpec

  commentedClass :: CS (r Doc) -> CS (r cls) -> CS (r cls)

class ClassElim r cls where
  class' :: r cls -> Doc

class RenderMod r mod | r -> mod where
  modFromData :: String -> FS Doc -> FS (r mod)
  updateModuleDoc :: (Doc -> Doc) -> r mod -> r mod

class ModuleElim r mod | r -> mod where
  module' :: r mod -> Doc
