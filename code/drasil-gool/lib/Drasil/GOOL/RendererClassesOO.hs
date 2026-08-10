{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Drasil.GOOL.RendererClassesOO (
  OORenderSym, RenderFile(..), PermElim(..), InternalGetSet(..),
  StateVarElim(..), ParentSpec, RenderClass(..), ClassElim(..), RenderMod(..),
  ModuleElim(..), OORenderMethod(..), OOMethodTypeSym(..)
) where

import Drasil.Shared.InterfaceCommon (Label, Block, Body, SVariable, SValue)
import qualified Drasil.GOOL.InterfaceGOOL as IG (Class, CSStateVar,
  OOVariableValue, OOValueExpression(..), InternalValueExp(..), FileSym(..),
  GetSet(..), ObserverPattern(..), StrategyPattern(..))
import Drasil.Shared.AST (AttachmentTag, TypeData, ParamData, FuncData, ModData)
import Drasil.Shared.State (FS, CS, VS, MS)

import Text.PrettyPrint.HughesPJ (Doc)

import Drasil.Shared.RendererClassesCommon (MSMthdType, CommonRenderSym,
  BlockCommentSym(..), MethodTypeSym(..), RenderMethod(..))

class (CommonRenderSym r vis stmt mthd,
  IG.FileSym r vis stmt mthd stvr attch mod file, IG.InternalValueExp r,
  IG.GetSet r, IG.ObserverPattern r stmt, IG.StrategyPattern r stmt,
  IG.OOVariableValue r, IG.OOValueExpression r, RenderClass r vis mthd stvr,
  ClassElim r, RenderFile r file, InternalGetSet r,
  OORenderMethod r vis mthd attch, RenderMod r, ModuleElim r,
  StateVarElim r stvr, PermElim r attch
  ) => OORenderSym r vis stmt mthd stvr attch mod file

-- OO-Only Typeclasses --

class (BlockCommentSym r) => RenderFile r file | r -> file where
  -- top and bottom are only used for pre-processor guards for C++ header
  -- files. FIXME: Remove them (generation of pre-processor guards can be
  -- handled by fileDoc instead)
  top :: r ModData -> r Block
  bottom :: r Block

  commentedMod :: FS (r file) -> FS (r Doc) -> FS (r file)

  fileFromData :: FilePath -> FS (r ModData) -> FS (r file)

class PermElim r attch where
  perm :: r attch -> Doc
  binding :: r attch -> AttachmentTag

class InternalGetSet r where
  getFunc :: SVariable r -> VS (r FuncData)
  setFunc :: VS (r TypeData) -> SVariable r -> SValue r -> VS (r FuncData)

class (MethodTypeSym r) => OOMethodTypeSym r where
  construct :: Label -> MSMthdType r

class (RenderMethod r mthd, OOMethodTypeSym r) => OORenderMethod r vis mthd attch | r -> vis attch where
  -- | Main method?, name, public/private, classLevel/instanceLevel,
  --   return type, parameters, body
  intMethod     :: Bool -> Label -> r vis -> r attch ->
    MSMthdType r -> [MS (r ParamData)] -> MS (r Body) -> MS (r mthd)
  -- | True for main function, name, public/private, classLevel/instanceLevel,
  --   return type, parameters, body
  intFunc       :: Bool -> Label -> r vis -> r attch
    -> MSMthdType r -> [MS (r ParamData)] -> MS (r Body) -> MS (r mthd)

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

class RenderMod r where
  modFromData :: String -> FS Doc -> FS (r ModData)
  updateModuleDoc :: (Doc -> Doc) -> r ModData -> r ModData

class ModuleElim r where
  module' :: r ModData -> Doc
