{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Drasil.GOOL.RendererClassesOO (
  OORenderSym, RenderFile(..), PermElim(..), InternalGetSet(..),
  StateVarElim(..), ParentSpec, RenderClass(..), ClassElim(..), RenderMod(..),
  ModuleElim(..), OORenderMethod(..), OOMethodTypeSym(..)
) where

import Drasil.Shared.InterfaceCommon (Label, MSBody, SVariable, SValue, SMethod,
  BlockSym(..), FunctionSym(..))
import qualified Drasil.GOOL.InterfaceGOOL as IG (SFile, FSModule, SClass,
  CSStateVar, OOVariableValue, OOValueExpression(..), InternalValueExp(..),
  FileSym(..), ModuleSym(..), ClassSym(..), AttachmentSym(..), GetSet(..),
  StateVarSym(..), ObserverPattern(..), StrategyPattern(..))
import Drasil.Shared.AST (AttachmentTag)
import Drasil.Shared.State (FS, CS, VS, MS)

import Text.PrettyPrint.HughesPJ (Doc)

import Drasil.Shared.RendererClassesCommon (MSMthdType, CommonRenderSym,
  BlockCommentSym(..), MethodTypeSym(..), RenderMethod(..))

class (CommonRenderSym r tp vis smt par, IG.FileSym r tp vis smt par,
  IG.InternalValueExp r tp, IG.GetSet r tp, IG.ObserverPattern r tp smt,
  IG.StrategyPattern r tp smt, IG.OOVariableValue r tp,
  IG.OOValueExpression r tp, RenderClass r vis, ClassElim r, RenderFile r,
  InternalGetSet r tp, OORenderMethod r tp vis par, RenderMod r, ModuleElim r,
  StateVarElim r, PermElim r
  ) => OORenderSym r tp vis smt par

-- OO-Only Typeclasses --

class (BlockCommentSym r) => RenderFile r where
  -- top and bottom are only used for pre-processor guards for C++ header
  -- files. FIXME: Remove them (generation of pre-processor guards can be
  -- handled by fileDoc instead)
  top :: r (IG.Module r) -> r (Block r)
  bottom :: r (Block r)

  commentedMod :: IG.SFile r -> FS (r Doc) -> IG.SFile r

  fileFromData :: FilePath -> IG.FSModule r -> IG.SFile r

class PermElim r where
  perm :: r (IG.Attachment r) -> Doc
  binding :: r (IG.Attachment r) -> AttachmentTag

class InternalGetSet r tp | r -> tp where
  getFunc :: SVariable r -> VS (r (Function r))
  setFunc :: VS (r tp) -> SVariable r -> SValue r -> VS (r (Function r))

class (MethodTypeSym r tp) => OOMethodTypeSym r tp where
  construct :: Label -> MSMthdType r

class (RenderMethod r tp, OOMethodTypeSym r tp) => OORenderMethod r tp vis par | r -> vis par where
  -- | Main method?, name, public/private, classLevel/instanceLevel,
  --   return type, parameters, body
  intMethod     :: Bool -> Label -> r vis -> r (IG.Attachment r) ->
    MSMthdType r -> [MS (r par)] -> MSBody r -> SMethod r
  -- | True for main function, name, public/private, classLevel/instanceLevel,
  --   return type, parameters, body
  intFunc       :: Bool -> Label -> r vis -> r (IG.Attachment r)
    -> MSMthdType r -> [MS (r par)] -> MSBody r -> SMethod r

  destructor :: [IG.CSStateVar r] -> SMethod r

class StateVarElim r where
  stateVar :: r (IG.StateVar r) -> Doc

type ParentSpec = Doc

class (BlockCommentSym r) => RenderClass r vis | r -> vis where
  -- class name, visibility, parent, state variables, constructor(s), methods
  intClass :: Label -> r vis -> r ParentSpec -> [IG.CSStateVar r]
    -> [SMethod r] -> [SMethod r] -> IG.SClass r

  inherit :: Maybe Label -> r ParentSpec
  implements :: [Label] -> r ParentSpec

  commentedClass :: CS (r Doc) -> IG.SClass r -> IG.SClass r

class ClassElim r where
  class' :: r (IG.Class r) -> Doc

class RenderMod r where
  modFromData :: String -> FS Doc -> IG.FSModule r
  updateModuleDoc :: (Doc -> Doc) -> r (IG.Module r) -> r (IG.Module r)

class ModuleElim r where
  module' :: r (IG.Module r) -> Doc
