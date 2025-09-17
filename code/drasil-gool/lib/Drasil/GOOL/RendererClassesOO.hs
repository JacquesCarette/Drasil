{-# LANGUAGE TypeFamilyDependencies #-}

module Drasil.GOOL.RendererClassesOO (
  OORenderSym, RenderFile(..), PermElim(..), InternalGetSet(..),
  StateVarElim(..), RenderClass(..), ClassElim(..), RenderMod(..),
  ModuleElim(..), OORenderMethod(..), OOMethodTypeSym(..)
) where

import Drasil.Shared.InterfaceCommon (Label, Body, Function, Type,
  Variable, Value, Parameter, Method, BlockSym(..), VisibilitySym(..))
import qualified Drasil.GOOL.InterfaceGOOL as IG (File, Module, Class,
  StateVar, OOVariableValue, OOValueExpression(..), InternalValueExp(..),
  FileSym(..), ModuleSym(..), ClassSym(..), PermanenceSym(..), GetSet(..),
  StateVarSym(..), ObserverPattern(..), StrategyPattern(..))
import Drasil.Shared.AST (Binding)
import Drasil.Shared.State (FS, CS)

import Text.PrettyPrint.HughesPJ (Doc)

import Drasil.Shared.RendererClassesCommon (CommonRenderSym,
  BlockCommentSym(..), MethodTypeSym(..), RenderMethod(..))

class (CommonRenderSym r, IG.FileSym r, IG.InternalValueExp r, IG.GetSet r,
  IG.ObserverPattern r, IG.StrategyPattern r, IG.OOVariableValue r,
  IG.OOValueExpression r, RenderClass r, ClassElim r, RenderFile r,
  InternalGetSet r, OORenderMethod r, RenderMod r, ModuleElim r,
  StateVarElim r, PermElim r
  ) => OORenderSym r

-- OO-Only Typeclasses --

class (BlockCommentSym r) => RenderFile r where
  -- top and bottom are only used for pre-processor guards for C++ header 
  -- files. FIXME: Remove them (generation of pre-processor guards can be 
  -- handled by fileDoc instead)
  top :: IG.Module r -> Block r 
  bottom :: Block r

  commentedMod :: IG.File r -> FS (BlockComment r) -> IG.File r

  fileFromData :: FilePath -> IG.Module r -> IG.File r

class PermElim r where
  perm :: IG.Permanence r -> Doc
  binding :: IG.Permanence r -> Binding

class InternalGetSet r where
  getFunc :: Variable r -> Function r
  setFunc :: Type r -> Variable r -> Value r -> Function r

class (MethodTypeSym r) => OOMethodTypeSym r where
  construct :: Label -> MethodType r

class (RenderMethod r, OOMethodTypeSym r) => OORenderMethod r where
  -- | Main method?, name, public/private, static/dynamic, 
  --   return type, parameters, body
  intMethod     :: Bool -> Label -> Visibility r -> IG.Permanence r -> 
    MethodType r -> [Parameter r] -> Body r -> Method r
  -- | True for main function, name, public/private, static/dynamic, 
  --   return type, parameters, body
  intFunc       :: Bool -> Label -> Visibility r -> IG.Permanence r 
    -> MethodType r -> [Parameter r] -> Body r -> Method r
    
  destructor :: [IG.StateVar r] -> Method r

class StateVarElim r where  
  stateVar :: IG.StateVar r -> Doc

class (BlockCommentSym r) => RenderClass r where
  type ParentSpec r = t | t -> r
  -- class name, visibility, parent, state variables, constructor(s), methods
  intClass :: Label -> Visibility r -> ParentSpec r -> [IG.StateVar r] 
    -> [Method r] -> [Method r] -> IG.Class r
    
  inherit :: Maybe Label -> ParentSpec r
  implements :: [Label] -> ParentSpec r

  commentedClass :: CS (BlockComment r) -> IG.Class r -> IG.Class r
  
class ClassElim r where
  class' :: IG.Class r -> Doc

class RenderMod r where
  modFromData :: String -> FS Doc -> IG.Module r
  updateModuleDoc :: (Doc -> Doc) -> IG.Module r -> IG.Module r
  
class ModuleElim r where
  module' :: IG.Module r -> Doc
