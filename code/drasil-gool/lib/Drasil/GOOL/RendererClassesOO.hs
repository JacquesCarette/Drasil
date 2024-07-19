{-# LANGUAGE TypeFamilies #-}

module Drasil.GOOL.RendererClassesOO (
  OORenderSym, RenderFile(..), PermElim(..), InternalGetSet(..),
  StateVarElim(..), ParentSpec, RenderClass(..), ClassElim(..), RenderMod(..),
  ModuleElim(..), OORenderMethod(..), OOMethodTypeSym(..)
) where

import Drasil.GOOL.InterfaceCommon (Label, MSBody, VSFunction, VSType,
  SVariable, SValue, MSParameter, SMethod, BlockSym(..), ScopeSym(..))
import qualified Drasil.GOOL.InterfaceGOOL as IG (SFile, FSModule, SClass,
  CSStateVar, OOVariableValue, OOValueExpression(..), InternalValueExp(..),
  FileSym(..), ModuleSym(..), ClassSym(..), PermanenceSym(..), GetSet(..),
  StateVarSym(..), ObserverPattern(..), StrategyPattern(..))
import Drasil.GOOL.AST (Binding)
import Drasil.GOOL.State (FS, CS)

import Text.PrettyPrint.HughesPJ (Doc)

import Drasil.GOOL.RendererClassesCommon (MSMthdType, CommonRenderSym,
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
  top :: r (IG.Module r) -> r (Block r) 
  bottom :: r (Block r)

  commentedMod :: IG.SFile r -> FS (r (BlockComment r)) -> IG.SFile r

  fileFromData :: FilePath -> IG.FSModule r -> IG.SFile r

class PermElim r where
  perm :: r (IG.Permanence r) -> Doc
  binding :: r (IG.Permanence r) -> Binding

class InternalGetSet r where
  getFunc :: SVariable r -> VSFunction r
  setFunc :: VSType r -> SVariable r -> SValue r -> VSFunction r

class (MethodTypeSym r) => OOMethodTypeSym r where
  construct :: Label -> MSMthdType r

class (RenderMethod r, OOMethodTypeSym r) => OORenderMethod r where
  -- | Main method?, name, public/private, static/dynamic, 
  --   return type, parameters, body
  intMethod     :: Bool -> Label -> r (Scope r) -> r (IG.Permanence r) -> 
    MSMthdType r -> [MSParameter r] -> MSBody r -> SMethod r
  -- | True for main function, name, public/private, static/dynamic, 
  --   return type, parameters, body
  intFunc       :: Bool -> Label -> r (Scope r) -> r (IG.Permanence r) 
    -> MSMthdType r -> [MSParameter r] -> MSBody r -> SMethod r
    
  destructor :: [IG.CSStateVar r] -> SMethod r

class StateVarElim r where  
  stateVar :: r (IG.StateVar r) -> Doc

type ParentSpec = Doc

class (BlockCommentSym r) => RenderClass r where
  intClass :: Label -> r (Scope r) -> r ParentSpec -> [IG.CSStateVar r] 
    -> [SMethod r] -> [SMethod r] -> IG.SClass r
    
  inherit :: Maybe Label -> r ParentSpec
  implements :: [Label] -> r ParentSpec

  commentedClass :: CS (r (BlockComment r)) -> IG.SClass r -> IG.SClass r
  
class ClassElim r where
  class' :: r (IG.Class r) -> Doc

class RenderMod r where
  modFromData :: String -> FS Doc -> IG.FSModule r
  updateModuleDoc :: (Doc -> Doc) -> r (IG.Module r) -> r (IG.Module r)
  
class ModuleElim r where
  module' :: r (IG.Module r) -> Doc
