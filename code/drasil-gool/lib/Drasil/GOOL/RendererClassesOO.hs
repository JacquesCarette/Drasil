{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Drasil.GOOL.RendererClassesOO (
  OORenderSym, RenderFile(..), PermElim(..), InternalGetSet(..),
  StateVarElim(..), ParentSpec, RenderClass(..), ClassElim(..), RenderMod(..),
  ModuleElim(..), OORenderMethod(..), OOMethodTypeSym(..)
) where

import Drasil.Shared.InterfaceCommon (Label, Block, MSBody, SVariable, SValue)
import qualified Drasil.GOOL.InterfaceGOOL as IG (SFile, Module, FSModule,
  Class, SClass, CSStateVar, OOVariableValue, OOValueExpression(..),
  InternalValueExp(..), FileSym(..), AttachmentSym(..), GetSet(..),
  StateVarSym(..), ObserverPattern(..), StrategyPattern(..))
import Drasil.Shared.AST (AttachmentTag, TypeData, ParamData, FuncData)
import Drasil.Shared.State (FS, CS, VS, MS)

import Text.PrettyPrint.HughesPJ (Doc)

import Drasil.Shared.RendererClassesCommon (MSMthdType, CommonRenderSym,
  BlockCommentSym(..), MethodTypeSym(..), RenderMethod(..))

class (CommonRenderSym r vis smt md, IG.FileSym r vis smt md,
  IG.InternalValueExp r, IG.GetSet r, IG.ObserverPattern r smt,
  IG.StrategyPattern r smt, IG.OOVariableValue r,
  IG.OOValueExpression r, RenderClass r vis md, ClassElim r, RenderFile r,
  InternalGetSet r, OORenderMethod r vis md, RenderMod r, ModuleElim r,
  StateVarElim r, PermElim r
  ) => OORenderSym r vis smt md

-- OO-Only Typeclasses --

class (BlockCommentSym r) => RenderFile r where
  -- top and bottom are only used for pre-processor guards for C++ header
  -- files. FIXME: Remove them (generation of pre-processor guards can be
  -- handled by fileDoc instead)
  top :: r IG.Module -> r Block
  bottom :: r Block

  commentedMod :: IG.SFile r -> FS (r Doc) -> IG.SFile r

  fileFromData :: FilePath -> IG.FSModule r -> IG.SFile r

class PermElim r where
  perm :: r (IG.Attachment r) -> Doc
  binding :: r (IG.Attachment r) -> AttachmentTag

class InternalGetSet r where
  getFunc :: SVariable r -> VS (r FuncData)
  setFunc :: VS (r TypeData) -> SVariable r -> SValue r -> VS (r FuncData)

class (MethodTypeSym r) => OOMethodTypeSym r where
  construct :: Label -> MSMthdType r

class (RenderMethod r md, OOMethodTypeSym r) => OORenderMethod r vis md | r -> vis where
  -- | Main method?, name, public/private, classLevel/instanceLevel,
  --   return type, parameters, body
  intMethod     :: Bool -> Label -> r vis -> r (IG.Attachment r) ->
    MSMthdType r -> [MS (r ParamData)] -> MSBody r -> MS (r md)
  -- | True for main function, name, public/private, classLevel/instanceLevel,
  --   return type, parameters, body
  intFunc       :: Bool -> Label -> r vis -> r (IG.Attachment r)
    -> MSMthdType r -> [MS (r ParamData)] -> MSBody r -> MS (r md)

  destructor :: [IG.CSStateVar r] -> MS (r md)

class StateVarElim r where
  stateVar :: r (IG.StateVar r) -> Doc

type ParentSpec = Doc

class (BlockCommentSym r) => RenderClass r vis md | r -> vis md where
  -- class name, visibility, parent, state variables, constructor(s), methods
  intClass :: Label -> r vis -> r ParentSpec -> [IG.CSStateVar r]
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
