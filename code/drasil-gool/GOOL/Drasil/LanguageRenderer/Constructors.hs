-- | Generic constructors and smart constructors to be used in renderers
module GOOL.Drasil.LanguageRenderer.Constructors (
  mkStmt, mkStmtNoEnd, mkStateVal, mkVal, mkStateVar, mkVar, mkStaticVar
) where

import GOOL.Drasil.ClassInterface (VSType, SVariable, SValue, TypeSym(..), 
  VariableSym(..), ValueSym(..), StatementSym(..))
import GOOL.Drasil.RendererClasses (RenderSym, RenderVariable(..), 
  RenderValue(..), RenderStatement(..))
import GOOL.Drasil.AST (Terminator(..), Binding(..))
import GOOL.Drasil.Helpers (onStateValue)

import Text.PrettyPrint.HughesPJ (Doc)

mkStmt :: (RenderSym r) => Doc -> r (Statement r)
mkStmt = flip stmtFromData Semi

mkStmtNoEnd :: (RenderSym r) => Doc -> r (Statement r)
mkStmtNoEnd = flip stmtFromData Empty

mkStateVal :: (RenderSym r) => VSType r -> Doc -> SValue r
mkStateVal t d = onStateValue (\tp -> valFromData Nothing tp d) t

mkVal :: (RenderSym r) => r (Type r) -> Doc -> r (Value r)
mkVal = valFromData Nothing

mkStateVar :: (RenderSym r) => String -> VSType r -> Doc -> SVariable r
mkStateVar n t d = onStateValue (\tp -> varFromData Dynamic n tp d) t

mkVar :: (RenderSym r) => String -> r (Type r) -> Doc -> r (Variable r)
mkVar = varFromData Dynamic

mkStaticVar :: (RenderSym r) => String -> VSType r -> Doc -> SVariable r
mkStaticVar n t d = onStateValue (\tp -> varFromData Static n tp d) t