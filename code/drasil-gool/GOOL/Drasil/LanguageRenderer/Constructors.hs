-- | Generic constructors and smart constructors to be used in renderers
module GOOL.Drasil.LanguageRenderer.Constructors (
  mkStmt, mkStmtNoEnd, mkStateVal, mkVal, mkStateVar, mkVar, mkStaticVar, 
  unOpPrec, compEqualPrec, compPrec, addPrec, multPrec, powerPrec, andPrec, 
  orPrec, unExpr, unExpr', unExprNumDbl, typeUnExpr, binExpr, binExpr', 
  binExprNumDbl', typeBinExpr
) where

import GOOL.Drasil.ClassInterface (VSType, SVariable, SValue, TypeSym(..), 
  TypeElim(..), VariableSym(..), ValueSym(..), StatementSym(..))
import GOOL.Drasil.RendererClasses (RenderSym, VSUnOp, VSBinOp, UnaryOpSym(..),
  BinaryOpSym(..), RenderOp(..), OpElim(uOpPrec, bOpPrec), RenderVariable(..), RenderValue(..), ValueElim(valuePrec), RenderStatement(..))
import qualified GOOL.Drasil.RendererClasses as RC (uOp, bOp, value)
import GOOL.Drasil.LanguageRenderer (unOpDocD, unOpDocD', binOpDocD, binOpDocD')
import GOOL.Drasil.AST (Terminator(..), Binding(..))
import GOOL.Drasil.CodeType (CodeType(..))
import GOOL.Drasil.Helpers (onStateValue, on2StateValues, on3StateValues)

import Text.PrettyPrint.HughesPJ (Doc, parens, text)

-- Statements

mkStmt :: (RenderSym r) => Doc -> r (Statement r)
mkStmt = flip stmtFromData Semi

mkStmtNoEnd :: (RenderSym r) => Doc -> r (Statement r)
mkStmtNoEnd = flip stmtFromData Empty

-- Values --

mkStateVal :: (RenderSym r) => VSType r -> Doc -> SValue r
mkStateVal t d = onStateValue (\tp -> valFromData Nothing tp d) t

mkVal :: (RenderSym r) => r (Type r) -> Doc -> r (Value r)
mkVal = valFromData Nothing

-- Variables --

mkStateVar :: (RenderSym r) => String -> VSType r -> Doc -> SVariable r
mkStateVar n t d = onStateValue (\tp -> varFromData Dynamic n tp d) t

mkVar :: (RenderSym r) => String -> r (Type r) -> Doc -> r (Variable r)
mkVar = varFromData Dynamic

mkStaticVar :: (RenderSym r) => String -> VSType r -> Doc -> SVariable r
mkStaticVar n t d = onStateValue (\tp -> varFromData Static n tp d) t

-- Operators --

unOpPrec :: (RenderSym r) => String -> VSUnOp r
unOpPrec = uOpFromData 9 . text

compEqualPrec :: (RenderSym r) => String -> VSBinOp r
compEqualPrec = bOpFromData 4 . text

compPrec :: (RenderSym r) => String -> VSBinOp r
compPrec = bOpFromData 5 . text

addPrec :: (RenderSym r) => String -> VSBinOp r
addPrec = bOpFromData 6 . text

multPrec :: (RenderSym r) => String -> VSBinOp r
multPrec = bOpFromData 7 . text

powerPrec :: (RenderSym r) => String -> VSBinOp r
powerPrec = bOpFromData 8 . text

andPrec :: (RenderSym r) => String -> VSBinOp r 
andPrec = bOpFromData 3 . text

orPrec :: (RenderSym r) => String -> VSBinOp r
orPrec = bOpFromData 2 . text

-- Expressions --

unExpr :: (RenderSym r) => VSUnOp r -> SValue r -> SValue r
unExpr = on2StateValues (mkUnExpr unOpDocD)

unExpr' :: (RenderSym r) => VSUnOp r -> SValue r -> SValue r
unExpr' = on2StateValues (mkUnExpr unOpDocD')

mkUnExpr :: (RenderSym r) => (Doc -> Doc -> Doc) -> r (UnaryOp r) -> 
  r (Value r) -> r (Value r)
mkUnExpr d u v = mkExpr (uOpPrec u) (valueType v) (d (RC.uOp u) (RC.value v))

unExprNumDbl :: (RenderSym r) => VSUnOp r -> SValue r -> SValue r
unExprNumDbl u' v' = u' >>= (\u -> v' >>= (\v -> 
  unExprCastFloat (valueType v) $ return $ mkUnExpr unOpDocD u v))

unExprCastFloat :: (RenderSym r) => r (Type r) -> (SValue r -> SValue r)
unExprCastFloat t = castType $ getType t
  where castType Float = cast float
        castType _ = id
  
typeUnExpr :: (RenderSym r) => VSUnOp r -> VSType r -> SValue r -> SValue r
typeUnExpr = on3StateValues (\u t -> mkExpr (uOpPrec u) t . unOpDocD (RC.uOp u) 
  . RC.value)

binExpr :: (RenderSym r) => VSBinOp r -> SValue r -> SValue r -> SValue r
binExpr = on3StateValues (\b v1 v2 -> mkExpr (bOpPrec b) (numType (valueType v1)
  (valueType v2)) (binOpDocD (RC.bOp b) (exprParensL b v1 $ RC.value v1) 
  (exprParensR b v2 $ RC.value v2)))

binExpr' :: (RenderSym r) => VSBinOp r -> SValue r -> SValue r -> SValue r
binExpr' = on3StateValues (\b v1 v2 -> mkExpr 9 (numType (valueType v1) 
  (valueType v2)) (binOpDocD' (RC.bOp b) (RC.value v1) (RC.value v2)))

binExprNumDbl' :: (RenderSym r) => VSBinOp r -> SValue r -> SValue r -> SValue r
binExprNumDbl' b' v1' v2' = b' >>= (\b -> v1' >>= (\v1 -> v2' >>= (\v2 -> 
  let t1 = valueType v1
      t2 = valueType v2
  in binExprCastFloat t1 t2 $ return $ mkExpr 9 (numType t1 t2) 
  (binOpDocD' (RC.bOp b) (RC.value v1) (RC.value v2)))))

binExprCastFloat :: (RenderSym r) => r (Type r) -> r (Type r) ->
  (SValue r -> SValue r)
binExprCastFloat t1 t2 = castType (getType t1) (getType t2)
  where castType Float _ = cast float
        castType _ Float = cast float
        castType _ _ = id

typeBinExpr :: (RenderSym r) => VSBinOp r -> VSType r -> SValue r -> SValue r 
  -> SValue r
typeBinExpr bod tp vl1 vl2 = (\b t v1 v2 -> mkExpr (bOpPrec b) t (binOpDocD 
  (RC.bOp b) (exprParensL b v1 $ RC.value v1) (exprParensR b v2 $ RC.value v2)))
  <$> bod <*> tp <*> vl1 <*> vl2 

numType :: (RenderSym r) => r (Type r) -> r (Type r) -> r (Type r)
numType t1 t2 = numericType (getType t1) (getType t2)
  where numericType Integer Integer = t1
        numericType Float _ = t1
        numericType _ Float = t2
        numericType Double _ = t1
        numericType _ Double = t2
        numericType _ _ = error "Numeric types required for numeric expression"

mkExpr :: (RenderSym r) => Int -> r (Type r) -> Doc -> r (Value r)
mkExpr p = valFromData (Just p)

exprParensL :: (RenderSym r) => r (BinaryOp r) -> r (Value r) -> (Doc -> Doc)
exprParensL o v = if maybe False (< bOpPrec o) (valuePrec v) then parens else id

exprParensR :: (RenderSym r) => r (BinaryOp r) -> r (Value r) -> (Doc -> Doc)
exprParensR o v = if maybe False (<= bOpPrec o) (valuePrec v) then parens else 
  id