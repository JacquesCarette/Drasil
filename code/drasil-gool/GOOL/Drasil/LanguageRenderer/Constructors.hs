-- | Generic constructors and smart constructors to be used in renderers
module GOOL.Drasil.LanguageRenderer.Constructors (
  mkStmt, mkStmtNoEnd, mkStateVal, mkVal, mkStateVar, mkVar, mkStaticVar, 
  VSOp, mkOp, unOpPrec, compEqualPrec, compPrec, addPrec, multPrec, powerPrec, 
  andPrec, orPrec, unExpr, unExpr', unExprNumDbl, typeUnExpr, binExpr, 
  binExpr', binExprNumDbl', typeBinExpr
) where

import GOOL.Drasil.ClassInterface (VSType, SVariable, SValue, TypeSym(..), 
  TypeElim(..), VariableSym(..), ValueSym(..), StatementSym(..))
import GOOL.Drasil.RendererClasses (RenderSym, VSUnOp, VSBinOp, UnaryOpSym(..),
  BinaryOpSym(..), OpElim(uOpPrec, bOpPrec), RenderVariable(..), 
  RenderValue(..), ValueElim(valuePrec), RenderStatement(..))
import qualified GOOL.Drasil.RendererClasses as RC (uOp, bOp, value)
import GOOL.Drasil.LanguageRenderer (unOpDocD, unOpDocD', binOpDocD, binOpDocD')
import GOOL.Drasil.AST (Terminator(..), Binding(..), OpData, od)
import GOOL.Drasil.CodeType (CodeType(..))
import GOOL.Drasil.Helpers (toCode, toState, onStateValue, on2StateValues, 
  on3StateValues)
import GOOL.Drasil.State (VS)

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

type VSOp r = VS (r OpData)

mkOp :: (Monad r) => Int -> Doc -> VSOp r
mkOp p d = toState $ toCode $ od p d

unOpPrec :: (Monad r) => String -> VSOp r
unOpPrec = mkOp 9 . text

compEqualPrec :: (Monad r) => String -> VSOp r
compEqualPrec = mkOp 4 . text

compPrec :: (Monad r) => String -> VSOp r
compPrec = mkOp 5 . text

addPrec :: (Monad r) => String -> VSOp r
addPrec = mkOp 6 . text

multPrec :: (Monad r) => String -> VSOp r
multPrec = mkOp 7 . text

powerPrec :: (Monad r) => String -> VSOp r
powerPrec = mkOp 8 . text

andPrec :: (Monad r) => String -> VSOp r 
andPrec = mkOp 3 . text

orPrec :: (Monad r) => String -> VSOp r
orPrec = mkOp 2 . text

-- Expressions --

-- | Constructs a unary expression like ln(v), for some operator ln and value v
unExpr :: (RenderSym r) => VSUnOp r -> SValue r -> SValue r
unExpr = on2StateValues (mkUnExpr unOpDocD)

-- | Constructs a unary expression like -v, for some operator - and value v
unExpr' :: (RenderSym r) => VSUnOp r -> SValue r -> SValue r
unExpr' = on2StateValues (mkUnExpr unOpDocD')

mkUnExpr :: (RenderSym r) => (Doc -> Doc -> Doc) -> r (UnaryOp r) -> 
  r (Value r) -> r (Value r)
mkUnExpr d u v = mkExpr (uOpPrec u) (valueType v) (d (RC.uOp u) (RC.value v))

-- | To be used in languages where the unary operator returns a double. If the 
-- value passed to the operator is a float, this function preserves that type 
-- by casting the result to a float.
unExprNumDbl :: (RenderSym r) => VSUnOp r -> SValue r -> SValue r
unExprNumDbl u' v' = do
  u <- u'
  v <- v'
  unExprCastFloat (valueType v) $ mkUnExpr unOpDocD u v

-- Only used by unExprNumDbl
unExprCastFloat :: (RenderSym r) => r (Type r) -> r (Value r) -> SValue r
unExprCastFloat t = castType (getType t) . toState
  where castType Float = cast float
        castType _ = id
  
-- | To be used when the type of the value is different from the type of the
-- resulting expression. The type of the result is passed as a parameter.
typeUnExpr :: (RenderSym r) => VSUnOp r -> VSType r -> SValue r -> SValue r
typeUnExpr = on3StateValues (\u t -> mkExpr (uOpPrec u) t . unOpDocD (RC.uOp u) 
  . RC.value)

-- | Constructs binary expressions like v + w, for some operator + and values v 
-- and w, parenthesizing v and w if needed.
binExpr :: (RenderSym r) => VSBinOp r -> SValue r -> SValue r -> SValue r
binExpr = on3StateValues (\b v1 v2 -> 
  let exprType = numType (valueType v1) (valueType v2)
      exprRender = binExprRender b v1 v2
  in mkExpr (bOpPrec b) exprType exprRender)

-- | Constructs binary expressions like pow(v,w), for some operator pow and
-- values v and w
binExpr' :: (RenderSym r) => VSBinOp r -> SValue r -> SValue r -> SValue r
binExpr' = on3StateValues (\b v1 v2 -> 
  let exprType = numType (valueType v1) (valueType v2)
      exprRender = binOpDocD' (RC.bOp b) (RC.value v1) (RC.value v2)
  in mkExpr 9 exprType exprRender)

-- | To be used in languages where the binary operator returns a double. If 
-- either value passed to the operator is a float, this function preserves that 
-- type by casting the result to a float.
binExprNumDbl' :: (RenderSym r) => VSBinOp r -> SValue r -> SValue r -> SValue r
binExprNumDbl' b' v1' v2' = do
  b <- b'
  v1 <- v1'
  v2 <- v2'
  let t1 = valueType v1
      t2 = valueType v2
      exprType = numType t1 t2
      exprRender = binOpDocD' (RC.bOp b) (RC.value v1) (RC.value v2)
  binExprCastFloat t1 t2 $ mkExpr 9 exprType exprRender

-- Only used by binExprCastFloat
binExprCastFloat :: (RenderSym r) => r (Type r) -> r (Type r) -> r (Value r) -> 
  SValue r
binExprCastFloat t1 t2 = castType (getType t1) (getType t2) . toState
  where castType Float _ = cast float
        castType _ Float = cast float
        castType _ _ = id

-- | To be used when the types of the values are different from the type of the
-- resulting expression. The type of the result is passed as a parameter.
typeBinExpr :: (RenderSym r) => VSBinOp r -> VSType r -> SValue r -> SValue r 
  -> SValue r
typeBinExpr b' t' v1' v2' = do
  b <- b'
  t <- t'
  v1 <- v1'
  v2 <- v2'
  toState $ mkExpr (bOpPrec b) t (binExprRender b v1 v2)

-- For numeric binary expressions, checks that both types are numeric and 
-- returns result type. Selects the type with lowest precision.
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

-- Adds parentheses around an expression passed as the left argument to a 
-- left-associative binary operator if the precedence of the expression is less 
-- than the precedence of the operator
exprParensL :: (RenderSym r) => r (BinaryOp r) -> r (Value r) -> Doc
exprParensL o v = (if maybe False (< bOpPrec o) (valuePrec v) then parens else 
  id) $ RC.value v

-- Adds parentheses around an expression passed as the right argument to a 
-- left-associative binary operator if the precedence of the expression is less 
-- than or equal to the precedence of the operator
exprParensR :: (RenderSym r) => r (BinaryOp r) -> r (Value r) -> Doc
exprParensR o v = (if maybe False (<= bOpPrec o) (valuePrec v) then parens else 
  id) $ RC.value v

-- Renders binary expression, adding parentheses if needed
binExprRender :: (RenderSym r) => r (BinaryOp r) -> r (Value r) -> r (Value r) 
  -> Doc
binExprRender b v1 v2 = 
  let leftExpr = exprParensL b v1
      rightExpr = exprParensR b v2
  in binOpDocD (RC.bOp b) leftExpr rightExpr