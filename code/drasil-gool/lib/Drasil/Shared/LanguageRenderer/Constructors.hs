-- | Generic constructors and smart constructors to be used in renderers
module Drasil.Shared.LanguageRenderer.Constructors (
  mkStmt, mkStmtNoEnd, mkStateVal, mkVal, mkStateVar, mkVar, mkStaticVar,
  mkOp, unOpPrec, compEqualPrec, compPrec, addPrec, multPrec, powerPrec,
  andPrec, orPrec, inPrec, unExpr, unExpr', unExprNumDbl, typeUnExpr, binExpr,
  binExpr', binExprNumDbl', typeBinExpr
) where

import Drasil.Shared.InterfaceCommon (TypeSym(..), TypeElim(..), ValueSym(..),
  StatementSym(..), VariableSym(..))
import Drasil.Shared.RendererClassesCommon (CommonRenderSym,
  UnaryOpSym(..), BinaryOpSym(..), OpElim(uOpPrec, bOpPrec), RenderVariable(..),
  RenderValue(..), ValueElim(valuePrec), RenderStatement(..))
import qualified Drasil.Shared.RendererClassesCommon as RC (uOp, bOp, value)
import Drasil.Shared.LanguageRenderer (unOpDocD, unOpDocD', binOpDocD, binOpDocD')
import Drasil.Shared.AST (Terminator(..), Binding(..), OpData, od)
import Drasil.Shared.CodeType (CodeType(..))

import Text.PrettyPrint.HughesPJ (Doc, parens, text)

-- Statements

-- | Constructs a statement terminated by a semi-colon
mkStmt :: (CommonRenderSym r) => Doc -> Statement r
mkStmt = flip stmtFromData Semi

-- | Constructs a statement without a termination character
mkStmtNoEnd :: (CommonRenderSym r) => Doc -> Statement r
mkStmtNoEnd = flip stmtFromData Empty

-- Values --

-- | Constructs a value in a stateful context
mkStateVal :: (CommonRenderSym r) => Type r -> Doc -> Value r
mkStateVal = valFromData Nothing Nothing

-- | Constructs a value in a non-stateful context
mkVal :: (CommonRenderSym r) => Type r -> Doc -> Value r
mkVal = valFromData Nothing Nothing

-- Variables --

-- | Constructs a dynamic variable in a stateful context
mkStateVar :: (CommonRenderSym r) => String -> Type r -> Doc -> Variable r
mkStateVar = varFromData Dynamic

-- | Constructs a dynamic variable in a non-stateful context
mkVar :: (CommonRenderSym r) => String -> Type r -> Doc -> Variable r
mkVar = varFromData Dynamic

-- | Constructs a static variable in a stateful context
mkStaticVar :: (CommonRenderSym r) => String -> Type r -> Doc -> Variable r
mkStaticVar = varFromData Static

-- Operators --

-- | Construct an operator with given precedence and rendering
mkOp :: Int -> Doc -> OpData
mkOp = od

-- | Construct an operator with typical unary-operator precedence
unOpPrec :: String -> OpData
unOpPrec = mkOp 9 . text

-- | Construct an operator with equality-comparison-level precedence
compEqualPrec :: String -> OpData
compEqualPrec = mkOp 4 . text

-- | Construct an operator with comparison-level precedence
compPrec :: String -> OpData
compPrec = mkOp 5 . text

-- | Construct an operator with addition-level precedence
addPrec :: String -> OpData
addPrec = mkOp 6 . text

-- | Construct an operator with multiplication-level precedence
multPrec :: String -> OpData
multPrec = mkOp 7 . text

-- | Construct an operator with exponentiation-level precedence
powerPrec :: String -> OpData
powerPrec = mkOp 8 . text

-- | Construct an operator with conjunction-level precedence
andPrec :: String -> OpData
andPrec = mkOp 3 . text

-- | Construct an operator with disjunction-level precedence
orPrec :: String -> OpData
orPrec = mkOp 2 . text

inPrec :: String -> OpData
inPrec = mkOp 2 . text

-- Expressions --

-- | Constructs a unary expression like ln(v), for some operator ln and value v
unExpr :: (CommonRenderSym r) => UnaryOp r -> Value r -> Value r
unExpr = mkUnExpr unOpDocD

-- | Constructs a unary expression like -v, for some operator - and value v
unExpr' :: (CommonRenderSym r) => UnaryOp r -> Value r -> Value r
unExpr' u v = mkUnExpr (if maybe False (< uOpPrec u) (valuePrec v) then unOpDocD else unOpDocD') u v

mkUnExpr :: (CommonRenderSym r) => (Doc -> Doc -> Doc) -> UnaryOp r ->
  Value r -> Value r
mkUnExpr d u v = mkExpr (uOpPrec u) (valueType v) (d (RC.uOp u) (RC.value v))

-- | To be used in languages where the unary operator returns a double. If the 
-- value passed to the operator is a float, this function preserves that type 
-- by casting the result to a float.
unExprNumDbl :: (CommonRenderSym r) => UnaryOp r -> Value r -> Value r
unExprNumDbl u v =
  let w = mkUnExpr unOpDocD u v
  in unExprCastFloat (valueType v) w

-- Only used by unExprNumDbl
unExprCastFloat :: (CommonRenderSym r) => Type r -> Value r -> Value r
unExprCastFloat t = castType (getType t)
  where castType Float = cast float
        castType _ = id

-- | To be used when the type of the value is different from the type of the
-- resulting expression. The type of the result is passed as a parameter.
typeUnExpr :: (CommonRenderSym r) => UnaryOp r -> Type r -> Value r -> Value r
typeUnExpr u t s = mkExpr (uOpPrec u) t (unOpDocD (RC.uOp u) (RC.value s))

-- | Constructs binary expressions like v + w, for some operator + and values v 
-- and w, parenthesizing v and w if needed.
binExpr :: (CommonRenderSym r) => BinaryOp r -> Value r -> Value r -> Value r
binExpr b v1 v2=
  let
    exprType = numType v1 v2
    exprRender = exprRender' binExprRender b v1 v2
  in mkExpr (bOpPrec b) exprType exprRender

-- | Constructs binary expressions like pow(v,w), for some operator pow and
-- values v and w
binExpr' :: (CommonRenderSym r) => BinaryOp r -> Value r -> Value r -> Value r
binExpr' b v1 v2 =
  let 
    exprType = numType v1 v2
    exprRender = exprRender' binOpDocDRend b v1 v2
  in mkExpr 9 exprType exprRender

-- | To be used in languages where the binary operator returns a double. If 
-- either value passed to the operator is a float, this function preserves that 
-- type by casting the result to a float.
binExprNumDbl' :: (CommonRenderSym r) => BinaryOp r -> Value r -> Value r -> Value r
binExprNumDbl' b v1 v2 =
  let t1 = valueType v1
      t2 = valueType v2
      e = binExpr' b v1 v2
  in binExprCastFloat t1 t2 e

-- Only used by binExprNumDbl'
binExprCastFloat :: (CommonRenderSym r) => Type r -> Type r -> Value r ->
  Value r
binExprCastFloat t1 t2 = castType (getType t1) (getType t2)
  where castType Float _ = cast float
        castType _ Float = cast float
        castType _ _ = id

-- | To be used when the types of the values are different from the type of the
-- resulting expression. The type of the result is passed as a parameter.
typeBinExpr :: (CommonRenderSym r) => BinaryOp r -> Type r -> Value r -> Value r
  -> Value r
typeBinExpr b t v1 v2 =
  let bnexr = exprRender' binExprRender b v1 v2
  in mkExpr (bOpPrec b) t bnexr

-- For numeric binary expressions, checks that both types are numeric and 
-- returns result type. Selects the type with lowest precision.
numType :: (CommonRenderSym r) => Value r-> Value r -> Type r
numType v1 v2 =
  let t1 = valueType v1
      t2 = valueType v2
      numericType Integer Integer = t1
      numericType Float _ = t1
      numericType _ Float = t2
      numericType Double _ = t1
      numericType _ Double = t2
      numericType _ _ = error "Numeric types required for numeric expression"
  in numericType (getType t1) (getType t2)

exprRender' :: (BinaryOp r -> Value r -> Value r -> Doc) ->
  BinaryOp r -> Value r -> Value r -> Doc
exprRender' f = f

mkExpr :: (CommonRenderSym r) => Int -> Type r -> Doc -> Value r
mkExpr p = valFromData (Just p) Nothing

binOpDocDRend :: (CommonRenderSym r) => BinaryOp r -> Value r ->
  Value r -> Doc
binOpDocDRend b v1 v2 = binOpDocD' (RC.bOp b) (RC.value v1) (RC.value v2)

-- Adds parentheses around an expression passed as the left argument to a 
-- left-associative binary operator if the precedence of the expression is less 
-- than the precedence of the operator
exprParensL :: (CommonRenderSym r) => BinaryOp r -> Value r -> Doc
exprParensL o v = (if maybe False (< bOpPrec o) (valuePrec v) then parens else
  id) $ RC.value v

-- Adds parentheses around an expression passed as the right argument to a 
-- left-associative binary operator if the precedence of the expression is less 
-- than or equal to the precedence of the operator
exprParensR :: (CommonRenderSym r) => BinaryOp r -> Value r -> Doc
exprParensR o v = (if maybe False (<= bOpPrec o) (valuePrec v) then parens else
  id) $ RC.value v

-- Renders binary expression, adding parentheses if needed
binExprRender :: (CommonRenderSym r) =>  BinaryOp r -> Value r -> Value r
  -> Doc
binExprRender b v1 v2 =
  let leftExpr = exprParensL b v1
      rightExpr = exprParensR b v2
  in binOpDocD (RC.bOp b) leftExpr rightExpr