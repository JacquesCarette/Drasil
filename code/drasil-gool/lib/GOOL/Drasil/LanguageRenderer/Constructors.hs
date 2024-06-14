-- | Generic constructors and smart constructors to be used in renderers
module GOOL.Drasil.LanguageRenderer.Constructors (
  mkStmt, mkStmtNoEnd, mkStateVal, mkVal, mkStateVar, mkVar, mkStaticVar, 
  VSOp, mkOp, unOpPrec, compEqualPrec, compPrec, addPrec, multPrec, powerPrec, 
  andPrec, orPrec, unExpr, unExpr', unExprNumDbl, typeUnExpr, binExpr, 
  binExpr', binExprNumDbl', typeBinExpr
) where

import GOOL.Drasil.ClassInterface (VSType, MSStatement, SVariable, SValue, TypeSym(..), 
  TypeElim(..), ValueSym(..))
import GOOL.Drasil.RendererClasses (RenderSym, VSUnOp, VSBinOp, UnaryOpSym(..),
  BinaryOpSym(..), OpElim(uOpPrec, bOpPrec), RenderVariable(..), 
  RenderValue(..), ValueElim(valuePrec), RenderStatement(..))
import qualified GOOL.Drasil.RendererClasses as RC (uOp, bOp, value)
import GOOL.Drasil.LanguageRenderer (unOpDocD, unOpDocD', binOpDocD, binOpDocD')
import GOOL.Drasil.AST (Terminator(..), Binding(..), OpData, od)
import GOOL.Drasil.CodeType (CodeType(..))
import GOOL.Drasil.Helpers (toCode, toState, on2StateValues)
import GOOL.Drasil.State (VS)

import Text.PrettyPrint.HughesPJ (Doc, parens, text)
import Data.Composition ((.:))
import Control.Monad (join)

-- Statements

-- | Constructs a statement terminated by a semi-colon
mkStmt :: (RenderSym r) => Doc -> MSStatement r
mkStmt = flip stmtFromData Semi

-- | Constructs a statement without a termination character
mkStmtNoEnd :: (RenderSym r) => Doc -> MSStatement r
mkStmtNoEnd = flip stmtFromData Empty

-- Values --

-- | Constructs a value in a stateful context
mkStateVal :: (RenderSym r) => VSType r -> Doc -> SValue r
mkStateVal = valFromData Nothing

-- | Constructs a value in a non-stateful context
mkVal :: (RenderSym r) => r (Type r) -> Doc -> SValue r
mkVal t = valFromData Nothing (toState t)

-- Variables --

-- | Constructs a dynamic variable in a stateful context
mkStateVar :: (RenderSym r) => String -> VSType r -> Doc -> SVariable r
mkStateVar = varFromData Dynamic

-- | Constructs a dynamic variable in a non-stateful context
mkVar :: (RenderSym r) => String -> r (Type r) -> Doc -> SVariable r
mkVar n t = varFromData Dynamic n (toState t)

-- | Constructs a static variable in a stateful context
mkStaticVar :: (RenderSym r) => String -> VSType r -> Doc -> SVariable r
mkStaticVar = varFromData Static

-- Operators --

type VSOp r = VS (r OpData)

-- | Construct an operator with given precedence and rendering
mkOp :: (Monad r) => Int -> Doc -> VSOp r
mkOp p d = toState $ toCode $ od p d

-- | Construct an operator with typical unary-operator precedence
unOpPrec :: (Monad r) => String -> VSOp r
unOpPrec = mkOp 9 . text

-- | Construct an operator with equality-comparison-level precedence
compEqualPrec :: (Monad r) => String -> VSOp r
compEqualPrec = mkOp 4 . text

-- | Construct an operator with comparison-level precedence
compPrec :: (Monad r) => String -> VSOp r
compPrec = mkOp 5 . text

-- | Construct an operator with addition-level precedence
addPrec :: (Monad r) => String -> VSOp r
addPrec = mkOp 6 . text

-- | Construct an operator with multiplication-level precedence
multPrec :: (Monad r) => String -> VSOp r
multPrec = mkOp 7 . text

-- | Construct an operator with exponentiation-level precedence
powerPrec :: (Monad r) => String -> VSOp r
powerPrec = mkOp 8 . text

-- | Construct an operator with conjunction-level precedence
andPrec :: (Monad r) => String -> VSOp r 
andPrec = mkOp 3 . text

-- | Construct an operator with disjunction-level precedence
orPrec :: (Monad r) => String -> VSOp r
orPrec = mkOp 2 . text

-- Expressions --

-- | Constructs a unary expression like ln(v), for some operator ln and value v
unExpr :: (RenderSym r) => VSUnOp r -> SValue r -> SValue r
unExpr = join .: on2StateValues (mkUnExpr unOpDocD)

-- | Constructs a unary expression like -v, for some operator - and value v
unExpr' :: (RenderSym r) => VSUnOp r -> SValue r -> SValue r
unExpr' u' v'= do
  u <- u'
  v <- v'
  (join .: on2StateValues (mkUnExpr (if maybe False (< uOpPrec u) (valuePrec v) then unOpDocD else unOpDocD'))) u' v'

mkUnExpr :: (RenderSym r) => (Doc -> Doc -> Doc) -> r (UnaryOp r) -> 
  r (Value r) -> SValue r
mkUnExpr d u v = mkExpr (uOpPrec u) (valueType v) (d (RC.uOp u) (RC.value v))

-- | To be used in languages where the unary operator returns a double. If the 
-- value passed to the operator is a float, this function preserves that type 
-- by casting the result to a float.
unExprNumDbl :: (RenderSym r) => VSUnOp r -> SValue r -> SValue r
unExprNumDbl u' v' = do
  u <- u'
  v <- v'
  w <- mkUnExpr unOpDocD u v
  unExprCastFloat (valueType v) w

-- Only used by unExprNumDbl
unExprCastFloat :: (RenderSym r) => r (Type r) -> r (Value r) -> SValue r
unExprCastFloat t = castType (getType t) . toState
  where castType Float = cast float
        castType _ = id
  
-- | To be used when the type of the value is different from the type of the
-- resulting expression. The type of the result is passed as a parameter.
typeUnExpr :: (RenderSym r) => VSUnOp r -> VSType r -> SValue r -> SValue r
typeUnExpr u' t' s' = do 
  u <- u'
  t <- t'
  s <- s'
  mkExpr (uOpPrec u) t (unOpDocD (RC.uOp u) (RC.value s))

-- | Constructs binary expressions like v + w, for some operator + and values v 
-- and w, parenthesizing v and w if needed.
binExpr :: (RenderSym r) => VSBinOp r -> SValue r -> SValue r -> SValue r
binExpr b' v1' v2'= do 
  b <- b'
  exprType <- numType v1' v2'
  exprRender <- exprRender' binExprRender b' v1' v2'
  mkExpr (bOpPrec b) exprType exprRender

-- | Constructs binary expressions like pow(v,w), for some operator pow and
-- values v and w
binExpr' :: (RenderSym r) => VSBinOp r -> SValue r -> SValue r -> SValue r
binExpr' b' v1' v2' = do 
  exprType <- numType v1' v2'
  exprRender <- exprRender' binOpDocDRend b' v1' v2'
  mkExpr 9 exprType exprRender 

-- | To be used in languages where the binary operator returns a double. If 
-- either value passed to the operator is a float, this function preserves that 
-- type by casting the result to a float.
binExprNumDbl' :: (RenderSym r) => VSBinOp r -> SValue r -> SValue r -> SValue r
binExprNumDbl' b' v1' v2' = do 
  v1 <- v1'
  v2 <- v2'
  let t1 = valueType v1
      t2 = valueType v2
  e <- binExpr' b' v1' v2'
  binExprCastFloat t1 t2 e

-- Only used by binExprNumDbl'
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
  bnexr <- exprRender' binExprRender b' v1' v2'
  mkExpr (bOpPrec b) t bnexr

-- For numeric binary expressions, checks that both types are numeric and 
-- returns result type. Selects the type with lowest precision.
numType :: (RenderSym r) => SValue r-> SValue r -> VSType r
numType v1' v2' = do
  v1 <- v1'
  v2 <- v2'
  let t1 = valueType v1
      t2 = valueType v2
      numericType Integer Integer = t1
      numericType Float _ = t1
      numericType _ Float = t2
      numericType Double _ = t1
      numericType _ Double = t2
      numericType _ _ = error "Numeric types required for numeric expression"
  toState $ numericType (getType t1) (getType t2)

exprRender' :: (r (BinaryOp r) -> r (Value r) -> r (Value r) -> Doc) -> 
  VSBinOp r -> SValue r -> SValue r -> VS Doc
exprRender' f b' v1' v2' = do 
  b <- b' 
  v1 <- v1'
  v2 <- v2'
  toState $ f b v1 v2

mkExpr :: (RenderSym r) => Int -> r (Type r) -> Doc -> SValue r
mkExpr p t = valFromData (Just p) (toState t)

binOpDocDRend :: (RenderSym r) => r (BinaryOp r) -> r (Value r) -> 
  r (Value r) -> Doc
binOpDocDRend b v1 v2 = binOpDocD' (RC.bOp b) (RC.value v1) (RC.value v2)

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
binExprRender :: (RenderSym r) =>  r (BinaryOp r) -> r (Value r) -> r (Value r) 
  -> Doc
binExprRender b v1 v2 = 
  let leftExpr = exprParensL b v1
      rightExpr = exprParensR b v2
  in binOpDocD (RC.bOp b) leftExpr rightExpr