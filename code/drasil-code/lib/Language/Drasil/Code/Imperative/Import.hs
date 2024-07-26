{-# LANGUAGE PostfixOperators, Rank2Types #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Language.Drasil.Code.Imperative.Import (codeType, spaceCodeType,
  publicFunc, publicFuncProc, privateMethod, privateFuncProc, publicInOutFunc,
  publicInOutFuncProc, privateInOutMethod, privateInOutFuncProc, genConstructor,
  mkVar, mkVarProc, mkVal, mkValProc, convExpr, convExprProc, convStmt,
  convStmtProc, genModDef, genModDefProc, genModFuncs, genModFuncsProc,
  genModClasses, readData, readDataProc, renderC
) where

import Language.Drasil (HasSymbol, HasUID(..), HasSpace(..),
  Space (Rational, Real), RealInterval(..), UID, Constraint(..), Inclusive (..))
import Database.Drasil (symbResolve)
import Language.Drasil.CodeExpr (sy, ($<), ($>), ($<=), ($>=), ($&&))
import Language.Drasil.CodeExpr.Development (CodeExpr(..), ArithBinOp(..),
  AssocArithOper(..), AssocBoolOper(..), BoolBinOp(..), EqBinOp(..),
  LABinOp(..), OrdBinOp(..), UFunc(..), UFuncB(..), UFuncVV(..), UFuncVN(..),
  VVNBinOp(..), VVVBinOp(..), NVVBinOp(..))
import Language.Drasil.Code.Imperative.Comments (getComment)
import Language.Drasil.Code.Imperative.ConceptMatch (conceptToGOOL)
import Language.Drasil.Code.Imperative.GenerateGOOL (auxClass, fApp, fAppProc,
  ctorCall, genModuleWithImports, genModuleWithImportsProc, primaryClass)
import Language.Drasil.Code.Imperative.Helpers (lookupC)
import Language.Drasil.Code.Imperative.Logging (maybeLog, logBody)
import Language.Drasil.Code.Imperative.DrasilState (GenState, DrasilState(..),
  genICName)
import Language.Drasil.Chunk.Code (CodeIdea(codeName), CodeVarChunk, obv,
  quantvar, quantfunc, ccObjVar, DefiningCodeExpr(..))
import Language.Drasil.Chunk.Parameter (ParameterChunk(..), PassBy(..), pcAuto)
import Language.Drasil.Code.CodeQuantityDicts (inFileName, inParams, consts)
import Language.Drasil.Choices (Comments(..), ConstantRepr(..),
  ConstantStructure(..), Structure(..), InternalConcept(..))
import Language.Drasil.CodeSpec (CodeSpec(..))
import Language.Drasil.Code.DataDesc (DataItem, LinePattern(Repeat, Straight),
  Data(Line, Lines, JunkData, Singleton), DataDesc, isLine, isLines, getInputs,
  getPatternInputs)
import Language.Drasil.Literal.Development
import Language.Drasil.Mod (Func(..), FuncData(..), FuncDef(..), FuncStmt(..),
  Mod(..), Name, Description, StateVariable(..), fstdecl)
import qualified Language.Drasil.Mod as M (Class(..))

import Drasil.GOOL (Label, MSBody, MSBlock, VSType, SVariable, SValue,
  MSStatement, MSParameter, SMethod, CSStateVar, SClass, NamedArgs,
  Initializers, SharedProg, OOProg, PermanenceSym(..), bodyStatements,
  BlockSym(..), TypeSym(..), VariableSym(..), var, constant, ScopeSym(..),
  OOVariableSym(..), staticConst, VariableElim(..), ($->), ValueSym(..),
  Literal(..), VariableValue(..), NumericExpression(..), BooleanExpression(..),
  Comparison(..), ValueExpression(..), OOValueExpression(..),
  objMethodCallMixedArgs, List(..), StatementSym(..), AssignStatement(..),
  DeclStatement(..), IOStatement(..), StringStatement(..), ControlStatement(..),
  ifNoElse, VisibilitySym(..), ParameterSym(..), MethodSym(..), OOMethodSym(..),
  pubDVar, privDVar, nonInitConstructor, convType, convTypeOO,
  VisibilityTag(..), CodeType(..), onStateValue)
import qualified Drasil.GOOL as OO (SFile)
import qualified Drasil.GOOL as C (CodeType(List, Array))
import Drasil.GProc (ProcProg)
import qualified Drasil.GProc as Proc (SFile)

import Prelude hiding (sin, cos, tan, log, exp)
import Data.List ((\\), intersect)
import qualified Data.Map as Map (lookup)
import Control.Monad (liftM2,liftM3)
import Control.Monad.State (get)
import Control.Lens ((^.))

-- | Gets a chunk's 'CodeType', by checking which 'CodeType' the user has chosen to
-- match the chunk's 'Space' to.
codeType :: (HasSpace c) => c -> GenState CodeType
codeType c = spaceCodeType (c ^. typ)

-- | Gets the 'CodeType' for a 'Space', based on the user's choice.
spaceCodeType :: Space -> GenState CodeType
spaceCodeType s = do
  g <- get
  spaceMatches g s

-- | If 'UID' for the variable is matched to a concept, call 'conceptToGOOL' to get
-- the GOOL code for the concept, and return.
-- If 'UID' is for a constant and user has chosen 'Inline', convert the constant's
-- defining 'Expr' to a value with 'convExpr'.
-- Otherwise, just a regular variable: construct it by calling the variable, then
-- call 'valueOf' to reference its value.
value :: (OOProg r) => UID -> Name -> VSType r -> r (Scope r) -> GenState (SValue r)
value u s t scp = do
  g <- get
  let cs = codeSpec g
      mm = constMap cs
      constDef = do
        cd <- Map.lookup u mm
        maybeInline (conStruct g) cd
      maybeInline Inline m = Just m
      maybeInline _ _ = Nothing
      cm = concMatches g
      cdCncpt = Map.lookup u cm
  val <- maybe (valueOf <$> variable s t scp) (convExpr scp . (^. codeExpr)) constDef
  return $ maybe val conceptToGOOL cdCncpt

-- | If variable is an input, construct it with 'var' and pass to inputVariable.
-- If variable is a constant and 'Var' constant representation is chosen,
-- construct it with 'var' and pass to 'constVariable'.
-- If variable is a constant and 'Const' constant representation is chosen,
-- construct it with 'staticVar' and pass to 'constVariable'.
-- If variable is neither, just construct it with 'var' and return it.
variable :: (OOProg r) => Name -> VSType r -> r (Scope r) -> GenState (SVariable r)
variable s t scp = do
  g <- get
  let cs = codeSpec g
      defFunc Var = \nm tp -> var nm tp scp
      defFunc Const = staticConst
  if s `elem` map codeName (inputs cs)
    then inputVariable (inStruct g) Var (var s t scp) scp
    else if s `elem` map codeName (constants $ codeSpec g)
      then constVariable (conStruct g) (conRepr g)
                          ((defFunc $ conRepr g) s t) scp
      else return $ var s t scp

-- | If 'Unbundled' inputs, just return variable as-is.
-- If 'Bundled' inputs, access variable through object, where the object is self
-- if current module is InputParameters, 'inParams' otherwise.
-- Final case is for when 'constVariable' calls inputVariable, when user chooses
-- WithInputs for constant structure, inputs are 'Bundled', and constant
-- representation is 'Const'. Variable should be accessed through class, so
-- 'classVariable' is called.
inputVariable :: (OOProg r) => Structure -> ConstantRepr -> SVariable r ->
  r (Scope r) -> GenState (SVariable r)
inputVariable Unbundled _ v _ = return v
inputVariable Bundled Var v scp = do
  g <- get
  inClsName <- genICName InputParameters
  ip <- mkVar (quantvar inParams) scp
  return $ if currentClass g == inClsName then objVarSelf v else ip $-> v
inputVariable Bundled Const v scp = do
  ip <- mkVar (quantvar inParams) scp
  classVariable ip v

-- | If 'Unbundled' constants, just return variable as-is.
-- If 'Bundled' constants and 'Var' constant representation, access variable
-- through 'consts' object.
-- If 'Bundled' constants and 'Const' constant representation, access variable
-- through class, so call 'classVariable'.
-- If constants stored 'WithInputs', call 'inputVariable'.
-- If constants are 'Inline'd, the generator should not be attempting to make a
-- variable for one of the constants.
constVariable :: (OOProg r) => ConstantStructure -> ConstantRepr ->
  SVariable r -> r (Scope r) -> GenState (SVariable r)
constVariable (Store Unbundled) _ v _ = return v
constVariable (Store Bundled) Var v scp = do
  cs <- mkVar (quantvar consts) scp
  return $ cs $-> v
constVariable (Store Bundled) Const v scp = do
  cs <- mkVar (quantvar consts) scp
  classVariable cs v
constVariable WithInputs cr v scp = do
  g <- get
  inputVariable (inStruct g) cr v scp
constVariable Inline _ _ _ = error $ "mkVar called on a constant, but user " ++
  "chose to Inline constants. Generator has a bug."

-- | For generating GOOL for a variable that is accessed through a class.
-- If the variable is not in the export map, then it is not a public class variable
-- and cannot be accessed, so throw an error.
-- If the variable is exported by the current module, use 'classVar'.
-- If the variable is exported by a different module, use 'extClassVar'.
classVariable :: (OOProg r) => SVariable r -> SVariable r ->
  GenState (SVariable r)
classVariable c v = do
  g <- get
  let checkCurrent m = if currentModule g == m then classVar else extClassVar
  return $ do
    v' <- v
    let nm = variableName v'
    maybe (error $ "Variable " ++ nm ++ " missing from export map")
      checkCurrent (Map.lookup nm (eMap g)) (onStateValue variableType c) v

-- | Generates a GOOL Value for a variable represented by a 'CodeVarChunk'.
mkVal :: (OOProg r) => CodeVarChunk -> r (Scope r) -> GenState (SValue r)
mkVal v scp = do
  t <- codeType v
  let toGOOLVal Nothing = value (v ^. uid) (codeName v) (convTypeOO t) scp
      toGOOLVal (Just o) = do
        ot <- codeType o
        return $ valueOf $ objVar (var (codeName o) (convTypeOO ot) scp)
          (var (codeName v) (convTypeOO t) local)
  toGOOLVal (v ^. obv)

-- | Generates a GOOL Variable for a variable represented by a 'CodeVarChunk'.
mkVar :: (OOProg r) => CodeVarChunk -> r (Scope r) -> GenState (SVariable r)
mkVar v scp = do
  t <- codeType v
  let toGOOLVar Nothing = variable (codeName v) (convTypeOO t) scp
      toGOOLVar (Just o) = do
        ot <- codeType o
        return $ objVar (var (codeName o) (convTypeOO ot) scp)
          (var (codeName v) (convTypeOO t) local)
  toGOOLVar (v ^. obv)

-- | Generates a GOOL Parameter for a parameter represented by a 'ParameterChunk'.
mkParam :: (OOProg r) => ParameterChunk -> GenState (MSParameter r)
mkParam p = do
  v <- mkVar (quantvar p) local
  return $ paramFunc (passBy p) v
  where paramFunc Ref = pointerParam
        paramFunc Val = param

-- | Generates a public function.
publicFunc :: (OOProg r) => Label -> VSType r -> Description ->
  [ParameterChunk] -> Maybe Description -> [MSBlock r] ->
  GenState (SMethod r)
publicFunc n t = genMethod (function n public t) n

-- | Generates a public method.
publicMethod :: (OOProg r) => Label -> VSType r -> Description ->
  [ParameterChunk] -> Maybe Description -> [MSBlock r] ->
  GenState (SMethod r)
publicMethod n t = genMethod (method n public dynamic t) n

-- | Generates a private method.
privateMethod :: (OOProg r) => Label -> VSType r -> Description ->
  [ParameterChunk] -> Maybe Description -> [MSBlock r] ->
  GenState (SMethod r)
privateMethod n t = genMethod (method n private dynamic t) n

-- | Generates a public function, defined by its inputs and outputs.
publicInOutFunc :: (OOProg r) => Label -> Description -> [CodeVarChunk] ->
  [CodeVarChunk] -> [MSBlock r] -> GenState (SMethod r)
publicInOutFunc n = genInOutFunc (inOutFunc n public) (docInOutFunc n public) n

-- | Generates a private method, defined by its inputs and outputs.
privateInOutMethod :: (OOProg r) => Label -> Description -> [CodeVarChunk] ->
  [CodeVarChunk] -> [MSBlock r] -> GenState (SMethod r)
privateInOutMethod n = genInOutFunc (inOutMethod n private dynamic) (docInOutMethod n private dynamic) n

-- | Generates a constructor.
genConstructor :: (OOProg r) => Label -> Description -> [ParameterChunk] ->
  [MSBlock r] -> GenState (SMethod r)
genConstructor n desc p = genMethod nonInitConstructor n desc p Nothing

-- | Generates a constructor that includes initialization of variables.
genInitConstructor :: (OOProg r) => Label -> Description -> [ParameterChunk]
  -> Initializers r -> [MSBlock r] -> GenState (SMethod r)
genInitConstructor n desc p is = genMethod (`constructor` is) n desc p
  Nothing

-- | Generates a function or method using the passed GOOL constructor. Other
-- parameters are the method's name, description, list of parameters,
-- description of what is returned (if applicable), and body.
genMethod :: (OOProg r) => ([MSParameter r] -> MSBody r -> SMethod r) ->
  Label -> Description -> [ParameterChunk] -> Maybe Description -> [MSBlock r]
  -> GenState (SMethod r)
genMethod f n desc p r b = do
  g <- get
  vars <- mapM ((`mkVar` local) . quantvar) p
  ps <- mapM mkParam p
  bod <- logBody n vars b
  let fn = f ps bod
  pComms <- mapM getComment p
  return $ if CommentFunc `elem` commented g
    then docFunc desc pComms r fn else fn

-- | Generates a function or method defined by its inputs and outputs.
-- Parameters are: the GOOL constructor to use, the equivalent GOOL constructor
-- for a documented function/method, the visibility, permanence, name, description,
-- list of inputs, list of outputs, and body.
genInOutFunc :: (OOProg r) => ([SVariable r] -> [SVariable r] ->
    [SVariable r] -> MSBody r -> SMethod r) ->
  (String -> [(String, SVariable r)] -> [(String, SVariable r)] ->
    [(String, SVariable r)] -> MSBody r -> SMethod r)
  -> Label -> Description -> [CodeVarChunk] -> [CodeVarChunk] ->
  [MSBlock r] -> GenState (SMethod r)
genInOutFunc f docf n desc ins' outs' b = do
  g <- get
  let ins = ins' \\ outs'
      outs = outs' \\ ins'
      both = ins' `intersect` outs'
  inVs <- mapM (`mkVar` local) ins
  outVs <- mapM (`mkVar` local) outs
  bothVs <- mapM (`mkVar` local) both
  bod <- logBody n (bothVs ++ inVs) b
  pComms <- mapM getComment ins
  oComms <- mapM getComment outs
  bComms <- mapM getComment both
  return $ if CommentFunc `elem` commented g
    then docf desc (zip pComms inVs) (zip oComms outVs) (zip
    bComms bothVs) bod else f inVs outVs bothVs bod

-- | Converts an 'Expr' to a GOOL Value.
convExpr :: (OOProg r) => r (Scope r) -> CodeExpr -> GenState (SValue r)
convExpr _ (Lit (Dbl d)) = do
  sm <- spaceCodeType Real
  let getLiteral Double = litDouble d
      getLiteral Float = litFloat (realToFrac d)
      getLiteral _ = error "convExpr: Real space matched to invalid CodeType; should be Double or Float"
  return $ getLiteral sm
convExpr scp (Lit (ExactDbl d)) = convExpr scp $ Lit . Dbl $ fromInteger d
convExpr _ (Lit (Int i))      = return $ litInt i
convExpr _ (Lit (Str s))      = return $ litString s
convExpr _ (Lit (Perc a b)) = do
  sm <- spaceCodeType Rational
  let getLiteral Double = litDouble
      getLiteral Float = litFloat . realToFrac
      getLiteral _ = error "convExpr: Rational space matched to invalid CodeType; should be Double or Float"
  return $ getLiteral sm (fromIntegral a / (10 ** fromIntegral b))
convExpr scp (AssocA Add l) = foldl1 (#+)  <$> mapM (convExpr scp) l
convExpr scp (AssocA Mul l) = foldl1 (#*)  <$> mapM (convExpr scp) l
convExpr scp (AssocB And l) = foldl1 (?&&) <$> mapM (convExpr scp) l
convExpr scp (AssocB Or l)  = foldl1 (?||) <$> mapM (convExpr scp) l
convExpr scp (C c)   = do
  g <- get
  let v = quantvar (lookupC g c)
  mkVal v scp
convExpr scp (FCall c x ns) = convCall c x ns fApp libFuncAppMixedArgs scp
convExpr scp (New c x ns) = convCall c x ns (\m _ -> ctorCall m)
  (\m _ -> libNewObjMixedArgs m) scp
convExpr scp (Message a m x ns) = do
  g <- get
  let info = sysinfodb $ codeSpec g
      objCd = quantvar (symbResolve info a)
  o <- (`mkVal` scp) objCd
  convCall m x ns
    (\_ n t ps nas -> return (objMethodCallMixedArgs t o n ps nas))
    (\_ n t -> objMethodCallMixedArgs t o n) scp
convExpr scp (Field o f) = do
  g <- get
  let ob  = quantvar (lookupC g o)
      fld = quantvar (lookupC g f)
  v <- mkVar (ccObjVar ob fld) scp
  return $ valueOf v
convExpr scp (UnaryOp o u)    = fmap (unop o) (convExpr scp u)
convExpr scp (UnaryOpB o u)   = fmap (unopB o) (convExpr scp u)
convExpr scp (UnaryOpVV o u)  = fmap (unopVV o) (convExpr scp u)
convExpr scp (UnaryOpVN o u)  = fmap (unopVN o) (convExpr scp u)
convExpr _ (ArithBinaryOp Frac (Lit (Int a)) (Lit (Int b))) = do -- hack to deal with integer division
  sm <- spaceCodeType Rational
  let getLiteral Double = litDouble (fromIntegral a) #/ litDouble (fromIntegral b)
      getLiteral Float = litFloat (fromIntegral a) #/ litFloat (fromIntegral b)
      getLiteral _ = error "convExpr: Rational space matched to invalid CodeType; should be Double or Float"
  return $ getLiteral sm
convExpr scp (ArithBinaryOp o a b) = liftM2 (arithBfunc o)
                                      (convExpr scp a) (convExpr scp b)
convExpr scp (BoolBinaryOp o a b)  = liftM2 (boolBfunc o)
                                      (convExpr scp a) (convExpr scp b)
convExpr scp (LABinaryOp o a b)    = liftM2 (laBfunc o)
                                      (convExpr scp a) (convExpr scp b)
convExpr scp (EqBinaryOp o a b)    = liftM2 (eqBfunc o)
                                      (convExpr scp a) (convExpr scp b)
convExpr scp (OrdBinaryOp o a b)   = liftM2 (ordBfunc o)
                                      (convExpr scp a) (convExpr scp b)
convExpr scp (VVVBinaryOp o a b)   = liftM2 (vecVecVecBfunc o)
                                      (convExpr scp a) (convExpr scp b)
convExpr scp (VVNBinaryOp o a b)   = liftM2 (vecVecNumBfunc o)
                                      (convExpr scp a) (convExpr scp b)
convExpr scp (NVVBinaryOp o a b)   = liftM2 (numVecVecBfunc o)
                                      (convExpr scp a) (convExpr scp b)
convExpr scp (Case c l)            = doit l -- FIXME this is sub-optimal
  where
    doit [] = error "should never happen" -- TODO: change error message?
    doit [(e,_)] = convExpr scp e -- should always be the else clause
    doit ((e,cond):xs) = liftM3 inlineIf (convExpr scp cond) (convExpr scp e)
      (convExpr scp (Case c xs))
convExpr scp (Matrix [l]) = do
  ar <- mapM (convExpr scp) l
                                    -- hd will never fail here
  return $ litArray (fmap valueType (head ar)) ar
convExpr _ Matrix{} = error "convExpr: Matrix"
convExpr _ Operator{} = error "convExpr: Operator"
convExpr scp (RealI c ri)  = do
  g <- get
  convExpr scp $ renderRealInt (lookupC g c) ri

-- | Generates a function/method call, based on the 'UID' of the chunk representing
-- the function, the list of argument 'Expr's, the list of named argument 'Expr's,
-- the function call generator to use, and the library version of the function
-- call generator (used if the function is in the library export map).
convCall :: (OOProg r) => UID -> [CodeExpr] -> [(UID, CodeExpr)] ->
  (Name -> Name -> VSType r -> [SValue r] -> NamedArgs r ->
  GenState (SValue r)) -> (Name -> Name -> VSType r -> [SValue r]
  -> NamedArgs r -> SValue r) -> r (Scope r) -> GenState (SValue r)
convCall c x ns f libf scp = do
  g <- get
  let info = sysinfodb $ codeSpec g
      mem = eMap g
      lem = libEMap g
      funcCd = quantfunc (symbResolve info c)
      funcNm = codeName funcCd
  funcTp <- codeType funcCd
  args <- mapM (convExpr scp) x
  nms <- mapM ((`mkVar` scp) . quantvar . symbResolve info . fst) ns
  nargs <- mapM (convExpr scp . snd) ns
  maybe (maybe (error $ "Call to non-existent function " ++ funcNm)
      (\m -> return $ libf m funcNm (convTypeOO funcTp) args (zip nms nargs))
      (Map.lookup funcNm lem))
    (\m -> f m funcNm (convTypeOO funcTp) args (zip nms nargs))
    (Map.lookup funcNm mem)

-- | Converts a 'Constraint' to a 'CodeExpr'.
renderC :: (HasUID c, HasSymbol c) => c -> Constraint CodeExpr -> CodeExpr
renderC s (Range _ rr)         = renderRealInt s rr

-- | Converts an interval ('RealInterval') to a 'CodeExpr'.
renderRealInt :: (HasUID c, HasSymbol c) => c -> RealInterval CodeExpr CodeExpr -> CodeExpr
renderRealInt s (Bounded (Inc, a) (Inc, b)) = (a $<= sy s) $&& (sy s $<= b)
renderRealInt s (Bounded (Inc, a) (Exc, b)) = (a $<= sy s) $&& (sy s $<  b)
renderRealInt s (Bounded (Exc, a) (Inc, b)) = (a $<  sy s) $&& (sy s $<= b)
renderRealInt s (Bounded (Exc, a) (Exc, b)) = (a $<  sy s) $&& (sy s $<  b)
renderRealInt s (UpTo    (Inc, a))          = sy s $<= a
renderRealInt s (UpTo    (Exc, a))          = sy s $<  a
renderRealInt s (UpFrom  (Inc, a))          = sy s $>= a
renderRealInt s (UpFrom  (Exc, a))          = sy s $>  a

-- | Maps a 'UFunc' to the corresponding GOOL unary function.
unop :: (SharedProg r) => UFunc -> (SValue r -> SValue r)
unop Sqrt = (#/^)
unop Log  = log
unop Ln   = ln
unop Abs  = (#|)
unop Exp  = exp
unop Sin  = sin
unop Cos  = cos
unop Tan  = tan
unop Csc  = csc
unop Sec  = sec
unop Cot  = cot
unop Arcsin = arcsin
unop Arccos = arccos
unop Arctan = arctan
unop Neg  = (#~)

-- | Similar to 'unop', but for the 'Not' constructor.
unopB :: (SharedProg r) => UFuncB -> (SValue r -> SValue r)
unopB Not = (?!)

-- | Similar to 'unop', but for vectors.
unopVN :: (SharedProg r) => UFuncVN -> (SValue r -> SValue r)
unopVN Dim = listSize
unopVN Norm = error "unop: Norm not implemented" -- TODO

-- | Similar to 'unop', but for vectors.
unopVV :: (SharedProg r) => UFuncVV -> (SValue r -> SValue r)
unopVV NegV = error "unop: Negation on Vectors not implemented" -- TODO

-- Maps an 'ArithBinOp' to it's corresponding GOOL binary function.
arithBfunc :: (SharedProg r) => ArithBinOp -> (SValue r -> SValue r -> SValue r)
arithBfunc Pow  = (#^)
arithBfunc Subt = (#-)
arithBfunc Frac = (#/)

-- Maps a 'BoolBinOp' to it's corresponding GOOL binary function.
boolBfunc :: BoolBinOp -> (SValue r -> SValue r -> SValue r)
boolBfunc Impl = error "convExpr :=>"
boolBfunc Iff  = error "convExpr :<=>"

-- Maps an 'EqBinOp' to it's corresponding GOOL binary function.
eqBfunc :: (SharedProg r) => EqBinOp -> (SValue r -> SValue r -> SValue r)
eqBfunc Eq  = (?==)
eqBfunc NEq = (?!=)

-- Maps an 'LABinOp' to it's corresponding GOOL binary function.
laBfunc :: (SharedProg r) => LABinOp -> (SValue r -> SValue r -> SValue r)
laBfunc Index = listAccess

-- Maps an 'OrdBinOp' to it's corresponding GOOL binary function.
ordBfunc :: (SharedProg r) => OrdBinOp -> (SValue r -> SValue r -> SValue r)
ordBfunc Gt  = (?>)
ordBfunc Lt  = (?<)
ordBfunc LEq = (?<=)
ordBfunc GEq = (?>=)

-- Maps a 'VVVBinOp' to it's corresponding GOOL binary function.
vecVecVecBfunc :: VVVBinOp -> (SValue r -> SValue r -> SValue r)
vecVecVecBfunc Cross = error "bfunc: Cross not implemented"
vecVecVecBfunc VAdd = error "bfunc: Vector addition not implemented"
vecVecVecBfunc VSub = error "bfunc: Vector subtraction not implemented"

-- Maps a 'VVNBinOp' to it's corresponding GOOL binary function.
vecVecNumBfunc :: VVNBinOp -> (SValue r -> SValue r -> SValue r)
vecVecNumBfunc Dot = error "convExpr DotProduct"

-- Maps a 'NVVBinOp' to it's corresponding GOOL binary function.
numVecVecBfunc :: NVVBinOp -> (SValue r -> SValue r -> SValue r)
numVecVecBfunc Scale = error "convExpr Scaling of Vectors"

-- medium hacks --

-- | Converts a 'Mod' to GOOL.
genModDef :: (OOProg r) => Mod -> GenState (OO.SFile r)
genModDef (Mod n desc is cs fs) = genModuleWithImports n desc is (map (fmap
  Just . genFunc publicFunc []) fs)
  (case cs of [] -> []
              (cl:cls) -> fmap Just (genClass primaryClass cl) :
                map (fmap Just . genClass auxClass) cls)

-- | Converts a 'Mod'\'s functions to GOOL.
genModFuncs :: (OOProg r) => Mod -> [GenState (SMethod r)]
genModFuncs (Mod _ _ _ _ fs) = map (genFunc publicFunc []) fs

-- | Converts a 'Mod'\'s classes to GOOL.
genModClasses :: (OOProg r) => Mod -> [GenState (SClass r)]
genModClasses (Mod _ _ _ cs _) = map (genClass auxClass) cs

-- | Converts a Class (from the Mod AST) to GOOL.
-- The class generator to use is passed as a parameter.
genClass :: (OOProg r) => (Name -> Maybe Name -> Description -> [CSStateVar r]
  -> GenState [SMethod r] -> GenState [SMethod r] -> GenState (SClass r)) ->
  M.Class -> GenState (SClass r)
genClass f (M.ClassDef n i desc svs cs ms) = let svar Pub = pubDVar
                                                 svar Priv = privDVar
  in do
  svrs <- mapM (\(SV s v) -> fmap (svar s . var' (codeName v) local .
                convTypeOO) (codeType v)) svs
  f n i desc svrs (mapM (genFunc publicMethod svs) cs) 
                  (mapM (genFunc publicMethod svs) ms)

-- | Converts a 'Func' (from the Mod AST) to GOOL.
-- The function generator to use is passed as a parameter. Automatically adds
-- variable declaration statements for any undeclared variables. For methods,
-- the list of StateVariables is needed so they can be included in the list of
-- declared variables.
genFunc :: (OOProg r) => (Name -> VSType r -> Description -> [ParameterChunk]
  -> Maybe Description -> [MSBlock r] -> GenState (SMethod r)) ->
  [StateVariable] -> Func -> GenState (SMethod r)
genFunc f svs (FDef (FuncDef n desc parms o rd s)) = do
  g <- get
  stmts <- mapM (convStmt local) s
  vars <- mapM (`mkVar` local) (fstdecl (sysinfodb $ codeSpec g) s
    \\ (map quantvar parms ++ map stVar svs))
  t <- spaceCodeType o
  f n (convTypeOO t) desc parms rd [block $ map varDec vars, block stmts]
genFunc _ svs (FDef (CtorDef n desc parms i s)) = do
  g <- get
  inits <- mapM (convExpr local . snd) i
  initvars <- mapM ((\iv -> fmap (var' (codeName iv) local . convTypeOO)
    (codeType iv)) . fst) i
  stmts <- mapM (convStmt local) s
  vars <- mapM (`mkVar` local) (fstdecl (sysinfodb $ codeSpec g) s
    \\ (map quantvar parms ++ map stVar svs))
  genInitConstructor n desc parms (zip initvars inits)
    [block $ map varDec vars, block stmts]
genFunc _ _ (FData (FuncData n desc ddef)) = genDataFunc n desc ddef

-- | Converts a 'FuncStmt' to a GOOL Statement.
convStmt :: (OOProg r) => r (Scope r) -> FuncStmt -> GenState (MSStatement r)
convStmt scp (FAsg v (Matrix [es])) = do
  els <- mapM (convExpr scp) es
  v' <- mkVar v scp
  t <- codeType v
  let listFunc (C.List _) = litList
      listFunc (C.Array _) = litArray
      listFunc _ = error "Type mismatch between variable and value in assignment FuncStmt"
  l <- maybeLog v' local
  return $ multi $ assign v' (listFunc t (listInnerType $ fmap variableType v')
    els) : l
convStmt scp (FAsg v e) = do
  e' <- convExpr scp e
  v' <- mkVar v scp
  l <- maybeLog v' local
  return $ multi $ assign v' e' : l
convStmt scp (FAsgIndex v i e) = do
  e' <- convExpr scp e
  v' <- mkVar v scp
  t <- codeType v
  let asgFunc (C.List _) = valStmt $ listSet (valueOf v') (litInt i) e'
      asgFunc (C.Array _) = assign (arrayElem i v') e'
      asgFunc _ = error "FAsgIndex used with non-indexed value"
      vi = arrayElem i v'
  l <- maybeLog vi local
  return $ multi $ asgFunc t : l
convStmt scp (FFor v start end step st) = do
  stmts <- mapM (convStmt scp) st
  vari <- mkVar v scp
  start' <- convExpr scp start
  end' <- convExpr scp end
  step' <- convExpr scp step
  return $ forRange vari start' end' step' (bodyStatements stmts)
convStmt scp (FForEach v e st) = do
  stmts <- mapM (convStmt scp) st
  vari <- mkVar v scp
  e' <- convExpr scp e
  return $ forEach vari e' (bodyStatements stmts)
convStmt scp (FWhile e st) = do
  stmts <- mapM (convStmt scp) st
  e' <- convExpr scp e
  return $ while e' (bodyStatements stmts)
convStmt scp (FCond e tSt []) = do
  stmts <- mapM (convStmt scp) tSt
  e' <- convExpr scp e
  return $ ifNoElse [(e', bodyStatements stmts)]
convStmt scp (FCond e tSt eSt) = do
  stmt1 <- mapM (convStmt scp) tSt
  stmt2 <- mapM (convStmt scp) eSt
  e' <- convExpr scp e
  return $ ifCond [(e', bodyStatements stmt1)] (bodyStatements stmt2)
convStmt scp (FRet e) = do
  e' <- convExpr scp e
  return $ returnStmt e'
convStmt _ (FThrow s) = return $ throw s
convStmt scp (FTry t c) = do
  stmt1 <- mapM (convStmt scp) t
  stmt2 <- mapM (convStmt scp) c
  return $ tryCatch (bodyStatements stmt1) (bodyStatements stmt2)
convStmt _ FContinue = return continue
convStmt scp (FDecDef v (Matrix [[]])) = do
  vari <- mkVar v scp
  let convDec (C.List _) = listDec 0 vari
      convDec (C.Array _) = arrayDec 0 vari
      convDec _ = varDec vari
  fmap convDec (codeType v)
convStmt scp (FDecDef v e) = do
  v' <- mkVar v scp
  l <- maybeLog v' local
  t <- codeType v
  let convDecDef (Matrix [lst]) = do
        let contDecDef (C.List _) = listDecDef
            contDecDef (C.Array _) = arrayDecDef
            contDecDef _ = error "Type mismatch between variable and value in declare-define FuncStmt"
        e' <- mapM (convExpr scp) lst
        return $ contDecDef t v' e'
      convDecDef _ = do
        e' <- convExpr scp e
        return $ varDecDef v' e'
  dd <- convDecDef e
  return $ multi $ dd : l
convStmt scp (FFuncDef f ps sts) = do
  f' <- mkVar (quantvar f) scp
  pms <- mapM ((`mkVar` scp) . quantvar) ps
  b <- mapM (convStmt scp) sts
  return $ funcDecDef f' pms (bodyStatements b)
convStmt scp (FVal e) = do
  e' <- convExpr scp e
  return $ valStmt e'
convStmt scp (FMulti ss) = do
  stmts <- mapM (convStmt scp) ss
  return $ multi stmts
convStmt scp (FAppend a b) = do
  a' <- convExpr scp a
  b' <- convExpr scp b
  return $ valStmt $ listAppend a' b'

-- | Generates a function that reads a file whose format is based on the passed
-- 'DataDesc'.
genDataFunc :: (OOProg r) => Name -> Description -> DataDesc ->
  GenState (SMethod r)
genDataFunc nameTitle desc ddef = do
  let parms = getInputs ddef
  bod <- readData ddef local
  publicFunc nameTitle void desc (map pcAuto $ quantvar inFileName : parms)
    Nothing bod

-- this is really ugly!!
-- | Read from a data description into a 'MSBlock' of 'MSStatement's.
readData :: (OOProg r) => DataDesc -> r (Scope r) -> GenState [MSBlock r]
readData ddef scope = do
  inD <- mapM (`inData` scope) ddef
  v_filename <- mkVal (quantvar inFileName) scope
  return [block $
    varDec (var_infile scope) :
    (if any (\d -> isLine d || isLines d) ddef then [varDec (var_line scope),
    listDec 0 (var_linetokens scope)] else []) ++
    [listDec 0 (var_lines scope) | any isLines ddef] ++ openFileR (var_infile scope)
    v_filename : concat inD ++ [closeFile (v_infile scope) ]]
  where inData :: (OOProg r) => Data -> r (Scope r) -> GenState [MSStatement r]
        inData (Singleton v) scp = do
            vv <- mkVar v scp
            l <- maybeLog vv scp
            return [multi $ getFileInput (v_infile scp) vv : l]
        inData JunkData scp = return [discardFileLine (v_infile scp)]
        inData (Line lp d) scp = do
          lnI <- lineData Nothing lp scp
          logs <- getEntryVarLogs lp scp
          return $ [getFileInputLine (v_infile scp) (var_line scp),
            stringSplit d (var_linetokens scp) (v_line scp)] ++ lnI ++ logs
        inData (Lines lp ls d) scp = do
          lnV <- lineData (Just "_temp") lp scp
          logs <- getEntryVarLogs lp scp
          let readLines Nothing = [getFileInputAll (v_infile scp) (var_lines scp),
                forRange var_i (litInt 0) (listSize (v_lines scp)) (litInt 1)
                  (bodyStatements $ stringSplit d (var_linetokens scp) (
                  listAccess (v_lines scp) v_i) : lnV)]
              readLines (Just numLines) = [forRange var_i (litInt 0)
                (litInt numLines) (litInt 1)
                (bodyStatements $
                  [getFileInputLine (v_infile scp) (var_line scp),
                   stringSplit d (var_linetokens scp) (v_line scp)
                  ] ++ lnV)]
          return $ readLines ls ++ logs
        ---------------
        lineData :: (OOProg r) => Maybe String -> LinePattern -> r (Scope r) ->
          GenState [MSStatement r]
        lineData s p@(Straight _) scp = do
          vs <- getEntryVars s p scp
          return [stringListVals vs (v_linetokens scp)]
        lineData s p@(Repeat ds) scp = do
          vs <- getEntryVars s p scp
          sequence $ clearTemps s ds scp ++ return
            (stringListLists vs (v_linetokens scp)) : appendTemps s ds scp
        ---------------
        clearTemps :: (OOProg r) => Maybe String -> [DataItem] -> r (Scope r) ->
          [GenState (MSStatement r)]
        clearTemps Nothing    _  _   = []
        clearTemps (Just sfx) es scp = map (\v -> clearTemp sfx v scp) es
        ---------------
        clearTemp :: (OOProg r) => String -> DataItem -> r (Scope r) ->
          GenState (MSStatement r)
        clearTemp sfx v scp = fmap (\t -> listDecDef (var (codeName v ++ sfx)
          (listInnerType $ convTypeOO t) scp) []) (codeType v)
        ---------------
        appendTemps :: (OOProg r) => Maybe String -> [DataItem] -> r (Scope r)
          -> [GenState (MSStatement r)]
        appendTemps Nothing _ _ = []
        appendTemps (Just sfx) es scp = map (\v -> appendTemp sfx v scp) es
        ---------------
        appendTemp :: (OOProg r) => String -> DataItem -> r (Scope r) ->
          GenState (MSStatement r)
        appendTemp sfx v scp = fmap (\t -> valStmt $ listAppend
          (valueOf $ var (codeName v) (convTypeOO t) scp)
          (valueOf $ var (codeName v ++ sfx) (convTypeOO t) scp)) (codeType v)

-- | Get entry variables.
getEntryVars :: (OOProg r) => Maybe String -> LinePattern -> r (Scope r) ->
  GenState [SVariable r]
getEntryVars s lp scp = mapM (maybe (`mkVar` scp) (\st v -> codeType v >>= 
  ((\sNm tp -> variable sNm tp scp) (codeName v ++ st) . listInnerType . convTypeOO))
    s) (getPatternInputs lp)

-- | Get entry variable logs.
getEntryVarLogs :: (OOProg r) => LinePattern -> r (Scope r) ->
  GenState [MSStatement r]
getEntryVarLogs lp scp = do
  vs <- getEntryVars Nothing lp scp
  logs <- mapM (`maybeLog` local) vs
  return $ concat logs

-- Procedural Versions --

-- | If 'UID' for the variable is matched to a concept, call 'conceptToGOOL' to get
-- the GOOL code for the concept, and return.
-- If 'UID' is for a constant and user has chosen 'Inline', convert the constant's
-- defining 'Expr' to a value with 'convExprProc'.
-- Otherwise, just a regular variable: construct it by calling the variable, then
-- call 'valueOf' to reference its value.
valueProc :: (SharedProg r) => UID -> Name -> VSType r -> r (Scope r) -> GenState (SValue r)
valueProc u s t scp = do
  g <- get
  let cs = codeSpec g
      mm = constMap cs
      constDef = do
        cd <- Map.lookup u mm
        maybeInline (conStruct g) cd
      maybeInline Inline m = Just m
      maybeInline _ _ = Nothing
      cm = concMatches g
      cdCncpt = Map.lookup u cm
  val <- maybe (valueOf <$> variableProc s t scp)
                (convExprProc scp . (^. codeExpr)) constDef
  return $ maybe val conceptToGOOL cdCncpt

-- | If variable is an input, construct it with 'var' and pass to inputVariableProc.
-- If variable is a constant and 'Var' constant representation is chosen,
-- construct it with 'var' and pass to 'constVariableProc'.
-- If variable is a constant and 'Const' constant representation is chosen,
-- construct it with 'constant' and pass to 'constVariableProc'.
-- If variable is neither, just construct it with 'var' and return it.
variableProc :: (SharedProg r) => Name -> VSType r -> r (Scope r) -> GenState (SVariable r)
variableProc s t scp = do
  g <- get
  let cs = codeSpec g
      defFunc Var = \nm tp -> var nm tp scp
      defFunc Const = \nm tp -> constant nm tp scp -- This might be wrong
  if s `elem` map codeName (inputs cs)
    then inputVariableProc (inStruct g) Var (var s t scp)
    else if s `elem` map codeName (constants $ codeSpec g)
      then constVariableProc (conStruct g) (conRepr g)
              ((defFunc $ conRepr g) s t)
      else return $ var s t scp

-- | If 'Unbundled' inputs, just return variable as-is.
-- If 'Bundled' inputs, throw an error, since procedural renderers
-- don't support 'Bundled' inputs yet.
inputVariableProc :: (SharedProg r) => Structure -> ConstantRepr -> SVariable r ->
  GenState (SVariable r)
inputVariableProc Unbundled _ v = return v
inputVariableProc Bundled _ _ = error "inputVariableProc: Procedural renderers do not support bundled inputs"

-- | If 'Unbundled' constants, just return variable as-is.
-- If 'Bundled' constants, throw an error, since procedural renderers
-- don't support 'Bundled' constants yet.
-- If constants stored 'WithInputs', call 'inputVariableProc'.
-- If constants are 'Inline'd, the generator should not be attempting to make a
-- variable for one of the constants.
constVariableProc :: (SharedProg r) => ConstantStructure -> ConstantRepr ->
  SVariable r -> GenState (SVariable r)
constVariableProc (Store Unbundled) _ v = return v
constVariableProc (Store Bundled) _ _ = error "constVariableProc: Procedural renderers do not support bundled constants"
constVariableProc WithInputs cr v = do
  g <- get
  inputVariableProc (inStruct g) cr v
constVariableProc Inline _ _ = error $ "mkVar called on a constant, but user " ++
  "chose to Inline constants. Generator has a bug."

-- | Generates a GOOL Value for a variable represented by a 'CodeVarChunk'.
mkValProc :: (SharedProg r) => CodeVarChunk -> r (Scope r) -> GenState (SValue r)
mkValProc v scp = do
  t <- codeType v
  let toGOOLVal Nothing = valueProc (v ^. uid) (codeName v) (convType t) scp
      toGOOLVal (Just _) = error "mkValProc: Procedural renderers do not support objects"
  toGOOLVal (v ^. obv)

-- | Generates a GOOL Variable for a variable represented by a 'CodeVarChunk'.
mkVarProc :: (SharedProg r) => CodeVarChunk -> r (Scope r) -> GenState (SVariable r)
mkVarProc v scp = do
  t <- codeType v
  let toGOOLVar Nothing = variableProc (codeName v) (convType t) scp
      toGOOLVar (Just _) = error "mkVarProc: Procedural renderers do not support objects"
  toGOOLVar (v ^. obv)

-- | Converts a 'Mod' to GOOL.
genModDefProc :: (ProcProg r) => Mod -> GenState (Proc.SFile r)
genModDefProc (Mod n desc is cs fs) = case cs of
  [] -> genModuleWithImportsProc n desc is
          (map (fmap Just . genFuncProc publicFuncProc []) fs)
  _  -> error "genModDefProc: Procedural renderers do not support classes"

-- | Generates a GOOL Parameter for a parameter represented by a 'ParameterChunk'.
mkParamProc :: (SharedProg r) => ParameterChunk -> GenState (MSParameter r)
mkParamProc p = do
  v <- mkVarProc (quantvar p) local
  return $ paramFunc (passBy p) v
  where paramFunc Ref = pointerParam
        paramFunc Val = param

-- | Generates a public function.
publicFuncProc :: (SharedProg r) => Label -> VSType r -> Description ->
  [ParameterChunk] -> Maybe Description -> [MSBlock r] ->
  GenState (SMethod r)
publicFuncProc n t = genMethodProc (function n public t) n

-- | Generates a private function.
privateFuncProc :: (SharedProg r) => Label -> VSType r -> Description ->
  [ParameterChunk] -> Maybe Description -> [MSBlock r] ->
  GenState (SMethod r)
privateFuncProc n t = genMethodProc (function n private t) n

-- | Generates a function or method using the passed GOOL constructor. Other
-- parameters are the method's name, description, list of parameters,
-- description of what is returned (if applicable), and body.
genMethodProc :: (SharedProg r) => ([MSParameter r] -> MSBody r -> SMethod r) ->
  Label -> Description -> [ParameterChunk] -> Maybe Description -> [MSBlock r]
  -> GenState (SMethod r)
genMethodProc f n desc p r b = do
  g <- get
  vars <- mapM ((`mkVarProc` local) . quantvar) p
  ps <- mapM mkParamProc p
  bod <- logBody n vars b
  let fn = f ps bod
  pComms <- mapM getComment p
  return $ if CommentFunc `elem` commented g
    then docFunc desc pComms r fn else fn

-- | Converts a 'Func' (from the Mod AST) to GOOL.
-- The function generator to use is passed as a parameter. Automatically adds
-- variable declaration statements for any undeclared variables. For methods,
-- the list of StateVariables is needed so they can be included in the list of
-- declared variables.
genFuncProc :: (SharedProg r) => (Name -> VSType r -> Description -> [ParameterChunk]
  -> Maybe Description -> [MSBlock r] -> GenState (SMethod r)) ->
  [StateVariable] -> Func -> GenState (SMethod r)
genFuncProc f svs (FDef (FuncDef n desc parms o rd s)) = do
  g <- get
  stmts <- mapM (convStmtProc local) s
  vars <- mapM (`mkVarProc` local) (fstdecl (sysinfodb $ codeSpec g) s
    \\ (map quantvar parms ++ map stVar svs))
  t <- spaceCodeType o
  f n (convType t) desc parms rd [block $ map varDec vars, block stmts]
genFuncProc _ _ (FDef (CtorDef {})) = error "genFuncProc: Procedural renderers do not support constructors"
genFuncProc _ _ (FData (FuncData n desc ddef)) = genDataFuncProc n desc ddef

-- | Converts a 'Mod'\'s functions to GOOL.
genModFuncsProc :: (SharedProg r) => Mod -> [GenState (SMethod r)]
genModFuncsProc (Mod _ _ _ _ fs) = map (genFuncProc publicFuncProc []) fs

-- this is really ugly!!
-- | Read from a data description into a 'MSBlock' of 'MSStatement's.
readDataProc :: (SharedProg r) => DataDesc -> r (Scope r) -> GenState [MSBlock r]
readDataProc ddef scope = do
  inD <- mapM (`inData` scope) ddef
  v_filename <- mkValProc (quantvar inFileName) scope
  return [block $
    varDec (var_infile scope) :
    (if any (\d -> isLine d || isLines d) ddef then [varDec (var_line scope),
    listDec 0 (var_linetokens scope)] else []) ++
    [listDec 0 (var_lines scope) | any isLines ddef] ++ openFileR (var_infile scope)
    v_filename : concat inD ++ [closeFile (v_infile scope) ]]
  where inData :: (SharedProg r) => Data -> r (Scope r) -> GenState [MSStatement r]
        inData (Singleton v) scp = do
            vv <- mkVarProc v scp
            l <- maybeLog vv scp
            return [multi $ getFileInput (v_infile scp) vv : l]
        inData JunkData scp = return [discardFileLine (v_infile scp)]
        inData (Line lp d) scp = do
          lnI <- lineData Nothing lp scp
          logs <- getEntryVarLogsProc lp scp
          return $ [getFileInputLine (v_infile scp) (var_line scp),
            stringSplit d (var_linetokens scp) (v_line scp)] ++ lnI ++ logs
        inData (Lines lp ls d) scp = do
          lnV <- lineData (Just "_temp") lp scp
          logs <- getEntryVarLogsProc lp scp
          let readLines Nothing = [getFileInputAll (v_infile scp) (var_lines scp),
                forRange var_i (litInt 0) (listSize (v_lines scp)) (litInt 1)
                  (bodyStatements $ stringSplit d (var_linetokens scp) (
                  listAccess (v_lines scp) v_i) : lnV)]
              readLines (Just numLines) = [forRange var_i (litInt 0)
                (litInt numLines) (litInt 1)
                (bodyStatements $
                  [getFileInputLine (v_infile scp) (var_line scp),
                   stringSplit d (var_linetokens scp) (v_line scp)
                  ] ++ lnV)]
          return $ readLines ls ++ logs
        ---------------
        lineData :: (SharedProg r) => Maybe String -> LinePattern -> r (Scope r) ->
          GenState [MSStatement r]
        lineData s p@(Straight _) scp = do
          vs <- getEntryVarsProc s p scp
          return [stringListVals vs (v_linetokens scp)]
        lineData s p@(Repeat ds) scp = do
          vs <- getEntryVarsProc s p scp
          sequence $ clearTemps s ds scp ++ return
            (stringListLists vs (v_linetokens scp)) : appendTemps s ds scp
        ---------------
        clearTemps :: (SharedProg r) => Maybe String -> [DataItem] -> r (Scope r) ->
          [GenState (MSStatement r)]
        clearTemps Nothing    _  _   = []
        clearTemps (Just sfx) es scp = map (\v -> clearTemp sfx v scp) es
        ---------------
        clearTemp :: (SharedProg r) => String -> DataItem -> r (Scope r) ->
          GenState (MSStatement r)
        clearTemp sfx v scp = fmap (\t -> listDecDef (var (codeName v ++ sfx)
          (listInnerType $ convType t) scp) []) (codeType v)
        ---------------
        appendTemps :: (SharedProg r) => Maybe String -> [DataItem] -> r (Scope r)
          -> [GenState (MSStatement r)]
        appendTemps Nothing _ _ = []
        appendTemps (Just sfx) es scp = map (\v -> appendTemp sfx v scp) es
        ---------------
        appendTemp :: (SharedProg r) => String -> DataItem -> r (Scope r) ->
          GenState (MSStatement r)
        appendTemp sfx v scp = fmap (\t -> valStmt $ listAppend
          (valueOf $ var (codeName v) (convType t) scp)
          (valueOf $ var (codeName v ++ sfx) (convType t) scp)) (codeType v)

-- | Get entry variables.
getEntryVarsProc :: (SharedProg r) => Maybe String -> LinePattern -> r (Scope r) ->
  GenState [SVariable r]
getEntryVarsProc s lp scp = mapM (maybe (`mkVarProc` scp) (\st v -> codeType v >>= 
  ((\sNm tp -> variableProc sNm tp scp) (codeName v ++ st) . listInnerType . convType))
    s) (getPatternInputs lp)

-- | Get entry variable logs.
getEntryVarLogsProc :: (SharedProg r) => LinePattern -> r (Scope r) ->
  GenState [MSStatement r]
getEntryVarLogsProc lp scp = do
  vs <- getEntryVarsProc Nothing lp scp
  logs <- mapM (`maybeLog` local) vs
  return $ concat logs

-- | Converts an 'Expr' to a GOOL Value.
convExprProc :: (SharedProg r) => r (Scope r) -> CodeExpr -> GenState (SValue r)
convExprProc _ (Lit (Dbl d)) = do
  sm <- spaceCodeType Real
  let getLiteral Double = litDouble d
      getLiteral Float = litFloat (realToFrac d)
      getLiteral _ = error "convExprProc: Real space matched to invalid CodeType; should be Double or Float"
  return $ getLiteral sm
convExprProc scp (Lit (ExactDbl d)) = convExprProc scp $ Lit . Dbl $ fromInteger d
convExprProc _ (Lit (Int i))      = return $ litInt i
convExprProc _ (Lit (Str s))      = return $ litString s
convExprProc _ (Lit (Perc a b)) = do
  sm <- spaceCodeType Rational
  let getLiteral Double = litDouble
      getLiteral Float = litFloat . realToFrac
      getLiteral _ = error "convExprProc: Rational space matched to invalid CodeType; should be Double or Float"
  return $ getLiteral sm (fromIntegral a / (10 ** fromIntegral b))
convExprProc scp (AssocA Add l) = foldl1 (#+)  <$> mapM (convExprProc scp) l
convExprProc scp (AssocA Mul l) = foldl1 (#*)  <$> mapM (convExprProc scp) l
convExprProc scp (AssocB And l) = foldl1 (?&&) <$> mapM (convExprProc scp) l
convExprProc scp (AssocB Or l)  = foldl1 (?||) <$> mapM (convExprProc scp) l
convExprProc scp (C c) = do
  g <- get
  let v = quantvar (lookupC g c)
  mkValProc v scp
convExprProc scp (FCall c x ns) = convCallProc c x ns fAppProc
                                    libFuncAppMixedArgs scp
convExprProc _ (New {}) = error "convExprProc: Procedural renderers do not support object creation"
convExprProc _ (Message {}) = error "convExprProc: Procedural renderers do not support methods"
convExprProc _ (Field _ _) = error "convExprProc: Procedural renderers do not support object field access"
convExprProc scp (UnaryOp o u)    = fmap (unop o) (convExprProc scp u)
convExprProc scp (UnaryOpB o u)   = fmap (unopB o) (convExprProc scp u)
convExprProc scp (UnaryOpVV o u)  = fmap (unopVV o) (convExprProc scp u)
convExprProc scp (UnaryOpVN o u)  = fmap (unopVN o) (convExprProc scp u)
convExprProc _ (ArithBinaryOp Frac (Lit (Int a)) (Lit (Int b))) = do -- hack to deal with integer division
  sm <- spaceCodeType Rational
  let getLiteral Double = litDouble (fromIntegral a) #/ litDouble (fromIntegral b)
      getLiteral Float = litFloat (fromIntegral a) #/ litFloat (fromIntegral b)
      getLiteral _ = error "convExprProc: Rational space matched to invalid CodeType; should be Double or Float"
  return $ getLiteral sm
convExprProc scp (ArithBinaryOp o a b) = liftM2 (arithBfunc o)
                                      (convExprProc scp a) (convExprProc scp b)
convExprProc scp (BoolBinaryOp o a b)  = liftM2 (boolBfunc o)
                                      (convExprProc scp a) (convExprProc scp b)
convExprProc scp (LABinaryOp o a b)    = liftM2 (laBfunc o)
                                      (convExprProc scp a) (convExprProc scp b)
convExprProc scp (EqBinaryOp o a b)    = liftM2 (eqBfunc o)
                                      (convExprProc scp a) (convExprProc scp b)
convExprProc scp (OrdBinaryOp o a b)   = liftM2 (ordBfunc o)
                                      (convExprProc scp a) (convExprProc scp b)
convExprProc scp (VVVBinaryOp o a b)   = liftM2 (vecVecVecBfunc o)
                                      (convExprProc scp a) (convExprProc scp b)
convExprProc scp (VVNBinaryOp o a b)   = liftM2 (vecVecNumBfunc o)
                                      (convExprProc scp a) (convExprProc scp b)
convExprProc scp (NVVBinaryOp o a b)   = liftM2 (numVecVecBfunc o)
                                      (convExprProc scp a) (convExprProc scp b)
convExprProc scp (Case c l)            = doit l -- FIXME this is sub-optimal
  where
    doit [] = error "should never happen" -- TODO: change error message?
    doit [(e,_)] = convExprProc scp e -- should always be the else clause
    doit ((e,cond):xs) = liftM3 inlineIf (convExprProc scp cond)
      (convExprProc scp e) (convExprProc scp (Case c xs))
convExprProc scp (Matrix [l]) = do
  ar <- mapM (convExprProc scp) l
                                    -- hd will never fail here
  return $ litArray (fmap valueType (head ar)) ar
convExprProc _ Matrix{} = error "convExprProc: Matrix"
convExprProc _ Operator{} = error "convExprProc: Operator"
convExprProc scp (RealI c ri)  = do
  g <- get
  convExprProc scp $ renderRealInt (lookupC g c) ri

-- | Generates a function call, based on the 'UID' of the chunk representing
-- the function, the list of argument 'Expr's, the list of named argument 'Expr's,
-- the function call generator to use, and the library version of the function
-- call generator (used if the function is in the library export map).
convCallProc :: (SharedProg r) => UID -> [CodeExpr] -> [(UID, CodeExpr)] ->
  (Name -> Name -> VSType r -> [SValue r] -> NamedArgs r ->
  GenState (SValue r)) -> (Name -> Name -> VSType r -> [SValue r]
  -> NamedArgs r -> SValue r) -> r (Scope r) -> GenState (SValue r)
convCallProc c x ns f libf scp = do
  g <- get
  let info = sysinfodb $ codeSpec g
      mem = eMap g
      lem = libEMap g
      funcCd = quantfunc (symbResolve info c)
      funcNm = codeName funcCd
  funcTp <- codeType funcCd
  args <- mapM (convExprProc scp) x
  nms <- mapM ((`mkVarProc` scp) . quantvar . symbResolve info . fst) ns
  nargs <- mapM (convExprProc scp . snd) ns
  maybe (maybe (error $ "Call to non-existent function " ++ funcNm)
      (\m -> return $ libf m funcNm (convType funcTp) args (zip nms nargs))
      (Map.lookup funcNm lem))
    (\m -> f m funcNm (convType funcTp) args (zip nms nargs))
    (Map.lookup funcNm mem)

-- | Converts a 'FuncStmt' to a GOOL Statement.
convStmtProc :: (SharedProg r) => r (Scope r) -> FuncStmt ->
  GenState (MSStatement r)
convStmtProc scp (FAsg v (Matrix [es])) = do
  els <- mapM (convExprProc scp) es
  v' <- mkVarProc v scp
  t <- codeType v
  let listFunc (C.List _) = litList
      listFunc (C.Array _) = litArray
      listFunc _ = error "Type mismatch between variable and value in assignment FuncStmt"
  l <- maybeLog v' local
  return $ multi $ assign v' (listFunc t (listInnerType $ fmap variableType v')
    els) : l
convStmtProc scp (FAsg v e) = do
  e' <- convExprProc scp e
  v' <- mkVarProc v scp
  l <- maybeLog v' local
  return $ multi $ assign v' e' : l
convStmtProc scp (FAsgIndex v i e) = do
  e' <- convExprProc scp e
  v' <- mkVarProc v scp
  t <- codeType v
  let asgFunc (C.List _) = valStmt $ listSet (valueOf v') (litInt i) e'
      asgFunc (C.Array _) = assign (arrayElem i v') e'
      asgFunc _ = error "FAsgIndex used with non-indexed value"
      vi = arrayElem i v'
  l <- maybeLog vi local
  return $ multi $ asgFunc t : l
convStmtProc scp (FFor v start end step st) = do
  stmts <- mapM (convStmtProc scp) st
  vari <- mkVarProc v scp
  start' <- convExprProc scp start
  end' <- convExprProc scp end
  step' <- convExprProc scp step
  return $ forRange vari start' end' step' (bodyStatements stmts)
convStmtProc scp (FForEach v e st) = do
  stmts <- mapM (convStmtProc scp) st
  vari <- mkVarProc v scp
  e' <- convExprProc scp e
  return $ forEach vari e' (bodyStatements stmts)
convStmtProc scp (FWhile e st) = do
  stmts <- mapM (convStmtProc scp) st
  e' <- convExprProc scp e
  return $ while e' (bodyStatements stmts)
convStmtProc scp (FCond e tSt []) = do
  stmts <- mapM (convStmtProc scp) tSt
  e' <- convExprProc scp e
  return $ ifNoElse [(e', bodyStatements stmts)]
convStmtProc scp (FCond e tSt eSt) = do
  stmt1 <- mapM (convStmtProc scp) tSt
  stmt2 <- mapM (convStmtProc scp) eSt
  e' <- convExprProc scp e
  return $ ifCond [(e', bodyStatements stmt1)] (bodyStatements stmt2)
convStmtProc scp (FRet e) = do
  e' <- convExprProc scp e
  return $ returnStmt e'
convStmtProc _ (FThrow s) = return $ throw s
convStmtProc scp (FTry t c) = do
  stmt1 <- mapM (convStmtProc scp) t
  stmt2 <- mapM (convStmtProc scp) c
  return $ tryCatch (bodyStatements stmt1) (bodyStatements stmt2)
convStmtProc _ FContinue = return continue
convStmtProc scp (FDecDef v (Matrix [[]])) = do
  vari <- mkVarProc v scp
  let convDec (C.List _) = listDec 0 vari
      convDec (C.Array _) = arrayDec 0 vari
      convDec _ = varDec vari
  fmap convDec (codeType v)
convStmtProc scp (FDecDef v e) = do
  v' <- mkVarProc v local
  l <- maybeLog v' local
  t <- codeType v
  let convDecDef (Matrix [lst]) = do
        let contDecDef (C.List _) = listDecDef
            contDecDef (C.Array _) = arrayDecDef
            contDecDef _ = error "Type mismatch between variable and value in declare-define FuncStmt"
        e' <- mapM (convExprProc scp) lst
        return $ contDecDef t v' e'
      convDecDef _ = do
        e' <- convExprProc scp e
        return $ varDecDef v' e'
  dd <- convDecDef e
  return $ multi $ dd : l
convStmtProc scp (FFuncDef f ps sts) = do
  f' <- mkVarProc (quantvar f) scp
  pms <- mapM ((`mkVarProc` scp) . quantvar) ps
  b <- mapM (convStmtProc scp) sts
  return $ funcDecDef f' pms (bodyStatements b)
convStmtProc scp (FVal e) = do
  e' <- convExprProc scp e
  return $ valStmt e'
convStmtProc scp (FMulti ss) = do
  stmts <- mapM (convStmtProc scp) ss
  return $ multi stmts
convStmtProc scp (FAppend a b) = do
  a' <- convExprProc scp a
  b' <- convExprProc scp b
  return $ valStmt $ listAppend a' b'

-- | Generates a function that reads a file whose format is based on the passed
-- 'DataDesc'.
genDataFuncProc :: (SharedProg r) => Name -> Description -> DataDesc ->
  GenState (SMethod r)
genDataFuncProc nameTitle desc ddef = do
  let parms = getInputs ddef
  bod <- readDataProc ddef local
  publicFuncProc nameTitle void desc (map pcAuto $ quantvar inFileName : parms)
    Nothing bod

-- | Generates a public function, defined by its inputs and outputs.
publicInOutFuncProc :: (SharedProg r) => Label -> Description -> [CodeVarChunk] ->
  [CodeVarChunk] -> [MSBlock r] -> GenState (SMethod r)
publicInOutFuncProc n = genInOutFuncProc (inOutFunc n public) (docInOutFunc n public) n

-- | Generates a private function, defined by its inputs and outputs.
privateInOutFuncProc :: (SharedProg r) => Label -> Description -> [CodeVarChunk] ->
  [CodeVarChunk] -> [MSBlock r] -> GenState (SMethod r)
privateInOutFuncProc n = genInOutFuncProc (inOutFunc n private) (docInOutFunc n private) n

-- | Generates a function or method defined by its inputs and outputs.
-- Parameters are: the GOOL constructor to use, the equivalent GOOL constructor
-- for a documented function/method, the visibility, permanence, name, description,
-- list of inputs, list of outputs, and body.
genInOutFuncProc :: (SharedProg r) => ([SVariable r] -> [SVariable r] ->
    [SVariable r] -> MSBody r -> SMethod r) ->
  (String -> [(String, SVariable r)] -> [(String, SVariable r)] ->
    [(String, SVariable r)] -> MSBody r -> SMethod r)
  -> Label -> Description -> [CodeVarChunk] -> [CodeVarChunk] ->
  [MSBlock r] -> GenState (SMethod r)
genInOutFuncProc f docf n desc ins' outs' b = do
  g <- get
  let ins = ins' \\ outs'
      outs = outs' \\ ins'
      both = ins' `intersect` outs'
  inVs <- mapM (`mkVarProc` local) ins
  outVs <- mapM (`mkVarProc` local) outs
  bothVs <- mapM (`mkVarProc` local) both
  bod <- logBody n (bothVs ++ inVs) b
  pComms <- mapM getComment ins
  oComms <- mapM getComment outs
  bComms <- mapM getComment both
  return $ if CommentFunc `elem` commented g
    then docf desc (zip pComms inVs) (zip oComms outVs) (zip
    bComms bothVs) bod else f inVs outVs bothVs bod

-- Used for readData and readDataProc
l_line, l_lines, l_linetokens, l_infile, l_i :: Label
var_line, var_lines, var_linetokens, var_infile ::
  (SharedProg r) => r (Scope r) -> SVariable r
var_i :: (SharedProg r) => SVariable r
v_line, v_lines, v_linetokens, v_infile ::
  (SharedProg r) => r (Scope r) -> SValue r
v_i :: (SharedProg r) => SValue r
l_line = "line"
var_line = var l_line string
v_line scp = valueOf $ var_line scp
l_lines = "lines"
var_lines = var l_lines (listType string)
v_lines scp = valueOf $ var_lines scp
l_linetokens = "linetokens"
var_linetokens = var l_linetokens (listType string)
v_linetokens scp = valueOf $ var_linetokens scp
l_infile = "infile"
var_infile = var l_infile infile
v_infile scp = valueOf $ var_infile scp
l_i = "i"
var_i = var l_i int local
v_i = valueOf var_i