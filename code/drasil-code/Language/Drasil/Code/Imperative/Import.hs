{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE Rank2Types #-}
module Language.Drasil.Code.Imperative.Import (
  publicMethod, publicInOutFunc, mkVar, mkVal, convExpr, genCalcBlock, 
  CalcType(..), genModDef, readData, renderC
) where

import Language.Drasil hiding (int, log, ln, exp,
  sin, cos, tan, csc, sec, cot, arcsin, arccos, arctan)
import Database.Drasil (symbResolve)
import Language.Drasil.Code.Code as C (CodeType(List))
import Language.Drasil.Code.Imperative.Comments (paramComment, returnComment)
import Language.Drasil.Code.Imperative.GenerateGOOL (fApp, genModule, mkParam)
import Language.Drasil.Code.Imperative.Helpers (getUpperBound, liftS, lookupC)
import Language.Drasil.Code.Imperative.Logging (maybeLog, loggedMethod)
import Language.Drasil.Code.Imperative.Parameters (getCalcParams)
import Language.Drasil.Code.Imperative.State (State(..))
import Language.Drasil.Code.Imperative.GOOL.Symantics (Label, RenderSym(..), 
  PermanenceSym(..), BodySym(..), BlockSym(..), StateTypeSym(..), 
  VariableSym(..), ValueSym(..), NumericExpression(..), BooleanExpression(..), 
  ValueExpression(..), FunctionSym(..), SelectorFunction(..), StatementSym(..), 
  ControlStatementSym(..), ScopeSym(..), MethodTypeSym(..), MethodSym(..))
import Language.Drasil.Code.Imperative.GOOL.Helpers (convType)
import Language.Drasil.Chunk.Code (CodeIdea(codeName), codeType, codevar, 
  quantvar, quantfunc)
import Language.Drasil.Chunk.CodeDefinition (CodeDefinition, codeEquat)
import Language.Drasil.Chunk.CodeQuantity (HasCodeType)
import Language.Drasil.Code.CodeQuantityDicts (inFileName, inParams)
import Language.Drasil.CodeSpec hiding (codeSpec, Mod(..))
import qualified Language.Drasil.CodeSpec as CS (Mod(..))
import Language.Drasil.Code.DataDesc (DataItem, LinePattern(Repeat, Straight), 
  Data(Line, Lines, JunkData, Singleton), DataDesc, isLine, isLines, getInputs,
  getPatternInputs)

import Prelude hiding (sin, cos, tan, log, exp)
import Data.List ((\\))
import qualified Data.Map as Map (lookup)
import Data.Maybe (maybe)
import Control.Applicative ((<$>))
import Control.Monad (liftM2,liftM3)
import Control.Monad.Reader (Reader, ask)
import Control.Lens ((^.))

value :: (RenderSym repr) => UID -> String -> repr (StateType repr) -> 
  Reader State (repr (Value repr))
value u s t = do
  g <- ask
  let cs = codeSpec g
      mm = constMap cs
  maybe (do { v <- variable s t; return $ valueOf v }) 
    (convExpr . codeEquat) (Map.lookup u mm)

variable :: (RenderSym repr) => String -> repr (StateType repr) -> 
  Reader State (repr (Variable repr))
variable s t = do
  g <- ask
  let cs = csi $ codeSpec g
  if s `elem` map codeName (inputs cs) 
    then inputVariable (inStruct g) s t
    else return $ var s t
  
inputVariable :: (RenderSym repr) => Structure -> String -> 
  repr (StateType repr) -> Reader State (repr (Variable repr))
inputVariable Unbundled s t = return $ var s t
inputVariable Bundled s t = do
  ip <- mkVar (codevar inParams)
  return $ ip $-> var s t

mkVal :: (RenderSym repr, HasUID c, HasCodeType c, CodeIdea c) => c -> 
  Reader State (repr (Value repr))
mkVal v = value (v ^. uid) (codeName v) (convType $ codeType v)

mkVar :: (RenderSym repr, HasCodeType c, CodeIdea c) => c -> 
  Reader State (repr (Variable repr))
mkVar v = variable (codeName v) (convType $ codeType v)

publicMethod :: (RenderSym repr, HasUID c, HasCodeType c, CodeIdea c) => 
  repr (MethodType repr) -> Label -> String -> [c] -> Maybe String -> 
  [repr (Block repr)] -> Reader State (repr (Method repr))
publicMethod = genMethod public static_

publicInOutFunc :: (RenderSym repr, HasUID c, HasCodeType c, CodeIdea c) => 
  Label -> String -> [c] -> [c] -> [c] -> [repr (Block repr)] -> 
  Reader State (repr (Method repr))
publicInOutFunc = genInOutFunc public static_

genMethod :: (RenderSym repr, HasUID c, HasCodeType c, CodeIdea c) => 
  repr (Scope repr) -> repr (Permanence repr) -> repr (MethodType repr) -> 
  Label -> String -> [c] -> Maybe String -> [repr (Block repr)] -> 
  Reader State (repr (Method repr))
genMethod s pr t n desc p r b = do
  g <- ask
  vars <- mapM mkVar p
  let ps = map mkParam vars
      doLog = logKind g
      loggedBody LogFunc = loggedMethod (logName g) n vars b
      loggedBody LogAll  = loggedMethod (logName g) n vars b
      loggedBody _       = b
      bod = body $ loggedBody doLog
      fn = function n s pr t ps bod
  pComms <- mapM (paramComment . (^. uid)) p
  return $ if CommentFunc `elem` commented g
    then docFunc desc pComms r fn else fn

genInOutFunc :: (RenderSym repr, HasUID c, HasCodeType c, CodeIdea c) => 
  repr (Scope repr) -> repr (Permanence repr) -> Label -> String -> [c] ->
  [c] -> [c] -> [repr (Block repr)] -> Reader State (repr (Method repr))

genInOutFunc s pr n desc ins outs both b = do
  g <- ask
  inVs <- mapM mkVar ins
  outVs <- mapM mkVar outs
  bothVs <- mapM mkVar both
  let doLog = logKind g
      loggedBody LogFunc = loggedMethod (logName g) n inVs b
      loggedBody LogAll  = loggedMethod (logName g) n inVs b
      loggedBody _       = b
      bod = body $ loggedBody doLog
      fn = inOutFunc n s pr inVs outVs bothVs bod
  pComms <- mapM (paramComment . (^. uid)) ins
  oComms <- mapM (paramComment . (^. uid)) outs
  bComms <- mapM (paramComment . (^. uid)) both
  return $ if CommentFunc `elem` commented g 
    then docInOutFunc desc pComms oComms bComms fn else fn

convExpr :: (RenderSym repr) => Expr -> Reader State (repr (Value repr))
convExpr (Dbl d) = return $ litFloat d
convExpr (Int i) = return $ litInt i
convExpr (Str s) = return $ litString s
convExpr (Perc a b) = return $ litFloat $ fromIntegral a / (10 ** fromIntegral b)
convExpr (AssocA Add l) = foldl1 (#+)  <$> mapM convExpr l
convExpr (AssocA Mul l) = foldl1 (#*)  <$> mapM convExpr l
convExpr (AssocB And l) = foldl1 (?&&) <$> mapM convExpr l
convExpr (AssocB Or l)  = foldl1 (?||) <$> mapM convExpr l
convExpr Deriv{} = return $ litString "**convExpr :: Deriv unimplemented**"
convExpr (C c)   = do
  g <- ask
  let v = quantvar (lookupC g c)
  mkVal v
convExpr (FCall (C c) x) = do
  g <- ask
  let info = sysinfodb $ csi $ codeSpec g
      mem = eMap $ codeSpec g
      funcCd = quantfunc (symbResolve info c)
      funcNm = codeName funcCd
      funcTp = convType $ codeType funcCd
  args <- mapM convExpr x
  maybe (error $ "Call to non-existent function" ++ funcNm) 
    (\f -> fApp f funcNm funcTp args) (Map.lookup funcNm mem)
convExpr FCall{}   = return $ litString "**convExpr :: FCall unimplemented**"
convExpr (UnaryOp o u) = fmap (unop o) (convExpr u)
convExpr (BinaryOp Frac (Int a) (Int b)) =
  return $ litFloat (fromIntegral a) #/ litFloat (fromIntegral b) -- hack to deal with integer division
convExpr (BinaryOp o a b)  = liftM2 (bfunc o) (convExpr a) (convExpr b)
convExpr (Case c l)      = doit l -- FIXME this is sub-optimal
  where
    doit [] = error "should never happen"
    doit [(e,_)] = convExpr e -- should always be the else clause
    doit ((e,cond):xs) = liftM3 inlineIf (convExpr cond) (convExpr e) 
      (convExpr (Case c xs))
convExpr Matrix{}    = error "convExpr: Matrix"
convExpr Operator{} = error "convExpr: Operator"
convExpr IsIn{}    = error "convExpr: IsIn"
convExpr (RealI c ri)  = do
  g <- ask
  convExpr $ renderRealInt (lookupC g c) ri

renderC :: (HasUID c, HasSymbol c) => c -> Constraint -> Expr
renderC s (Range _ rr)          = renderRealInt s rr
renderC s (EnumeratedReal _ rr) = IsIn (sy s) (DiscreteD rr)
renderC s (EnumeratedStr _ rr)  = IsIn (sy s) (DiscreteS rr)

renderRealInt :: (HasUID c, HasSymbol c) => c -> RealInterval Expr Expr -> Expr
renderRealInt s (Bounded (Inc,a) (Inc,b)) = (a $<= sy s) $&& (sy s $<= b)
renderRealInt s (Bounded (Inc,a) (Exc,b)) = (a $<= sy s) $&& (sy s $<  b)
renderRealInt s (Bounded (Exc,a) (Inc,b)) = (a $<  sy s) $&& (sy s $<= b)
renderRealInt s (Bounded (Exc,a) (Exc,b)) = (a $<  sy s) $&& (sy s $<  b)
renderRealInt s (UpTo (Inc,a))    = sy s $<= a
renderRealInt s (UpTo (Exc,a))    = sy s $< a
renderRealInt s (UpFrom (Inc,a))  = sy s $>= a
renderRealInt s (UpFrom (Exc,a))  = sy s $>  a

unop :: (RenderSym repr) => UFunc -> (repr (Value repr) -> repr (Value repr))
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
unop Dim  = listSize
unop Norm = error "unop: Norm not implemented"
unop Not  = (?!)
unop Neg  = (#~)

bfunc :: (RenderSym repr) => BinOp -> (repr (Value repr) -> repr
  (Value repr) -> repr (Value repr))
bfunc Eq    = (?==)
bfunc NEq   = (?!=)
bfunc Gt    = (?>)
bfunc Lt    = (?<)
bfunc LEq   = (?<=)
bfunc GEq   = (?>=)
bfunc Cross = error "bfunc: Cross not implemented"
bfunc Pow   = (#^)
bfunc Subt  = (#-)
bfunc Impl  = error "convExpr :=>"
bfunc Iff   = error "convExpr :<=>"
bfunc Dot   = error "convExpr DotProduct"
bfunc Frac  = (#/)
bfunc Index = listAccess

------- CALC ----------

genCalcFunc :: (RenderSym repr) => CodeDefinition -> Reader State (repr
  (Method repr))
genCalcFunc cdef = do
  parms <- getCalcParams cdef
  let nm = codeName cdef
      tp = convType $ codeType cdef
  blck <- genCalcBlock CalcReturn cdef (codeEquat cdef)
  desc <- returnComment $ cdef ^. uid
  publicMethod
    (mState tp)
    nm
    ("Calculates " ++ desc)
    parms
    (Just desc)
    [blck]

data CalcType = CalcAssign | CalcReturn deriving Eq

genCalcBlock :: (RenderSym repr) => CalcType -> CodeDefinition -> Expr ->
  Reader State (repr (Block repr))
genCalcBlock t v (Case c e) = genCaseBlock t v c e
genCalcBlock t v e
    | t == CalcAssign  = fmap block $ liftS $ do { vv <- mkVar v; ee <-
      convExpr e; l <- maybeLog vv; return $ multi $ assign vv ee : l}
    | otherwise        = block <$> liftS (returnState <$> convExpr e)

genCaseBlock :: (RenderSym repr) => CalcType -> CodeDefinition -> Completeness 
  -> [(Expr,Relation)] -> Reader State (repr (Block repr))
genCaseBlock _ _ _ [] = error $ "Case expression with no cases encountered" ++
  " in code generator"
genCaseBlock t v c cs = do
  ifs <- mapM (\(e,r) -> liftM2 (,) (convExpr r) (calcBody e)) (ifEs c)
  els <- elseE c
  return $ block [ifCond ifs els]
  where calcBody e = fmap body $ liftS $ genCalcBlock t v e
        ifEs Complete = init cs
        ifEs Incomplete = cs
        elseE Complete = calcBody $ fst $ last cs
        elseE Incomplete = return $ oneLiner $ throw $  
          "Undefined case encountered in function " ++ codeName v

-- medium hacks --
genModDef :: (RenderSym repr) => CS.Mod -> Reader State (repr (RenderFile repr))
genModDef (CS.Mod n desc fs) = genModule n desc (Just $ mapM genFunc fs) Nothing

genFunc :: (RenderSym repr) => Func -> Reader State (repr (Method repr))
genFunc (FDef (FuncDef n desc parms o rd s)) = do
  g <- ask
  stmts <- mapM convStmt s
  vars <- mapM mkVar (fstdecl (sysinfodb $ csi $ codeSpec g) s \\ parms)
  publicMethod (mState $ convType o) n desc parms rd [block $ map varDec 
    vars ++ stmts]
genFunc (FData (FuncData n desc ddef)) = genDataFunc n desc ddef
genFunc (FCD cd) = genCalcFunc cd

convStmt :: (RenderSym repr) => FuncStmt -> Reader State (repr (Statement repr))
convStmt (FAsg v e) = do
  e' <- convExpr e
  v' <- mkVar v
  l <- maybeLog v'
  return $ multi $ assign v' e' : l
convStmt (FFor v e st) = do
  stmts <- mapM convStmt st
  e' <- convExpr $ getUpperBound e
  return $ forRange (codeName v) (litInt 0) e' (litInt 1) (bodyStatements stmts)
convStmt (FWhile e st) = do
  stmts <- mapM convStmt st
  e' <- convExpr e
  return $ while e' (bodyStatements stmts)
convStmt (FCond e tSt []) = do
  stmts <- mapM convStmt tSt
  e' <- convExpr e
  return $ ifNoElse [(e', bodyStatements stmts)]
convStmt (FCond e tSt eSt) = do
  stmt1 <- mapM convStmt tSt
  stmt2 <- mapM convStmt eSt
  e' <- convExpr e
  return $ ifCond [(e', bodyStatements stmt1)] (bodyStatements stmt2)
convStmt (FRet e) = do
  e' <- convExpr e
  return $ returnState e'
convStmt (FThrow s) = return $ throw s
convStmt (FTry t c) = do
  stmt1 <- mapM convStmt t
  stmt2 <- mapM convStmt c
  return $ tryCatch (bodyStatements stmt1) (bodyStatements stmt2)
convStmt FContinue = return continue
convStmt (FDec v) = do
  vari <- mkVar v
  let convDec (C.List _) = listDec 0 vari
      convDec _ = varDec vari
  return $ convDec (codeType v) 
convStmt (FProcCall n l) = do
  e' <- convExpr (FCall (asExpr n) l)
  return $ valState e'
convStmt (FAppend a b) = do
  a' <- convExpr a
  b' <- convExpr b
  return $ valState $ listAppend a' b'

genDataFunc :: (RenderSym repr) => Name -> String -> DataDesc -> 
  Reader State (repr (Method repr))
genDataFunc nameTitle desc ddef = do
  let parms = getInputs ddef
  bod <- readData ddef
  publicMethod (mState void) nameTitle desc (codevar inFileName : parms) 
    Nothing bod

-- this is really ugly!!
readData :: (RenderSym repr) => DataDesc -> Reader State
  [repr (Block repr)]
readData ddef = do
  inD <- mapM inData ddef
  v_filename <- mkVal $ codevar inFileName
  return [block $ 
    varDec var_infile :
    (if any (\d -> isLine d || isLines d) ddef then [varDec var_line, listDec 0 var_linetokens] else []) ++
    [listDec 0 var_lines | any isLines ddef] ++
    openFileR var_infile v_filename :
    concat inD ++ [
    closeFile v_infile ]]
  where inData :: (RenderSym repr) => Data -> Reader State [repr (Statement repr)]
        inData (Singleton v) = do
            vv <- mkVar v
            l <- maybeLog vv
            return [multi $ getFileInput v_infile vv : l]
        inData JunkData = return [discardFileLine v_infile]
        inData (Line lp d) = do
          lnI <- lineData Nothing lp
          logs <- getEntryVarLogs lp
          return $ [getFileInputLine v_infile var_line, 
            stringSplit d var_linetokens v_line] ++ lnI ++ logs
        inData (Lines lp ls d) = do
          lnV <- lineData (Just "_temp") lp
          logs <- getEntryVarLogs lp
          let readLines Nothing = [getFileInputAll v_infile var_lines,
                forRange l_i (litInt 0) (listSize v_lines) (litInt 1)
                  (bodyStatements $ stringSplit d var_linetokens (
                  listAccess v_lines v_i) : lnV)]
              readLines (Just numLines) = [forRange l_i (litInt 0) 
                (litInt numLines) (litInt 1)
                (bodyStatements $
                  [getFileInputLine v_infile var_line,
                   stringSplit d var_linetokens v_line
                  ] ++ lnV)]
          return $ readLines ls ++ logs
        ---------------
        lineData :: (RenderSym repr) => Maybe String -> LinePattern -> 
          Reader State [repr (Statement repr)]
        lineData s p@(Straight _) = do
          vs <- getEntryVars s p
          return [stringListVals vs v_linetokens]
        lineData s p@(Repeat ds) = do
          vs <- getEntryVars s p
          return $ clearTemps s ds ++ stringListLists vs v_linetokens 
            : appendTemps s ds
        ---------------
        clearTemps :: (RenderSym repr) => Maybe String -> [DataItem] -> 
          [repr (Statement repr)]
        clearTemps Nothing _ = []
        clearTemps (Just sfx) es = map (clearTemp sfx) es
        ---------------
        clearTemp :: (RenderSym repr) => String -> DataItem -> 
          repr (Statement repr)
        clearTemp sfx v = listDecDef (var (codeName v ++ sfx) 
          (listInnerType $ convType $ codeType v)) []
        ---------------
        appendTemps :: (RenderSym repr) => Maybe String -> [DataItem] -> 
          [repr (Statement repr)]
        appendTemps Nothing _ = []
        appendTemps (Just sfx) es = map (appendTemp sfx) es
        ---------------
        appendTemp :: (RenderSym repr) => String -> DataItem -> 
          repr (Statement repr)
        appendTemp sfx v = valState $ listAppend 
          (valueOf $ var (codeName v) (convType $ codeType v)) 
          (valueOf $ var (codeName v ++ sfx) (convType $ codeType v))
        ---------------
        l_line, l_lines, l_linetokens, l_infile, l_i :: Label
        var_line, var_lines, var_linetokens, var_infile :: 
          (RenderSym repr) => repr (Variable repr)
        v_line, v_lines, v_linetokens, v_infile, v_i ::
          (RenderSym repr) => repr (Value repr)
        l_line = "line"
        var_line = var l_line string
        v_line = valueOf var_line
        l_lines = "lines"
        var_lines = var l_lines (listType static_ string)
        v_lines = valueOf var_lines
        l_linetokens = "linetokens"
        var_linetokens = var l_linetokens (listType static_ string)
        v_linetokens = valueOf var_linetokens
        l_infile = "infile"
        var_infile = var l_infile infile
        v_infile = valueOf var_infile
        l_i = "i"
        v_i = valueOf $ var l_i int

getEntryVars :: (RenderSym repr) => Maybe String -> LinePattern -> 
  Reader State [repr (Variable repr)]
getEntryVars s lp = mapM (maybe mkVar (\st v -> variable (codeName v ++ st) 
  (listInnerType $ convType $ codeType v)) s) (getPatternInputs lp)

getEntryVarLogs :: (RenderSym repr) => LinePattern -> 
  Reader State [repr (Statement repr)]
getEntryVarLogs lp = do
  vs <- getEntryVars Nothing lp
  logs <- mapM maybeLog vs
  return $ concat logs
