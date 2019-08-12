{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE Rank2Types #-}
module Language.Drasil.Code.Imperative.Import (generator, generateCode) where

import Utils.Drasil (stringList)

import Language.Drasil hiding (int, ($.), log, ln, exp,
  sin, cos, tan, csc, sec, cot, arcsin, arccos, arctan)
import Database.Drasil (ChunkDB, symbResolve)
import Language.Drasil.Code.Code as C (CodeType(List, Object))
import Language.Drasil.Code.Imperative.Symantics (Label, PackageSym(..), 
  ProgramSym(..), RenderSym(..), AuxiliarySym(..), PermanenceSym(..), 
  BodySym(..), BlockSym(..), StateTypeSym(..), VariableSym(..), ValueSym(..),
  NumericExpression(..), BooleanExpression(..), ValueExpression(..), 
  FunctionSym(..), SelectorFunction(..), StatementSym(..), 
  ControlStatementSym(..), ScopeSym(..), MethodTypeSym(..), ParameterSym(..),
  MethodSym(..), StateVarSym(..), ClassSym(..), ModuleSym(..))
import Language.Drasil.Code.Imperative.Data (PackData(..), ProgData(..))
import Language.Drasil.Code.Imperative.Helpers (convType)
import Language.Drasil.Code.CodeGeneration (createCodeFiles, makeCode)
import Language.Drasil.Chunk.Code (CodeChunk, CodeIdea(codeName, codeChunk),
  codeType, codevar, quantvar, quantfunc, physLookup, sfwrLookup, programName)
import Language.Drasil.Chunk.CodeDefinition (CodeDefinition, codeEquat)
import Language.Drasil.Chunk.CodeQuantity (HasCodeType)
import Language.Drasil.Code.CodeQuantityDicts (inFileName, inParams)
import Language.Drasil.CodeSpec hiding (codeSpec, Mod(..))
import qualified Language.Drasil.CodeSpec as CS (Mod(..))
import Language.Drasil.Code.DataDesc (DataItem, LinePattern(Repeat, Straight), 
  Data(Line, Lines, JunkData, Singleton), DataDesc, isLine, isLines, getInputs,
  getPatternInputs, junkLine, singleton)
import Language.Drasil.Printers (Linearity(Linear), exprDoc, sentenceDoc, 
  unitDoc)

import Prelude hiding (sin, cos, tan, log, exp, const, print)
import Data.List (nub, intersperse, intercalate, (\\))
import System.Directory (setCurrentDirectory, createDirectoryIfMissing, getCurrentDirectory)
import Data.Map (Map, member)
import qualified Data.Map as Map (lookup, elems)
import Data.Maybe (fromMaybe, maybe, maybeToList, catMaybes, mapMaybe)
import Control.Applicative ((<$>))
import Control.Monad (liftM2,liftM3)
import Control.Monad.Reader (Reader, ask, runReader, withReader)
import Control.Lens ((^.), view)
import Text.PrettyPrint.HughesPJ (Doc, (<+>), empty, parens, render, text)

liftS :: Reader a b -> Reader a [b]
liftS = fmap (: [])

-- helpers

mkParam :: (RenderSym repr) => repr (Variable repr) -> repr (Parameter repr)
mkParam v = paramFunc (getType $ variableType v) v
  where paramFunc (C.List _) = pointerParam
        paramFunc (C.Object _) = pointerParam
        paramFunc _ = stateParam

mkVal :: (RenderSym repr, HasUID c, HasCodeType c, CodeIdea c) => c -> 
  Reader State (repr (Value repr))
mkVal v = value (v ^. uid) (codeName v) (convType $ codeType v)

mkVar :: (RenderSym repr, HasCodeType c, CodeIdea c) => c -> 
  Reader State (repr (Variable repr))
mkVar v = variable (codeName v) (convType $ codeType v)

getInputVars :: Structure -> [CodeChunk] -> [CodeChunk]
getInputVars _ [] = []
getInputVars Unbundled cs = cs
getInputVars Bundled _ = [codevar inParams]

-- Right now, we always inline constants. In the future, this will be captured by a choice and this function should be updated to read that choice
getConstVars :: [CodeChunk] -> [CodeChunk]
getConstVars _ = []

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

getUpperBound :: Expr -> Expr
getUpperBound (BinaryOp Lt _ b) = b
getUpperBound _ = error "Attempt to get upper bound of invalid expression"

lookupC :: State -> UID -> QuantityDict
lookupC g = symbResolve (sysinfodb $ csi $ codeSpec g)

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
