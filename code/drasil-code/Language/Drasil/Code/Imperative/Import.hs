{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE Rank2Types #-}
module Language.Drasil.Code.Imperative.Import (codeType, publicFunc, 
  privateMethod, publicInOutFunc, privateInOutMethod, genConstructor, mkVar, 
  mkVal, convExpr, genModDef, genModFuncs, readData, renderC
) where

import Language.Drasil hiding (int, log, ln, exp,
  sin, cos, tan, csc, sec, cot, arcsin, arccos, arctan)
import Database.Drasil (symbResolve)
import Language.Drasil.Code.Imperative.Comments (getComment)
import Language.Drasil.Code.Imperative.ConceptMatch (conceptToGOOL)
import Language.Drasil.Code.Imperative.GenerateGOOL (auxClass, fApp, ctorCall,
  genModuleWithImports, mkParam, primaryClass)
import Language.Drasil.Code.Imperative.Helpers (getUpperBound, lookupC)
import Language.Drasil.Code.Imperative.Logging (maybeLog, logBody)
import Language.Drasil.Code.Imperative.DrasilState (DrasilState(..))
import Language.Drasil.Chunk.Code (CodeIdea(codeName), quantvar, quantfunc)
import Language.Drasil.Chunk.CodeDefinition (codeEquat)
import Language.Drasil.Code.CodeQuantityDicts (inFileName, inParams, consts)
import Language.Drasil.CodeSpec (CodeSpec(..), CodeSystInfo(..), Comments(..),
  ConstantRepr(..), ConstantStructure(..), Structure(..))
import Language.Drasil.Code.DataDesc (DataItem, LinePattern(Repeat, Straight), 
  Data(Line, Lines, JunkData, Singleton), DataDesc, isLine, isLines, getInputs,
  getPatternInputs)
import Language.Drasil.Mod (Func(..), FuncData(..), FuncDef(..), FuncStmt(..), 
  Mod(..), Name, fstdecl)
import qualified Language.Drasil.Mod as M (Class(..))

import GOOL.Drasil (Label, ProgramSym, FileSym(..), PermanenceSym(..), 
  BodySym(..), BlockSym(..), TypeSym(..), VariableSym(..), ValueSym(..), 
  NumericExpression(..), BooleanExpression(..), ValueExpression(..), 
  objMethodCallMixedArgs, FunctionSym(..), SelectorFunction(..), 
  StatementSym(..), ControlStatementSym(..), ScopeSym(..), ParameterSym(..), 
  MethodSym(..), StateVarSym(..), ClassSym(..), nonInitConstructor, convType, 
  CodeType(..), FS, CS, MS, VS, onStateValue) 
import qualified GOOL.Drasil as C (CodeType(List, Array))

import Prelude hiding (sin, cos, tan, log, exp)
import Data.List ((\\), intersect)
import qualified Data.Map as Map (lookup)
import Data.Maybe (maybe)
import Control.Applicative ((<$>))
import Control.Monad (liftM2,liftM3)
import Control.Monad.Reader (Reader, ask)
import Control.Lens ((^.))

codeType :: (HasSpace c) => c -> Reader DrasilState CodeType
codeType c = do
  g <- ask
  return $ spaceMatches g (c ^. typ)

value :: (ProgramSym repr) => UID -> String -> VS (repr (Type repr)) -> 
  Reader DrasilState (VS (repr (Value repr)))
value u s t = do
  g <- ask
  let cs = codeSpec g
      mm = constMap cs
      cm = concMatches g
      maybeInline Inline m = Just m
      maybeInline _ _ = Nothing
  maybe (maybe (do { v <- variable s t; return $ valueOf v }) 
    (convExpr . codeEquat) (Map.lookup u mm >>= maybeInline (conStruct g))) 
    (return . conceptToGOOL) (Map.lookup u cm)

variable :: (ProgramSym repr) => String -> VS (repr (Type repr)) -> 
  Reader DrasilState (VS (repr (Variable repr)))
variable s t = do
  g <- ask
  let cs = csi $ codeSpec g
      defFunc Var = var
      defFunc Const = staticVar
  if s `elem` map codeName (inputs cs) 
    then inputVariable (inStruct g) Var (var s t)
    else if s `elem` map codeName (constants $ csi $ codeSpec g)
      then constVariable (conStruct g) (conRepr g) ((defFunc $ conRepr g) s t)
      else return $ var s t
  
inputVariable :: (ProgramSym repr) => Structure -> ConstantRepr -> 
  VS (repr (Variable repr)) -> Reader DrasilState (VS (repr (Variable repr)))
inputVariable Unbundled _ v = return v
inputVariable Bundled Var v = do
  g <- ask
  let inClsName = "InputParameters"
  ip <- mkVar (quantvar inParams)
  return $ if currentClass g == inClsName then objVarSelf v else ip $-> v
inputVariable Bundled Const v = do
  ip <- mkVar (quantvar inParams)
  classVariable ip v

constVariable :: (ProgramSym repr) => ConstantStructure -> ConstantRepr -> 
  VS (repr (Variable repr)) -> Reader DrasilState (VS (repr (Variable repr)))
constVariable (Store Bundled) Var v = do
  cs <- mkVar (quantvar consts)
  return $ cs $-> v
constVariable (Store Bundled) Const v = do
  cs <- mkVar (quantvar consts)
  classVariable cs v
constVariable WithInputs cr v = do
  g <- ask
  inputVariable (inStruct g) cr v
constVariable _ _ v = return v

classVariable :: (ProgramSym repr) => VS (repr (Variable repr)) -> 
  VS (repr (Variable repr)) -> Reader DrasilState (VS (repr (Variable repr)))
classVariable c v = do
  g <- ask
  let checkCurrent m = if currentModule g == m then classVar else extClassVar
  return $ v >>= (\v' -> maybe (error $ "Variable " ++ variableName v' ++ 
    " missing from export map") checkCurrent (Map.lookup (variableName v') 
    (eMap $ codeSpec g)) (onStateValue variableType c) v)

mkVal :: (ProgramSym repr, HasUID c, HasSpace c, CodeIdea c) => c -> 
  Reader DrasilState (VS (repr (Value repr)))
mkVal v = do
  t <- codeType v
  value (v ^. uid) (codeName v) (convType t)

mkVar :: (ProgramSym repr, HasSpace c, CodeIdea c) => c -> 
  Reader DrasilState (VS (repr (Variable repr)))
mkVar v = do
  t <- codeType v
  variable (codeName v) (convType t)

publicFunc :: (ProgramSym repr, HasSpace c, CodeIdea c) => 
  Label -> VS (repr (Type repr)) -> String -> [c] -> Maybe String -> 
  [MS (repr (Block repr))] -> Reader DrasilState (MS (repr (Method repr)))
publicFunc n t = genMethod (function n public static t) n

privateMethod :: (ProgramSym repr, HasSpace c, CodeIdea c) => 
  Label -> VS (repr (Type repr)) -> String -> [c] -> Maybe String -> 
  [MS (repr (Block repr))] -> Reader DrasilState (MS (repr (Method repr)))
privateMethod n t = genMethod (method n private dynamic t) n

publicInOutFunc :: (ProgramSym repr, HasSpace c, CodeIdea c, Eq c) 
  => Label -> String -> [c] -> [c] -> [MS (repr (Block repr))] -> 
  Reader DrasilState (MS (repr (Method repr)))
publicInOutFunc n = genInOutFunc (inOutFunc n) (docInOutFunc n) public static n

privateInOutMethod :: (ProgramSym repr, HasSpace c, CodeIdea c,
  Eq c) => Label -> String -> [c] -> [c] -> [MS (repr (Block repr))] 
  -> Reader DrasilState (MS (repr (Method repr)))
privateInOutMethod n = genInOutFunc (inOutMethod n) (docInOutMethod n) 
  private dynamic n

genConstructor :: (ProgramSym repr, HasSpace c, CodeIdea c) => 
  Label -> String -> [c] -> [MS (repr (Block repr))] -> 
  Reader DrasilState (MS (repr (Method repr)))
genConstructor n desc p = genMethod nonInitConstructor n desc p Nothing

genInitConstructor :: (ProgramSym repr, HasSpace c, CodeIdea c) => Label -> 
  String -> [c] -> [(VS (repr (Variable repr)), VS (repr (Value repr)))] -> 
  [MS (repr (Block repr))] -> Reader DrasilState (MS (repr (Method repr)))
genInitConstructor n desc p is = genMethod (`constructor` is) n desc p 
  Nothing

genMethod :: (ProgramSym repr, HasSpace c, CodeIdea c) => 
  ([MS (repr (Parameter repr))] -> MS (repr (Body repr)) -> 
  MS (repr (Method repr))) -> Label -> String -> [c] -> Maybe String -> 
  [MS (repr (Block repr))] -> Reader DrasilState (MS (repr (Method repr)))
genMethod f n desc p r b = do
  g <- ask
  vars <- mapM mkVar p
  bod <- logBody n vars b
  let ps = map mkParam vars
      fn = f ps bod
  pComms <- mapM getComment p
  return $ if CommentFunc `elem` commented g
    then docFunc desc pComms r fn else fn

genInOutFunc :: (ProgramSym repr, HasSpace c, CodeIdea c, Eq c) => 
  (repr (Scope repr) -> repr (Permanence repr) -> [VS (repr (Variable repr))] 
    -> [VS (repr (Variable repr))] -> [VS (repr (Variable repr))] -> 
    MS (repr (Body repr)) -> MS (repr (Method repr))) -> 
  (repr (Scope repr) -> repr (Permanence repr) -> String -> 
    [(String, VS (repr (Variable repr)))] -> 
    [(String, VS (repr (Variable repr)))] -> 
    [(String, VS (repr (Variable repr)))] -> MS (repr (Body repr)) -> 
    MS (repr (Method repr))) -> 
  repr (Scope repr) -> repr (Permanence repr) -> Label -> String -> [c] -> 
  [c] -> [MS (repr (Block repr))] -> 
  Reader DrasilState (MS (repr (Method repr)))
genInOutFunc f docf s pr n desc ins' outs' b = do
  g <- ask
  let ins = ins' \\ outs'
      outs = outs' \\ ins'
      both = ins' `intersect` outs'
  inVs <- mapM mkVar ins
  outVs <- mapM mkVar outs
  bothVs <- mapM mkVar both
  bod <- logBody n (bothVs ++ inVs) b
  pComms <- mapM getComment ins
  oComms <- mapM getComment outs
  bComms <- mapM getComment both
  return $ if CommentFunc `elem` commented g 
    then docf s pr desc (zip pComms inVs) (zip oComms outVs) (zip 
    bComms bothVs) bod else f s pr inVs outVs bothVs bod

convExpr :: (ProgramSym repr) => Expr -> Reader DrasilState (VS (repr (Value repr)))
convExpr (Dbl d) = do
  g <- ask
  let sm = spaceMatches g
      getLiteral Double = litDouble d
      getLiteral Float = litFloat (realToFrac d)
      getLiteral _ = error "convExpr: Real space matched to invalid CodeType; should be Double or Float"
  return $ getLiteral (sm Real)
convExpr (Int i) = return $ litInt i
convExpr (Str s) = return $ litString s
convExpr (Perc a b) = do
  g <- ask
  let sm = spaceMatches g
      getLiteral Double = litDouble
      getLiteral Float = litFloat . realToFrac
      getLiteral _ = error "convExpr: Rational space matched to invalid CodeType; should be Double or Float"
  return $ getLiteral (sm Rational) (fromIntegral a / (10 ** fromIntegral b))
convExpr (AssocA Add l) = foldl1 (#+)  <$> mapM convExpr l
convExpr (AssocA Mul l) = foldl1 (#*)  <$> mapM convExpr l
convExpr (AssocB And l) = foldl1 (?&&) <$> mapM convExpr l
convExpr (AssocB Or l)  = foldl1 (?||) <$> mapM convExpr l
convExpr Deriv{} = return $ litString "**convExpr :: Deriv unimplemented**"
convExpr (C c)   = do
  g <- ask
  let v = quantvar (lookupC g c)
  mkVal v
convExpr (FCall c x ns) = convCall c x ns fApp
convExpr (New c x ns) = convCall c x ns (\m _ -> ctorCall m)
convExpr (Message a m x ns) = do
  g <- ask
  let info = sysinfodb $ csi $ codeSpec g
      objCd = quantvar (symbResolve info a)
  o <- mkVal objCd
  convCall m x ns (\_ n t ps nas -> return (objMethodCallMixedArgs t o n ps nas))
convExpr (UnaryOp o u) = fmap (unop o) (convExpr u)
convExpr (BinaryOp Frac (Int a) (Int b)) = do -- hack to deal with integer division
  g <- ask
  let sm = spaceMatches g
      getLiteral Double = litDouble (fromIntegral a) #/ litDouble (fromIntegral b)
      getLiteral Float = litFloat (fromIntegral a) #/ litFloat (fromIntegral b)
      getLiteral _ = error "convExpr: Rational space matched to invalid CodeType; should be Double or Float"
  return $ getLiteral (sm Rational)
convExpr (BinaryOp o a b)  = liftM2 (bfunc o) (convExpr a) (convExpr b)
convExpr (Case c l)      = doit l -- FIXME this is sub-optimal
  where
    doit [] = error "should never happen"
    doit [(e,_)] = convExpr e -- should always be the else clause
    doit ((e,cond):xs) = liftM3 inlineIf (convExpr cond) (convExpr e) 
      (convExpr (Case c xs))
convExpr (Matrix [e:es]) = do
  (el:els) <- mapM convExpr (e:es)
  return $ litArray (fmap valueType el) els
convExpr Matrix{}    = error "convExpr: Matrix"
convExpr Operator{} = error "convExpr: Operator"
convExpr IsIn{}    = error "convExpr: IsIn"
convExpr (RealI c ri)  = do
  g <- ask
  convExpr $ renderRealInt (lookupC g c) ri

convCall :: (ProgramSym repr) => UID -> [Expr] -> [(UID, Expr)] -> 
  (String -> String -> VS (repr (Type repr)) -> [VS (repr (Value repr))] -> 
  [(VS (repr (Variable repr)), VS (repr (Value repr)))] -> 
  Reader DrasilState (VS (repr (Value repr)))) -> 
  Reader DrasilState (VS (repr (Value repr)))
convCall c x ns f = do
  g <- ask
  let info = sysinfodb $ csi $ codeSpec g
      mem = eMap $ codeSpec g
      funcCd = quantfunc (symbResolve info c)
      funcNm = codeName funcCd
  funcTp <- codeType funcCd
  args <- mapM convExpr x
  nms <- mapM (mkVar . quantfunc . symbResolve info . fst) ns 
  nargs <- mapM (convExpr . snd) ns
  maybe (error $ "Call to non-existent function " ++ funcNm) (\m -> f m funcNm 
    (convType funcTp) args (zip nms nargs)) (Map.lookup funcNm mem)
  
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

unop :: (ProgramSym repr) => UFunc -> (VS (repr (Value repr)) -> 
  VS (repr (Value repr)))
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

bfunc :: (ProgramSym repr) => BinOp -> (VS (repr (Value repr)) -> 
  VS (repr (Value repr)) -> VS (repr (Value repr)))
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
genModDef :: (ProgramSym repr) => Mod -> 
  Reader DrasilState (FS (repr (RenderFile repr)))
genModDef (Mod n desc is cs fs) = genModuleWithImports n desc is (map (fmap 
  Just . genFunc) fs) 
  (case cs of [] -> []
              (cl:cls) -> fmap Just (genClass primaryClass cl) : 
                map (fmap Just . genClass auxClass) cls)

genModFuncs :: (ProgramSym repr) => Mod -> 
  [Reader DrasilState (MS (repr (Method repr)))]
genModFuncs (Mod _ _ _ _ fs) = map genFunc fs

genClass :: (ProgramSym repr) => (String -> Label -> Maybe Label -> 
  [CS (repr (StateVar repr))] -> Reader DrasilState [MS (repr (Method repr))] 
  -> Reader DrasilState (CS (repr (Class repr)))) -> M.Class -> 
  Reader DrasilState (CS (repr (Class repr)))
genClass f (M.ClassDef n i desc svs ms) = do
  svrs <- mapM (\v -> fmap (pubMVar . var (codeName v) . convType) (codeType v))
    svs
  f n desc i svrs (mapM genFunc ms) 

genFunc :: (ProgramSym repr) => Func -> Reader DrasilState (MS (repr (Method repr)))
genFunc (FDef (FuncDef n desc parms o rd s)) = do
  g <- ask
  stmts <- mapM convStmt s
  vars <- mapM mkVar (fstdecl (sysinfodb $ csi $ codeSpec g) s \\ parms)
  publicFunc n (convType $ spaceMatches g o) desc parms rd 
    [block $ map varDec vars ++ stmts]
genFunc (FDef (CtorDef n desc parms i s)) = do
  g <- ask
  inits <- mapM (convExpr . snd) i
  initvars <- mapM ((\iv -> fmap (var (codeName iv) . convType) (codeType iv))
    . fst) i
  stmts <- mapM convStmt s
  vars <- mapM mkVar (fstdecl (sysinfodb $ csi $ codeSpec g) s \\ parms)
  genInitConstructor n desc parms (zip initvars inits) 
    [block $ map varDec vars ++ stmts]
genFunc (FData (FuncData n desc ddef)) = genDataFunc n desc ddef

convStmt :: (ProgramSym repr) => FuncStmt -> Reader DrasilState (MS (repr (Statement repr)))
convStmt (FAsg v (Matrix [es]))  = do
  els <- mapM convExpr es
  v' <- mkVar v
  t <- codeType v
  let listFunc (C.List _) = litList
      listFunc (C.Array _) = litArray
      listFunc _ = error "Type mismatch between variable and value in assignment FuncStmt"
  l <- maybeLog v'
  return $ multi $ assign v' (listFunc t (listInnerType $ fmap variableType v') 
    els) : l
convStmt (FAsg v e) = do
  e' <- convExpr e
  v' <- mkVar v
  l <- maybeLog v'
  return $ multi $ assign v' e' : l
convStmt (FAsgIndex v i e) = do
  e' <- convExpr e
  v' <- mkVar v
  let vi = arrayElem i v'
  l <- maybeLog vi
  return $ multi $ assign vi e' : l
convStmt (FAsgObjVar o v e) = do
  e' <- convExpr e
  o' <- mkVar o
  t <- codeType v
  let ov = objVar o' (var (codeName v) (convType t))
  l <- maybeLog ov
  return $ multi $ assign ov e' : l
convStmt (FFor v e st) = do
  stmts <- mapM convStmt st
  vari <- mkVar v
  e' <- convExpr $ getUpperBound e
  return $ forRange vari (litInt 0) e' (litInt 1) (bodyStatements stmts)
convStmt (FForEach v e st) = do
  stmts <- mapM convStmt st
  vari <- mkVar v
  e' <- convExpr e
  return $ forEach vari e' (bodyStatements stmts)
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
      convDec (C.Array _) = arrayDec 0 vari
      convDec _ = varDec vari
  fmap convDec (codeType v) 
convStmt (FDecDef v (Matrix [[]])) = convStmt (FDec v)
convStmt (FDecDef v e) = do
  v' <- mkVar v
  l <- maybeLog v'
  t <- codeType v
  let convDecDef (Matrix [lst]) = do
        let contDecDef (C.List _) = listDecDef
            contDecDef (C.Array _) = arrayDecDef
            contDecDef _ = error "Type mismatch between variable and value in declare-define FuncStmt"
        e' <- mapM convExpr lst
        return $ contDecDef t v' e'
      convDecDef _ = do
        e' <- convExpr e
        return $ varDecDef v' e'
  dd <- convDecDef e
  return $ multi $ dd : l
convStmt (FVal e) = do
  e' <- convExpr e
  return $ valState e'
convStmt (FMulti ss) = do
  stmts <- mapM convStmt ss
  return $ multi stmts
convStmt (FAppend a b) = do
  a' <- convExpr a
  b' <- convExpr b
  return $ valState $ listAppend a' b'

genDataFunc :: (ProgramSym repr) => Name -> String -> DataDesc -> 
  Reader DrasilState (MS (repr (Method repr)))
genDataFunc nameTitle desc ddef = do
  let parms = getInputs ddef
  bod <- readData ddef
  publicFunc nameTitle void desc (quantvar inFileName : parms) Nothing bod

-- this is really ugly!!
readData :: (ProgramSym repr) => DataDesc -> Reader DrasilState
  [MS (repr (Block repr))]
readData ddef = do
  inD <- mapM inData ddef
  v_filename <- mkVal $ quantvar inFileName
  return [block $ 
    varDec var_infile :
    (if any (\d -> isLine d || isLines d) ddef then [varDec var_line, listDec 0 var_linetokens] else []) ++
    [listDec 0 var_lines | any isLines ddef] ++
    openFileR var_infile v_filename :
    concat inD ++ [
    closeFile v_infile ]]
  where inData :: (ProgramSym repr) => Data -> Reader DrasilState [MS (repr (Statement repr))]
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
                forRange var_i (litInt 0) (listSize v_lines) (litInt 1)
                  (bodyStatements $ stringSplit d var_linetokens (
                  listAccess v_lines v_i) : lnV)]
              readLines (Just numLines) = [forRange var_i (litInt 0) 
                (litInt numLines) (litInt 1)
                (bodyStatements $
                  [getFileInputLine v_infile var_line,
                   stringSplit d var_linetokens v_line
                  ] ++ lnV)]
          return $ readLines ls ++ logs
        ---------------
        lineData :: (ProgramSym repr) => Maybe String -> LinePattern -> 
          Reader DrasilState [MS (repr (Statement repr))]
        lineData s p@(Straight _) = do
          vs <- getEntryVars s p
          return [stringListVals vs v_linetokens]
        lineData s p@(Repeat ds) = do
          vs <- getEntryVars s p
          sequence $ clearTemps s ds ++ return (stringListLists vs v_linetokens)
            : appendTemps s ds
        ---------------
        clearTemps :: (ProgramSym repr) => Maybe String -> [DataItem] -> 
          [Reader DrasilState (MS (repr (Statement repr)))]
        clearTemps Nothing _ = []
        clearTemps (Just sfx) es = map (clearTemp sfx) es
        ---------------
        clearTemp :: (ProgramSym repr) => String -> DataItem -> 
          Reader DrasilState (MS (repr (Statement repr)))
        clearTemp sfx v = fmap (\t -> listDecDef (var (codeName v ++ sfx) 
          (listInnerType $ convType t)) []) (codeType v)
        ---------------
        appendTemps :: (ProgramSym repr) => Maybe String -> [DataItem] -> 
          [Reader DrasilState (MS (repr (Statement repr)))]
        appendTemps Nothing _ = []
        appendTemps (Just sfx) es = map (appendTemp sfx) es
        ---------------
        appendTemp :: (ProgramSym repr) => String -> DataItem -> 
          Reader DrasilState (MS (repr (Statement repr)))
        appendTemp sfx v = fmap (\t -> valState $ listAppend 
          (valueOf $ var (codeName v) (convType t)) 
          (valueOf $ var (codeName v ++ sfx) (convType t))) (codeType v)
        ---------------
        l_line, l_lines, l_linetokens, l_infile, l_i :: Label
        var_line, var_lines, var_linetokens, var_infile, var_i :: 
          (ProgramSym repr) => VS (repr (Variable repr))
        v_line, v_lines, v_linetokens, v_infile, v_i ::
          (ProgramSym repr) => VS (repr (Value repr))
        l_line = "line"
        var_line = var l_line string
        v_line = valueOf var_line
        l_lines = "lines"
        var_lines = var l_lines (listType string)
        v_lines = valueOf var_lines
        l_linetokens = "linetokens"
        var_linetokens = var l_linetokens (listType string)
        v_linetokens = valueOf var_linetokens
        l_infile = "infile"
        var_infile = var l_infile infile
        v_infile = valueOf var_infile
        l_i = "i"
        var_i = var l_i int
        v_i = valueOf var_i

getEntryVars :: (ProgramSym repr) => Maybe String -> LinePattern -> 
  Reader DrasilState [VS (repr (Variable repr))]
getEntryVars s lp = mapM (maybe mkVar (\st v -> codeType v >>= (variable 
  (codeName v ++ st) . listInnerType . convType)) s) (getPatternInputs lp)

getEntryVarLogs :: (ProgramSym repr) => LinePattern -> 
  Reader DrasilState [MS (repr (Statement repr))]
getEntryVarLogs lp = do
  vs <- getEntryVars Nothing lp
  logs <- mapM maybeLog vs
  return $ concat logs
