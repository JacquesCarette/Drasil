{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE Rank2Types #-}
module Language.Drasil.Code.Imperative.Import (codeType, publicFunc, 
  privateMethod, publicInOutFunc, privateInOutMethod, genConstructor, mkVar, 
  mkVal, convExpr, genModDef, genModFuncs, genModClasses, readData, renderC
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
  Mod(..), Name, Description, fstdecl)
import qualified Language.Drasil.Mod as M (Class(..))

import GOOL.Drasil (Label, SFile, MSBody, MSBlock, VSType, SVariable, SValue, 
  MSStatement, MSParameter, SMethod, CSStateVar, SClass, NamedArgs, 
  Initializers, OOProg, PermanenceSym(..), bodyStatements, BlockSym(..), 
  TypeSym(..), VariableSym(..), VariableElim(..), ($->), ValueSym(..), 
  Literal(..), VariableValue(..), NumericExpression(..), BooleanExpression(..), 
  Comparison(..), ValueExpression(..), objMethodCallMixedArgs, List(..), 
  StatementSym(..), AssignStatement(..), DeclStatement(..), IOStatement(..),
  StringStatement(..), ControlStatement(..), ifNoElse, ScopeSym(..), 
  MethodSym(..), pubDVar, nonInitConstructor, convType, CodeType(..), 
  onStateValue) 
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

-- | If UID for the variable is matched to a concept, call conceptToGOOL to get 
-- the GOOL code for the concept, and return.
-- If UID is for a constant and user has chosen Inline, convert the constant's 
-- defining Expr to a value with convExpr.
-- Otherwise, just a regular variable: construct it by calling variable, then 
-- call valueOf to reference its value
value :: (OOProg r) => UID -> Name -> VSType r -> Reader DrasilState (SValue r)
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

-- | If variable is an input, construct it with var and pass to inputVariable
-- If variable is a constant and Var constant representation is chosen,
-- construct it with var and pass to constVariable.
-- If variable is a constant and Const constant representation is chosen,
-- construct it with staticVar and pass to constVariable
-- If variable is neither, just construct it with var and return it
variable :: (OOProg r) => Name -> VSType r -> Reader DrasilState (SVariable r)
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
  
-- | If Unbundled inputs, just return variable as-is
-- If Bundled inputs, access variable through object, where the object is self 
-- if current module is InputParameters, inParams otherwise
-- Final case is for when constVariable calls inputVariable, when user chooses 
-- WithInputs for constant structure, and inputs are Bundled, and constant
-- representation is Const. Variable should be accessed through class, so 
-- classVariable is called.
inputVariable :: (OOProg r) => Structure -> ConstantRepr -> SVariable r -> 
  Reader DrasilState (SVariable r)
inputVariable Unbundled _ v = return v
inputVariable Bundled Var v = do
  g <- ask
  let inClsName = "InputParameters"
  ip <- mkVar (quantvar inParams)
  return $ if currentClass g == inClsName then objVarSelf v else ip $-> v
inputVariable Bundled Const v = do
  ip <- mkVar (quantvar inParams)
  classVariable ip v

-- | If Unbundled constants, just return variable as-is
-- If Bundled constants and Var constant representation, access variable 
-- through consts object.
-- If Bundled constants and Const constant representation, access variable 
-- through class, so call classVariable.
-- If constants stored WithInputs, call inputVariable
-- If constants Inlined, the generator should not be attempting to make a 
-- variable for one of the constants.
constVariable :: (OOProg r) => ConstantStructure -> ConstantRepr -> 
  SVariable r -> Reader DrasilState (SVariable r)
constVariable (Store Unbundled) _ v = return v
constVariable (Store Bundled) Var v = do
  cs <- mkVar (quantvar consts)
  return $ cs $-> v
constVariable (Store Bundled) Const v = do
  cs <- mkVar (quantvar consts)
  classVariable cs v
constVariable WithInputs cr v = do
  g <- ask
  inputVariable (inStruct g) cr v
constVariable Inline _ _ = error $ "mkVar called on a constant, but user " ++
  "chose to Inline constants. Generator has bug."

-- | For generating GOOL for a variable that is accessed through a class.
-- If the variable is not in export map, then it is not a public class variable 
-- and cannot be accessed, so throw error
-- If the variable is exported by the current module, use classVar
-- If the variable is exported by a different module, use extClassVar
classVariable :: (OOProg r) => SVariable r -> SVariable r -> 
  Reader DrasilState (SVariable r)
classVariable c v = do
  g <- ask
  let checkCurrent m = if currentModule g == m then classVar else extClassVar
  return $ do
    v' <- v
    let nm = variableName v'
    maybe (error $ "Variable " ++ nm ++ " missing from export map") 
      checkCurrent (Map.lookup nm (eMap $ codeSpec g)) (onStateValue variableType c) v

mkVal :: (OOProg r, HasUID c, HasSpace c, CodeIdea c) => c -> 
  Reader DrasilState (SValue r)
mkVal v = do
  t <- codeType v
  value (v ^. uid) (codeName v) (convType t)

mkVar :: (OOProg r, HasSpace c, CodeIdea c) => c -> 
  Reader DrasilState (SVariable r)
mkVar v = do
  t <- codeType v
  variable (codeName v) (convType t)

publicFunc :: (OOProg r, HasSpace c, CodeIdea c) => Label -> VSType r -> 
  Description -> [c] -> Maybe Description -> [MSBlock r] ->
  Reader DrasilState (SMethod r)
publicFunc n t = genMethod (function n public static t) n

privateMethod :: (OOProg r, HasSpace c, CodeIdea c) => Label -> VSType r -> 
  Description -> [c] -> Maybe Description -> [MSBlock r] -> 
  Reader DrasilState (SMethod r)
privateMethod n t = genMethod (method n private dynamic t) n

publicInOutFunc :: (OOProg r, HasSpace c, CodeIdea c, Eq c) => Label -> 
  Description -> [c] -> [c] -> [MSBlock r] -> Reader DrasilState (SMethod r)
publicInOutFunc n = genInOutFunc (inOutFunc n) (docInOutFunc n) public static n

privateInOutMethod :: (OOProg r, HasSpace c, CodeIdea c, Eq c) => Label -> 
  Description -> [c] -> [c] -> [MSBlock r] -> Reader DrasilState (SMethod r)
privateInOutMethod n = genInOutFunc (inOutMethod n) (docInOutMethod n) 
  private dynamic n

genConstructor :: (OOProg r, HasSpace c, CodeIdea c) => Label -> Description -> 
  [c] -> [MSBlock r] -> Reader DrasilState (SMethod r)
genConstructor n desc p = genMethod nonInitConstructor n desc p Nothing

genInitConstructor :: (OOProg r, HasSpace c, CodeIdea c) => Label -> 
  Description -> [c] -> Initializers r -> [MSBlock r] -> 
  Reader DrasilState (SMethod r)
genInitConstructor n desc p is = genMethod (`constructor` is) n desc p 
  Nothing

genMethod :: (OOProg r, HasSpace c, CodeIdea c) => ([MSParameter r] -> 
  MSBody r -> SMethod r) -> Label -> Description -> [c] -> Maybe Description -> 
  [MSBlock r] -> Reader DrasilState (SMethod r)
genMethod f n desc p r b = do
  g <- ask
  vars <- mapM mkVar p
  bod <- logBody n vars b
  let ps = map mkParam vars
      fn = f ps bod
  pComms <- mapM getComment p
  return $ if CommentFunc `elem` commented g
    then docFunc desc pComms r fn else fn

genInOutFunc :: (OOProg r, HasSpace c, CodeIdea c, Eq c) => (r (Scope r) -> 
    r (Permanence r) -> [SVariable r] -> [SVariable r] -> [SVariable r] -> 
    MSBody r -> SMethod r) -> 
  (r (Scope r) -> r (Permanence r) -> String -> [(String, SVariable r)] -> 
    [(String, SVariable r)] -> [(String, SVariable r)] -> MSBody r -> SMethod r)
  -> r (Scope r) -> r (Permanence r) -> Label -> Description -> [c] -> [c] -> 
  [MSBlock r] -> Reader DrasilState (SMethod r)
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

convExpr :: (OOProg r) => Expr -> Reader DrasilState (SValue r)
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

convCall :: (OOProg r) => UID -> [Expr] -> [(UID, Expr)] -> (Name -> Name -> 
  VSType r -> [SValue r] -> NamedArgs r -> Reader DrasilState (SValue r)) -> 
  Reader DrasilState (SValue r)
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

unop :: (OOProg r) => UFunc -> (SValue r -> SValue r)
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

bfunc :: (OOProg r) => BinOp -> (SValue r -> SValue r -> SValue r)
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
genModDef :: (OOProg r) => Mod -> Reader DrasilState (SFile r)
genModDef (Mod n desc is cs fs) = genModuleWithImports n desc is (map (fmap 
  Just . genFunc) fs) 
  (case cs of [] -> []
              (cl:cls) -> fmap Just (genClass primaryClass cl) : 
                map (fmap Just . genClass auxClass) cls)

genModFuncs :: (OOProg r) => Mod -> [Reader DrasilState (SMethod r)]
genModFuncs (Mod _ _ _ _ fs) = map genFunc fs

genModClasses :: (OOProg r) => Mod -> [Reader DrasilState (SClass r)]
genModClasses (Mod _ _ _ cs _) = map (genClass auxClass) cs

genClass :: (OOProg r) => (Name -> Maybe Name -> Description -> [CSStateVar r] 
  -> Reader DrasilState [SMethod r] -> Reader DrasilState (SClass r)) -> 
  M.Class -> Reader DrasilState (SClass r)
genClass f (M.ClassDef n i desc svs ms) = do
  svrs <- mapM (\v -> fmap (pubDVar . var (codeName v) . convType) (codeType v))
    svs
  f n i desc svrs (mapM genFunc ms) 

genFunc :: (OOProg r) => Func -> Reader DrasilState (SMethod r)
genFunc (FDef (FuncDef n desc parms o rd s)) = do
  g <- ask
  stmts <- mapM convStmt s
  vars <- mapM mkVar (fstdecl (sysinfodb $ csi $ codeSpec g) s \\ parms)
  publicFunc n (convType $ spaceMatches g o) desc parms rd 
    [block $ map varDec vars, block stmts]
genFunc (FDef (CtorDef n desc parms i s)) = do
  g <- ask
  inits <- mapM (convExpr . snd) i
  initvars <- mapM ((\iv -> fmap (var (codeName iv) . convType) (codeType iv))
    . fst) i
  stmts <- mapM convStmt s
  vars <- mapM mkVar (fstdecl (sysinfodb $ csi $ codeSpec g) s \\ parms)
  genInitConstructor n desc parms (zip initvars inits) 
    [block $ map varDec vars, block stmts]
genFunc (FData (FuncData n desc ddef)) = genDataFunc n desc ddef

convStmt :: (OOProg r) => FuncStmt -> Reader DrasilState (MSStatement r)
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
  return $ returnStmt e'
convStmt (FThrow s) = return $ throw s
convStmt (FTry t c) = do
  stmt1 <- mapM convStmt t
  stmt2 <- mapM convStmt c
  return $ tryCatch (bodyStatements stmt1) (bodyStatements stmt2)
convStmt FContinue = return continue
convStmt (FDecDef v (Matrix [[]])) = do
  vari <- mkVar v
  let convDec (C.List _) = listDec 0 vari
      convDec (C.Array _) = arrayDec 0 vari
      convDec _ = varDec vari
  fmap convDec (codeType v) 
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
  return $ valStmt e'
convStmt (FMulti ss) = do
  stmts <- mapM convStmt ss
  return $ multi stmts
convStmt (FAppend a b) = do
  a' <- convExpr a
  b' <- convExpr b
  return $ valStmt $ listAppend a' b'

genDataFunc :: (OOProg r) => Name -> Description -> DataDesc -> 
  Reader DrasilState (SMethod r)
genDataFunc nameTitle desc ddef = do
  let parms = getInputs ddef
  bod <- readData ddef
  publicFunc nameTitle void desc (quantvar inFileName : parms) Nothing bod

-- this is really ugly!!
readData :: (OOProg r) => DataDesc -> Reader DrasilState [MSBlock r]
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
  where inData :: (OOProg r) => Data -> Reader DrasilState [MSStatement r]
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
        lineData :: (OOProg r) => Maybe String -> LinePattern -> 
          Reader DrasilState [MSStatement r]
        lineData s p@(Straight _) = do
          vs <- getEntryVars s p
          return [stringListVals vs v_linetokens]
        lineData s p@(Repeat ds) = do
          vs <- getEntryVars s p
          sequence $ clearTemps s ds ++ return (stringListLists vs v_linetokens)
            : appendTemps s ds
        ---------------
        clearTemps :: (OOProg r) => Maybe String -> [DataItem] -> 
          [Reader DrasilState (MSStatement r)]
        clearTemps Nothing _ = []
        clearTemps (Just sfx) es = map (clearTemp sfx) es
        ---------------
        clearTemp :: (OOProg r) => String -> DataItem -> 
          Reader DrasilState (MSStatement r)
        clearTemp sfx v = fmap (\t -> listDecDef (var (codeName v ++ sfx) 
          (listInnerType $ convType t)) []) (codeType v)
        ---------------
        appendTemps :: (OOProg r) => Maybe String -> [DataItem] -> 
          [Reader DrasilState (MSStatement r)]
        appendTemps Nothing _ = []
        appendTemps (Just sfx) es = map (appendTemp sfx) es
        ---------------
        appendTemp :: (OOProg r) => String -> DataItem -> 
          Reader DrasilState (MSStatement r)
        appendTemp sfx v = fmap (\t -> valStmt $ listAppend 
          (valueOf $ var (codeName v) (convType t)) 
          (valueOf $ var (codeName v ++ sfx) (convType t))) (codeType v)
        ---------------
        l_line, l_lines, l_linetokens, l_infile, l_i :: Label
        var_line, var_lines, var_linetokens, var_infile, var_i :: 
          (OOProg r) => SVariable r
        v_line, v_lines, v_linetokens, v_infile, v_i ::
          (OOProg r) => SValue r
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

getEntryVars :: (OOProg r) => Maybe String -> LinePattern -> 
  Reader DrasilState [SVariable r]
getEntryVars s lp = mapM (maybe mkVar (\st v -> codeType v >>= (variable 
  (codeName v ++ st) . listInnerType . convType)) s) (getPatternInputs lp)

getEntryVarLogs :: (OOProg r) => LinePattern -> 
  Reader DrasilState [MSStatement r]
getEntryVarLogs lp = do
  vs <- getEntryVars Nothing lp
  logs <- mapM maybeLog vs
  return $ concat logs
