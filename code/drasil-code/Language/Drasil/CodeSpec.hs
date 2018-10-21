{-# LANGUAGE GADTs #-}
module Language.Drasil.CodeSpec where

import Language.Drasil
import Language.Drasil.Chunk.Code (CodeChunk, CodeDefinition, CodeIdea, ConstraintMap,
  codevar, codeEquat, funcPrefix, codeName, spaceToCodeType, toCodeName, constraintMap,
  qtov, qtoc, symbToCodeName, codeType)
import Language.Drasil.Code.Code (CodeType)
import Language.Drasil.Code.DataDesc (DataDesc, getInputs)

import qualified Data.Map as Map
import Control.Lens ((^.))
import Data.List (nub, delete, (\\))

import Prelude hiding (const)

type Input = CodeChunk
type Output = CodeChunk
type Const = CodeDefinition
type Derived = CodeDefinition
type Def = CodeDefinition

data Lang = Cpp
          | CSharp
          | Java
          | Python
          deriving Eq

data CodeSpec where
  CodeSpec :: CommonIdea a => {
  program :: a,
  inputs :: [Input],
  extInputs :: [Input],
  derivedInputs :: [Derived],
  outputs :: [Output],
  relations :: [Def],
  execOrder :: [Def],
  cMap :: ConstraintMap,
  fMap :: FunctionMap,
  vMap :: VarMap,
  eMap :: ModExportMap,
  constMap :: FunctionMap,
  const :: [Const],
  mods :: [Mod],  -- medium hack
  dMap :: ModDepMap,
  sysinfodb :: ChunkDB
  } -> CodeSpec

type FunctionMap = Map.Map String CodeDefinition
type VarMap      = Map.Map String CodeChunk

assocToMap :: CodeIdea a => [a] -> Map.Map String a
assocToMap = Map.fromList . map (\x -> (codeName x, x))

funcTerm :: String -> FunctionMap -> String
funcTerm cname m = maybe "" (\cd -> getStr (phrase cd)) (Map.lookup cname m)
       
varTerm :: String -> VarMap -> String
varTerm cname m = maybe "" (\cch -> getStr (phrase cch)) (Map.lookup cname m)
        
varType :: String -> VarMap -> CodeType
varType cname m = maybe (error "Variable not found") codeType (Map.lookup cname m)
        
getStr :: Sentence -> String
getStr (S s) = s
getStr (P s) = symbToCodeName s
getStr ((:+:) s1 s2) = getStr s1 ++ getStr s2
getStr _ = error "Term is not a string" 

codeSpec :: SystemInformation -> [Mod] -> CodeSpec
codeSpec (SI {_sys = sys
              , _quants = q
              , _definitions = defs'
              , _datadefs = ddefs
              , _inputs = ins
              , _outputs = outs
              , _constraints = cs
              , _constants = constants
              , _sysinfodb = db}) ms = 
  let inputs' = map codevar ins
      const' = map qtov constants
      derived = map qtov $ getDerivedInputs ddefs defs' inputs' const' db
      rels = (map qtoc ({-defs'++-}(map qdFromDD ddefs))) \\ derived
      mods' = prefixFunctions $ (packmod "Calculations" $ map FCD rels):ms 
      mem   = modExportMap mods' inputs' const'
      outs' = map codevar outs
      allInputs = nub $ inputs' ++ map codevar derived
  in  CodeSpec {
        program = sys,
        inputs = allInputs,
        extInputs = inputs',
        derivedInputs = derived,
        outputs = outs',
        relations = rels,
        execOrder = getExecOrder rels (allInputs ++ map codevar const') outs' db,
        cMap = constraintMap cs,
        fMap = assocToMap rels,
        vMap = assocToMap (map codevar q),
        eMap = mem,
        constMap = assocToMap const',
        const = const',
        mods = mods',
        dMap = modDepMap db mem mods',
        sysinfodb = db
      }

data Choices = Choices {
  lang :: [Lang],
  impType :: ImplementationType,
  logFile :: String,
  logging :: Logging,
  comments :: Comments,
  onSfwrConstraint :: ConstraintBehaviour,
  onPhysConstraint :: ConstraintBehaviour,
  inputStructure :: Structure
}

data ImplementationType = Library
                        | Program

data Logging = LogNone
             | LogFunc
             | LogVar
             | LogAll
             
data Comments = CommentNone
              | CommentFunc
             
data ConstraintBehaviour = Warning
                         | Exception
                         
data Structure = Loose
               | AsClass
             
defaultChoices :: Choices
defaultChoices = Choices {
  lang = [Python],
  impType = Program,
  logFile = "log.txt",
  logging = LogNone,
  comments = CommentNone,
  onSfwrConstraint = Exception,
  onPhysConstraint = Warning,
  inputStructure = AsClass
}

type Name = String

-- medium hacks ---
relToQD :: (ExprRelat c, HasSymbolTable ctx) => ctx -> c -> QDefinition
relToQD sm r = convertRel sm (r ^. relat)

convertRel :: (HasSymbolTable ctx) => ctx -> Expr -> QDefinition
convertRel sm (BinaryOp Eq (C x) r) = ec (symbLookup x (sm ^. symbolTable)) r
convertRel _ _ = error "Conversion failed"

data Mod = Mod Name [Func]

packmod :: Name -> [Func] -> Mod
packmod n fs = Mod (toCodeName n) fs

data DMod = DMod [Name] Mod
     
data Func = FCD CodeDefinition
          | FDef FuncDef
          | FData FuncData

funcQD :: QDefinition -> Func
funcQD qd = FCD $ qtoc qd 

funcData :: Name -> DataDesc -> Func
funcData n dd = FData $ FuncData (toCodeName n) dd 

funcDef :: (Quantity c) => Name -> [c] -> Space -> [FuncStmt] -> Func  
funcDef s i t fs  = FDef $ FuncDef (toCodeName s) (map (codevar ) i) (spaceToCodeType t) fs 
     
data FuncData where
  FuncData :: Name -> DataDesc -> FuncData
  
data FuncDef where
  FuncDef :: Name -> [CodeChunk] -> CodeType -> [FuncStmt] -> FuncDef
 
data FuncStmt where
  FAsg :: CodeChunk -> Expr -> FuncStmt
  FFor :: CodeChunk -> Expr -> [FuncStmt] -> FuncStmt
  FWhile :: Expr -> [FuncStmt] -> FuncStmt
  FCond :: Expr -> [FuncStmt] -> [FuncStmt] -> FuncStmt
  FRet :: Expr -> FuncStmt
  FThrow :: String -> FuncStmt
  FTry :: [FuncStmt] -> [FuncStmt] -> FuncStmt
  FContinue :: FuncStmt
  FDec :: CodeChunk -> CodeType -> FuncStmt
  FProcCall :: Func -> [Expr] -> FuncStmt
  -- slight hack, for now
  FAppend :: Expr -> Expr -> FuncStmt
  
($:=) :: (Quantity c) => c -> Expr -> FuncStmt
v $:= e = FAsg (codevar v) e

ffor :: (Quantity c) => c -> Expr -> [FuncStmt] -> FuncStmt
ffor v e fs  = FFor (codevar  v) e fs

fdec :: (Quantity c) => c -> FuncStmt
fdec v  = FDec (codevar  v) (spaceToCodeType $ v ^. typ)

asVC :: Func -> QuantityDict
asVC (FDef (FuncDef n _ _ _)) = implVar n (nounPhraseSP n) (Atomic n) Real
asVC (FData (FuncData n _)) = implVar n (nounPhraseSP n) (Atomic n) Real
asVC (FCD cd) = codeVC cd (codeSymb cd) (cd ^. typ)

asExpr :: Func -> Expr
asExpr f = sy $ asVC f

-- name of variable/function maps to module name
type ModExportMap = Map.Map String String

modExportMap :: [Mod] -> [Input] -> [Const] -> ModExportMap
modExportMap ms ins _ = Map.fromList $ concatMap mpair ms
  where mpair (Mod n fs) = map fname fs `zip` repeat n
                        ++ map codeName ins `zip` repeat "InputParameters"
                        ++ [ ("get_input", "InputFormat"),
                             ("derived_values", "DerivedValues"),
                             ("input_constraints", "InputConstraints"),
                             ("write_output", "OutputFormat") ]  -- hardcoded for now
                     --   ++ map codeName consts `zip` repeat "Constants"
                     -- inlining constants for now
          
type ModDepMap = Map.Map String [String]

modDepMap :: HasSymbolTable ctx => ctx -> ModExportMap -> [Mod] -> ModDepMap
modDepMap sm mem ms  = Map.fromList $ map (\(Mod n _) -> n) ms `zip` map getModDep ms 
                                   ++ [("Control", [ "InputParameters",  
                                                     "DerivedValues",
                                                     "InputConstraints",
                                                     "InputFormat",
                                                     "OutputFormat",
                                                     "Calculations" ] ),
                                       ("DerivedValues", [ "InputParameters" ] ),
                                       ("InputConstraints", [ "InputParameters" ] )]  -- hardcoded for now
                                                                          -- will fix later
  where getModDep (Mod name' funcs) = 
          delete name' $ nub $ concatMap getDep (concatMap fdep funcs)
        getDep n = maybe [] (\x -> [x]) (Map.lookup n mem)        
        fdep (FCD cd) = codeName cd:map codeName (codevars  (codeEquat cd) sm)
        fdep (FDef (FuncDef _ i _ fs)) = map codeName (i ++ concatMap (fstdep sm ) fs)
        fdep (FData (FuncData _ d)) = map codeName $ getInputs d   

fstdep :: HasSymbolTable ctx => ctx -> FuncStmt ->[CodeChunk]
fstdep _  (FDec cch _) = [cch]
fstdep sm (FAsg cch e) = cch:codevars e sm
fstdep sm (FFor cch e fs) = delete cch $ nub (codevars  e sm ++ concatMap (fstdep sm ) fs)
fstdep sm (FWhile e fs) = codevars e sm ++ concatMap (fstdep sm ) fs
fstdep sm (FCond e tfs efs)  = codevars e sm ++ concatMap (fstdep sm ) tfs ++ concatMap (fstdep sm ) efs
fstdep sm (FRet e)  = codevars  e sm
fstdep sm (FTry tfs cfs) = concatMap (fstdep sm ) tfs ++ concatMap (fstdep sm ) cfs
fstdep _  (FThrow _) = [] -- is this right?
fstdep _  (FContinue) = []
fstdep sm (FProcCall _ l)  = concatMap (\x -> codevars  x sm) l
fstdep sm (FAppend a b)  = nub (codevars  a sm ++ codevars  b sm)

fstdecl :: HasSymbolTable ctx => ctx -> [FuncStmt] -> [CodeChunk]
fstdecl ctx fsts = (nub $ concatMap (fstvars ctx) fsts) \\ (nub $ concatMap (declared ctx) fsts) 
  where
    fstvars :: HasSymbolTable ctx => ctx -> FuncStmt -> [CodeChunk]
    fstvars _  (FDec cch _) = [cch]
    fstvars sm (FAsg cch e) = cch:codevars' e sm
    fstvars sm (FFor cch e fs) = delete cch $ nub (codevars' e sm ++ concatMap (fstvars sm) fs)
    fstvars sm (FWhile e fs) = codevars' e sm ++ concatMap (fstvars sm) fs
    fstvars sm (FCond e tfs efs) = codevars' e sm ++ concatMap (fstvars sm) tfs ++ concatMap (fstvars sm) efs
    fstvars sm (FRet e) = codevars' e sm
    fstvars sm (FTry tfs cfs) = concatMap (fstvars sm) tfs ++ concatMap (fstvars sm ) cfs
    fstvars _  (FThrow _) = [] -- is this right?
    fstvars _  (FContinue) = []
    fstvars sm (FProcCall _ l) = concatMap (\x -> codevars x sm) l
    fstvars sm (FAppend a b) = nub (codevars a sm ++ codevars b sm)

    declared :: HasSymbolTable ctx => ctx -> FuncStmt -> [CodeChunk]
    declared _  (FDec cch _) = [cch]
    declared _  (FAsg _ _) = []
    declared sm (FFor _ _ fs) = concatMap (declared sm) fs
    declared sm (FWhile _ fs) = concatMap (declared sm) fs
    declared sm (FCond _ tfs efs) = concatMap (declared sm) tfs ++ concatMap (declared sm) efs
    declared _  (FRet _) = []
    declared sm (FTry tfs cfs) = concatMap (declared sm) tfs ++ concatMap (declared sm) cfs
    declared _  (FThrow _) = [] -- is this right?
    declared _  (FContinue) = []
    declared _  (FProcCall _ _) = []
    declared _  (FAppend _ _) = []
       
fname :: Func -> Name       
fname (FCD cd) = codeName cd
fname (FDef (FuncDef n _ _ _)) = n
fname (FData (FuncData n _)) = n 

prefixFunctions :: [Mod] -> [Mod]
prefixFunctions = map (\(Mod nm fs) -> Mod nm $ map pfunc fs)
  where pfunc f@(FCD _) = f
        pfunc (FData (FuncData n dd)) = FData (FuncData (funcPrefix ++ n) dd)
        pfunc (FDef (FuncDef n a t f)) = FDef (FuncDef (funcPrefix ++ n) a t f)

getDerivedInputs :: HasSymbolTable ctx => [DataDefinition] -> [QDefinition] -> [Input] -> [Const] -> ctx -> [QDefinition]
getDerivedInputs ddefs defs' ins consts sm  =
  let refSet = ins ++ map codevar consts
  in  if (ddefs == []) then filter ((`subsetOf` refSet) . flip (codevars) sm . (^.equat)) defs'
      else filter ((`subsetOf` refSet) . flip codevars sm . (^.defnExpr)) (map qdFromDD ddefs)

type Known = CodeChunk
type Need  = CodeChunk

getExecOrder :: HasSymbolTable ctx => [Def] -> [Known] -> [Need] -> ctx -> [Def]
getExecOrder d k' n' sm  = getExecOrder' [] d k' (n' \\ k')
  where getExecOrder' ord _ _ []   = ord
        getExecOrder' ord defs' k n = 
          let new  = filter ((`subsetOf` k) . flip (codevars') sm . codeEquat) defs'
              cnew = map codevar new
              kNew = k ++ cnew
              nNew = n \\ cnew
          in  if null new 
              then error ("Cannot find path from inputs to outputs: " ++ (show $ map (^. uid) n)
                        ++ " given Defs as " ++ (show $ map (^. uid) defs')
                        ++ " and Knowns as " ++ (show $ map (^. uid) k) )
              else getExecOrder' (ord ++ new) (defs' \\ new) kNew nNew
  
subsetOf :: (Eq a) => [a] -> [a] -> Bool  
xs `subsetOf` ys = null $ filter (not . (`elem` ys)) xs

-- | Get a list of CodeChunks from an equation
codevars :: (HasSymbolTable s) => Expr -> s -> [CodeChunk]
codevars e m = map resolve $ dep e
  where resolve x = codevar (symbLookup x $ m ^. symbolTable)

-- | Get a list of CodeChunks from an equation (no functions)
codevars' :: (HasSymbolTable s) => Expr -> s -> [CodeChunk]
codevars' e m = map resolve $ nub $ names' e
  where  resolve x = codevar (symbLookup x (m ^. symbolTable))
