{-# LANGUAGE GADTs #-}
module Language.Drasil.CodeSpec where

import Language.Drasil
import Database.Drasil (ChunkDB, SystemInformation(SI), symbResolve,
  _authors, _constants, _constraints, _datadefs, _definitions, _inputs,
  _outputs, _quants, _sys, _sysinfodb, sampleData)
import Language.Drasil.Development (dep, names', namesRI)
import Theory.Drasil (DataDefinition, qdFromDD)

import Language.Drasil.Chunk.Code (CodeChunk, CodeIdea(codeChunk), 
  ConstraintMap, programName, codevar, quantvar, quantfunc, funcPrefix, 
  codeName, constraintMap)
import Language.Drasil.Chunk.CodeDefinition (CodeDefinition, qtov, qtoc, 
  codeEquat)
import Language.Drasil.Chunk.CodeQuantity (HasCodeType(ctyp))
import Language.Drasil.Code.Code (spaceToCodeType)
import Language.Drasil.Code.CodeQuantityDicts (inFileName, inParams, consts)
import Language.Drasil.Code.DataDesc (DataDesc, getInputs)
import Language.Drasil.Printers (toPlainName)

import GOOL.Drasil (CodeType)

import Control.Lens ((^.))
import Data.List (nub, delete, (\\))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

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

data CodeSystInfo where
  CSI :: (HasName a) => {
  authors :: [a], 
  inputs :: [Input],
  extInputs :: [Input],
  derivedInputs :: [Derived],
  outputs :: [Output],
  execOrder :: [Def],
  cMap :: ConstraintMap,
  constants :: [Const],
  mods :: [Mod],  -- medium hack
  sysinfodb :: ChunkDB,
  smplData :: FilePath
  } -> CodeSystInfo

data CodeSpec where
  CodeSpec :: {
  pName :: Name,
  relations :: [Def],
  fMap :: FunctionMap,
  vMap :: VarMap,
  defMap :: ModDefinitionMap,
  eMap :: ModExportMap,
  constMap :: FunctionMap,
  csi :: CodeSystInfo
  } -> CodeSpec

type FunctionMap = Map.Map String CodeDefinition
type VarMap      = Map.Map String CodeChunk

assocToMap :: HasUID a => [a] -> Map.Map UID a
assocToMap = Map.fromList . map (\x -> (x ^. uid, x))
        
varType :: String -> VarMap -> CodeType
varType cname m = maybe (error "Variable not found") (^. ctyp) (Map.lookup cname m)

codeSpec :: SystemInformation -> Choices -> [Mod] -> CodeSpec
codeSpec SI {_sys = sys
              , _authors = as
              , _quants = q
              , _definitions = defs'
              , _datadefs = ddefs
              , _inputs = ins
              , _outputs = outs
              , _constraints = cs
              , _constants = cnsts
              , _sysinfodb = db
              , sampleData = sd} chs ms = 
  let inputs' = map quantvar ins
      const' = map qtov (filter ((`Map.notMember` conceptMatch chs) . (^. uid)) 
        cnsts)
      derived = getDerivedInputs ddefs defs' inputs' const' db
      rels = map qtoc ((defs' ++ map qdFromDD ddefs) \\ derived)
      mdm = modDefMap csi' chs
      outs' = map quantvar outs
      allInputs = nub $ inputs' ++ map quantvar derived
      exOrder = getExecOrder rels (allInputs ++ map quantvar cnsts) outs' db
      csi' = CSI {
        authors = as,
        inputs = allInputs,
        extInputs = inputs',
        derivedInputs = map qtov derived,
        outputs = outs',
        execOrder = exOrder,
        cMap = constraintMap cs,
        constants = const',
        mods = prefixFunctions $ packmod "Calculations" 
          "Provides functions for calculating the outputs" 
          (map FCD exOrder) : ms,
        sysinfodb = db,
        smplData = sd
      }
  in  CodeSpec {
        pName = programName sys,
        relations = rels,
        fMap = assocToMap rels,
        vMap = assocToMap (map quantvar q ++ getAdditionalVars chs (mods csi')),
        defMap = mdm,
        eMap = modExportMap mdm chs,
        constMap = assocToMap const',
        csi = csi'
      }

data Choices = Choices {
  lang :: [Lang],
  modularity :: Modularity,
  impType :: ImplementationType,
  logFile :: String,
  logging :: Logging,
  comments :: [Comments],
  doxVerbosity :: Verbosity,
  dates :: Visibility,
  onSfwrConstraint :: ConstraintBehaviour,
  onPhysConstraint :: ConstraintBehaviour,
  inputStructure :: Structure,
  constStructure :: ConstantStructure,
  constRepr :: ConstantRepr,
  conceptMatch :: ConceptMatchMap,
  auxFiles :: [AuxFile]
}

data Modularity = Modular InputModule | Unmodular

data ImplementationType = Library
                        | Program

data Logging = LogNone
             | LogFunc
             | LogVar
             | LogAll
             
data Comments = CommentFunc
              | CommentClass
              | CommentMod deriving Eq

data Verbosity = Verbose | Quiet
             
data ConstraintBehaviour = Warning
                         | Exception
                         
data Structure = Unbundled
               | Bundled

data ConstantStructure = Inline | WithInputs | Store Structure

data ConstantRepr = Var | Const

data InputModule = Combined
                 | Separated

type ConceptMatchMap = Map.Map UID [CodeConcept]
type MatchedConceptMap = Map.Map UID CodeConcept

data CodeConcept = Pi

matchConcepts :: (HasUID c) => [c] -> [[CodeConcept]] -> ConceptMatchMap
matchConcepts cncs cdcs = Map.fromList (zip (map (^. uid) cncs) cdcs)

data AuxFile = SampleInput deriving Eq
             
data Visibility = Show
                | Hide

inputModule :: Choices -> InputModule
inputModule c = inputModule' $ modularity c
  where inputModule' Unmodular = Combined
        inputModule' (Modular im) = im

defaultChoices :: Choices
defaultChoices = Choices {
  lang = [Python],
  modularity = Modular Combined,
  impType = Program,
  logFile = "log.txt",
  logging = LogNone,
  comments = [],
  doxVerbosity = Verbose,
  dates = Hide,
  onSfwrConstraint = Exception,
  onPhysConstraint = Warning,
  inputStructure = Bundled,
  constStructure = Inline,
  constRepr = Const,
  conceptMatch = matchConcepts ([] :: [QDefinition]) [],
  auxFiles = [SampleInput]
}

type Name = String

-- medium hacks ---
relToQD :: ExprRelat c => ChunkDB -> c -> QDefinition
relToQD sm r = convertRel sm (r ^. relat)

convertRel :: ChunkDB -> Expr -> QDefinition
convertRel sm (BinaryOp Eq (C x) r) = ec (symbResolve sm x) r
convertRel _ _ = error "Conversion failed"

data Mod = Mod Name String [Func]

packmod :: Name -> String -> [Func] -> Mod
packmod n = Mod (toPlainName n)

data DMod = DMod [Name] Mod
     
data Func = FCD CodeDefinition
          | FDef FuncDef
          | FData FuncData

funcQD :: QDefinition -> Func
funcQD qd = FCD $ qtoc qd 

funcData :: Name -> String -> DataDesc -> Func
funcData n desc d = FData $ FuncData (toPlainName n) desc d

funcDef :: (Quantity c, MayHaveUnit c) => Name -> String -> [c] -> Space -> 
  Maybe String -> [FuncStmt] -> Func  
funcDef s desc i t returnDesc fs = FDef $ FuncDef (toPlainName s) desc 
  (map quantvar i) (spaceToCodeType t) returnDesc fs 

data FuncData where
  FuncData :: Name -> String -> DataDesc -> FuncData
  
data FuncDef where
  FuncDef :: Name -> String -> [CodeChunk] -> CodeType -> Maybe String -> 
    [FuncStmt] -> FuncDef
 
data FuncStmt where
  FAsg :: CodeChunk -> Expr -> FuncStmt
  FFor :: CodeChunk -> Expr -> [FuncStmt] -> FuncStmt
  FWhile :: Expr -> [FuncStmt] -> FuncStmt
  FCond :: Expr -> [FuncStmt] -> [FuncStmt] -> FuncStmt
  FRet :: Expr -> FuncStmt
  FThrow :: String -> FuncStmt
  FTry :: [FuncStmt] -> [FuncStmt] -> FuncStmt
  FContinue :: FuncStmt
  FDec :: CodeChunk -> FuncStmt
  FProcCall :: Func -> [Expr] -> FuncStmt
  -- slight hack, for now
  FAppend :: Expr -> Expr -> FuncStmt
  
($:=) :: (Quantity c, MayHaveUnit c) => c -> Expr -> FuncStmt
v $:= e = FAsg (quantvar v) e

ffor :: (Quantity c, MayHaveUnit c) => c -> Expr -> [FuncStmt] -> FuncStmt
ffor v = FFor (quantvar  v)

fdec :: (Quantity c, MayHaveUnit c) => c -> FuncStmt
fdec v  = FDec (quantvar v)

asVC :: Func -> QuantityDict
asVC (FDef (FuncDef n _ _ _ _ _)) = implVar n (nounPhraseSP n) (Variable n) Real
asVC (FData (FuncData n _ _)) = implVar n (nounPhraseSP n) (Variable n) Real
asVC (FCD _) = error "Can't make QuantityDict from FCD function" -- codeVC cd (codeSymb cd) (cd ^. typ)

asExpr :: Func -> Expr
asExpr f = sy $ asVC f

-- FIXME: hack. Use for implementation-stage functions that need to be displayed in the SRS.
asExpr' :: Func -> Expr
asExpr' f = sy $ asVC' f

-- FIXME: Part of above hack
asVC' :: Func -> QuantityDict
asVC' (FDef (FuncDef n _ _ _ _ _)) = vc n (nounPhraseSP n) (Variable n) Real
asVC' (FData (FuncData n _ _)) = vc n (nounPhraseSP n) (Variable n) Real
asVC' (FCD _) = error "Can't make QuantityDict from FCD function" -- vc'' cd (codeSymb cd) (cd ^. typ)

getAdditionalVars :: Choices -> [Mod] -> [CodeChunk]
getAdditionalVars chs ms = map codevar (inFileName 
  : inParamsVar (inputStructure chs) 
  ++ constsVar (constStructure chs))
  ++ concatMap funcParams ms
  where inParamsVar Bundled = [inParams]
        inParamsVar Unbundled = []
        constsVar (Store Bundled) = [consts]
        constsVar _ = []
        funcParams (Mod _ _ fs) = concatMap getFuncParams fs

getFuncParams :: Func -> [CodeChunk]
getFuncParams (FDef (FuncDef _ _ ps _ _ _)) = ps
getFuncParams (FData (FuncData _ _ d)) = getInputs d
getFuncParams (FCD _) = []

-- name of variable/function maps to module name
type ModExportMap = Map.Map String String

-- Like ModExportMap but includes variables/functions that are not exported
type ModDefinitionMap = Map.Map String String

modDefMap :: CodeSystInfo -> Choices -> ModDefinitionMap
modDefMap cs@CSI {
  inputs = ins,
  extInputs = extIns,
  derivedInputs = ds,
  constants = cns
  } chs = Map.fromList $ concatMap mpair (mods cs)
  where mpair (Mod n _ fs) = map fname fs `zip` repeat n
                        ++ getDefInput chs ins
                        ++ getDefConstants chs cns
                        ++ getDefDerived chs ds
                        ++ getDefConstraints chs (getConstraints (cMap cs) 
                          ins)
                        ++ getDefInputFormat chs extIns
                        ++ getDefOutput (outputs cs)

modExportMap :: ModDefinitionMap -> Choices -> ModExportMap
modExportMap mdm chs = Map.difference mdm $ Map.fromList $ removeDefs 
  (inputModule chs) (inputStructure chs)
  where removeDefs Combined Bundled = zip ["get_input", "derived_values", 
          "input_constraints"] (repeat "InputParameters")
        removeDefs _ _ = []

fstdecl :: ChunkDB -> [FuncStmt] -> [CodeChunk]
fstdecl ctx fsts = nub (concatMap (fstvars ctx) fsts) \\ nub (concatMap (declared ctx) fsts) 
  where
    fstvars :: ChunkDB -> FuncStmt -> [CodeChunk]
    fstvars _  (FDec cch) = [cch]
    fstvars sm (FAsg cch e) = cch:codevars' e sm
    fstvars sm (FFor cch e fs) = delete cch $ nub (codevars' e sm ++ concatMap (fstvars sm) fs)
    fstvars sm (FWhile e fs) = codevars' e sm ++ concatMap (fstvars sm) fs
    fstvars sm (FCond e tfs efs) = codevars' e sm ++ concatMap (fstvars sm) tfs ++ concatMap (fstvars sm) efs
    fstvars sm (FRet e) = codevars' e sm
    fstvars sm (FTry tfs cfs) = concatMap (fstvars sm) tfs ++ concatMap (fstvars sm ) cfs
    fstvars _  (FThrow _) = [] -- is this right?
    fstvars _  FContinue = []
    fstvars sm (FProcCall _ l) = concatMap (`codevars` sm) l
    fstvars sm (FAppend a b) = nub (codevars a sm ++ codevars b sm)

    declared :: ChunkDB -> FuncStmt -> [CodeChunk]
    declared _  (FDec cch) = [cch]
    declared _  (FAsg _ _) = []
    declared sm (FFor _ _ fs) = concatMap (declared sm) fs
    declared sm (FWhile _ fs) = concatMap (declared sm) fs
    declared sm (FCond _ tfs efs) = concatMap (declared sm) tfs ++ concatMap (declared sm) efs
    declared _  (FRet _) = []
    declared sm (FTry tfs cfs) = concatMap (declared sm) tfs ++ concatMap (declared sm) cfs
    declared _  (FThrow _) = [] -- is this right?
    declared _  FContinue = []
    declared _  (FProcCall _ _) = []
    declared _  (FAppend _ _) = []
       
fname :: Func -> Name       
fname (FCD cd) = codeName cd
fname (FDef (FuncDef n _ _ _ _ _)) = n
fname (FData (FuncData n _ _)) = n 

prefixFunctions :: [Mod] -> [Mod]
prefixFunctions = map (\(Mod nm desc fs) -> Mod nm desc $ map pfunc fs)
  where pfunc f@(FCD _) = f
        pfunc (FData (FuncData n desc d)) = FData (FuncData (funcPrefix ++ n) 
          desc d)
        pfunc (FDef (FuncDef n desc a t rd f)) = FDef (FuncDef (funcPrefix ++ n)
          desc a t rd f)

getDerivedInputs :: [DataDefinition] -> [QDefinition] -> [Input] -> [Const] ->
  ChunkDB -> [QDefinition]
getDerivedInputs ddefs defs' ins cnsts sm  =
  let refSet = ins ++ map codeChunk cnsts
  in  if null ddefs then filter ((`subsetOf` refSet) . flip codevars sm . (^.equat)) defs'
      else filter ((`subsetOf` refSet) . flip codevars sm . (^.defnExpr)) (map qdFromDD ddefs)

type Known = CodeChunk
type Need  = CodeChunk

getExecOrder :: [Def] -> [Known] -> [Need] -> ChunkDB -> [Def]
getExecOrder d k' n' sm  = getExecOrder' [] d k' (n' \\ k')
  where getExecOrder' ord _ _ []   = ord
        getExecOrder' ord defs' k n = 
          let new  = filter ((`subsetOf` k) . flip codevars' sm . codeEquat) 
                defs'
              cnew = map codeChunk new
              kNew = k ++ cnew
              nNew = n \\ cnew
          in  if null new 
              then error ("Cannot find path from inputs to outputs: " ++
                        show (map (^. uid) n)
                        ++ " given Defs as " ++ show (map (^. uid) defs')
                        ++ " and Knowns as " ++ show (map (^. uid) k) )
              else getExecOrder' (ord ++ new) (defs' \\ new) kNew nNew
  
type ModDef = (String, String)

getDefInput :: Choices -> [Input] -> [ModDef]
getDefInput _ [] = []
getDefInput chs ins = inExp (inputModule chs) (inputStructure chs) 
  where inExp _ Unbundled = []
        inExp Separated Bundled = inVarDefs
        inExp Combined Bundled = (modName , modName) : inVarDefs 
        inVarDefs = map codeName ins `zip` repeat modName
        modName = "InputParameters"

getDefConstants :: Choices -> [Const] -> [ModDef]
getDefConstants _ [] = []
getDefConstants chs cs = cExp (constStructure chs) (inputStructure chs)
  where cExp (Store Bundled) _ = map codeName cs `zip` repeat "Constants"
        cExp WithInputs Bundled = map codeName cs `zip` repeat "InputParameters"
        cExp _ _ = []

getDefDerived :: Choices -> [Derived] -> [ModDef]
getDefDerived _ [] = []
getDefDerived chs _ = [("derived_values", dMod $ inputModule chs)]
  where dMod Combined = "InputParameters"
        dMod Separated = "DerivedValues"

getDefConstraints :: Choices -> [Constraint] -> [ModDef]
getDefConstraints _ [] = []
getDefConstraints chs _ = [("input_constraints", cMod $ inputModule chs)]
  where cMod Combined = "InputParameters"
        cMod Separated = "InputConstraints"
        
getDefInputFormat :: Choices -> [Input] -> [ModDef]
getDefInputFormat _ [] = []
getDefInputFormat chs _ = [("get_input", fMod $ inputModule chs)]
  where fMod Combined = "InputParameters"
        fMod Separated = "InputFormat"

getDefOutput :: [Output] -> [ModDef]
getDefOutput [] = []
getDefOutput _ = [("write_output", "OutputFormat")]

subsetOf :: (Eq a) => [a] -> [a] -> Bool
xs `subsetOf` ys = all (`elem` ys) xs

-- | Get a list of Constraints for a list of CodeChunks
getConstraints :: ConstraintMap -> [CodeChunk] -> [Constraint]
getConstraints cm cs = concat $ mapMaybe (\c -> Map.lookup (c ^. uid) cm) cs

-- | Get a list of CodeChunks from an equation
codevars :: Expr -> ChunkDB -> [CodeChunk]
codevars e m = map (varResolve m) $ dep e

-- | Get a list of CodeChunks from an equation (no functions)
codevars' :: Expr -> ChunkDB -> [CodeChunk]
codevars' e m = map (varResolve m) $ nub $ names' e

-- | Get a list of CodeChunks from an equation, where the CodeChunks are correctly parameterized by either Var or Func
codevarsandfuncs :: Expr -> ChunkDB -> ModExportMap -> [CodeChunk]
codevarsandfuncs e m mem = map resolve $ dep e
  where resolve x 
          | Map.member (funcPrefix ++ x) mem = funcResolve m x
          | otherwise = varResolve m x

-- | Get a list of CodeChunks from a constraint, where the CodeChunks are correctly parameterized by either Var or Func
constraintvarsandfuncs :: Constraint -> ChunkDB -> ModExportMap -> [CodeChunk]
constraintvarsandfuncs (Range _ ri) m mem = map resolve $ nub $ namesRI ri
  where resolve x 
          | Map.member (funcPrefix ++ x) mem = funcResolve m x
          | otherwise = varResolve m x
constraintvarsandfuncs _ _ _ = []

funcResolve, varResolve :: ChunkDB -> UID -> CodeChunk
funcResolve m x = quantfunc $ symbResolve m x
varResolve  m x = quantvar  $ symbResolve m x
