{-# LANGUAGE GADTs #-}
module Language.Drasil.CodeSpec where

import Language.Drasil
import Database.Drasil (ChunkDB, SystemInformation(SI), symbResolve,
  _authors, _constants, _constraints, _datadefs, _definitions, _inputs,
  _outputs, _sys, _sysinfodb, sampleData)
import Language.Drasil.Development (dep, namesRI)
import Theory.Drasil (DataDefinition, qdFromDD)

import Language.Drasil.Chunk.Code (CodeChunk, CodeVarChunk, CodeIdea(codeChunk),
  ConstraintMap, programName, quantvar, funcPrefix, codeName,
  codevars, codevars', funcResolve, varResolve, constraintMap)
import Language.Drasil.Chunk.CodeDefinition (CodeDefinition, qtov, qtoc, 
  codeEquat)
import Language.Drasil.Code.Code (spaceToCodeType)
import Language.Drasil.Mod (Class(..), Func(..), FuncData(..), FuncDef(..), 
  Mod(..), Name, fname, prefixFunctions)

import GOOL.Drasil (CodeType)

import Control.Lens ((^.))
import Data.List (nub, (\\))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

import Prelude hiding (const)

type Input = CodeVarChunk
type Output = CodeVarChunk
type Const = CodeDefinition
type Derived = CodeDefinition
type Def = CodeDefinition

data Lang = Cpp
          | CSharp
          | Java
          | Python
          deriving (Eq, Show)

data CodeSystInfo where
  CSI :: (HasName a) => {
  pName :: Name,
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
  eMap :: ModExportMap,
  clsMap :: ClassDefinitionMap,
  defList :: [Name],
  constMap :: ConstantMap,
  csi :: CodeSystInfo
  } -> CodeSpec

type ConstantMap = Map.Map String CodeDefinition

assocToMap :: HasUID a => [a] -> Map.Map UID a
assocToMap = Map.fromList . map (\x -> (x ^. uid, x))

codeSpec :: SystemInformation -> Choices -> [Mod] -> CodeSpec
codeSpec SI {_sys = sys
              , _authors = as
              , _definitions = defs'
              , _datadefs = ddefs
              , _inputs = ins
              , _outputs = outs
              , _constraints = cs
              , _constants = cnsts
              , _sysinfodb = db
              , sampleData = sd} chs ms = 
  let n = programName sys
      inputs' = map quantvar ins
      const' = map qtov (filter ((`Map.notMember` conceptMatch chs) . (^. uid)) 
        cnsts)
      derived = getDerivedInputs ddefs defs' inputs' const' db
      rels = map qtoc ((defs' ++ map qdFromDD ddefs) \\ derived)
      outs' = map quantvar outs
      allInputs = nub $ inputs' ++ map quantvar derived
      exOrder = getExecOrder rels (allInputs ++ map quantvar cnsts) outs' db
      mem = modExportMap csi' chs
      cdm = clsDefMap csi' chs
      csi' = CSI {
        pName = n,
        authors = as,
        inputs = allInputs,
        extInputs = inputs',
        derivedInputs = map qtov derived,
        outputs = outs',
        execOrder = exOrder,
        cMap = constraintMap cs,
        constants = const',
        mods = prefixFunctions ms,
        sysinfodb = db,
        smplData = sd
      }
  in  CodeSpec {
        eMap = mem,
        clsMap = cdm,
        defList = nub $ Map.keys mem ++ Map.keys cdm,
        constMap = assocToMap const',
        csi = csi'
      }

data Choices = Choices {
  lang :: [Lang],
  modularity :: Modularity,
  impType :: ImplementationType,
  logFile :: String,
  logging :: [Logging],
  comments :: [Comments],
  doxVerbosity :: Verbosity,
  dates :: Visibility,
  onSfwrConstraint :: ConstraintBehaviour,
  onPhysConstraint :: ConstraintBehaviour,
  inputStructure :: Structure,
  constStructure :: ConstantStructure,
  constRepr :: ConstantRepr,
  conceptMatch :: ConceptMatchMap,
  spaceMatch :: SpaceMatch,
  auxFiles :: [AuxFile],
  odeMethod :: [ODEMethod]
}

data Modularity = Modular InputModule | Unmodular

data ImplementationType = Library
                        | Program

-- Eq instances required for Logging and Comments because generator needs to 
-- check membership of these elements in lists
data Logging = LogFunc
             | LogVar deriving Eq
             
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
type SpaceMatch = Space -> [CodeType]
type MatchedSpaces = Space -> CodeType

data CodeConcept = Pi

matchConcepts :: (HasUID c) => [(c, [CodeConcept])] -> ConceptMatchMap
matchConcepts = Map.fromList . map (\(cnc,cdc) -> (cnc ^. uid, cdc))

matchSpace :: Space -> [CodeType] -> SpaceMatch -> SpaceMatch
matchSpace _ [] _ = error "Must match each Space to at least one CodeType"
matchSpace s ts sm = \sp -> if sp == s then ts else sm sp

matchSpaces :: [(Space, [CodeType])] -> SpaceMatch
matchSpaces spMtchs = matchSpaces' spMtchs spaceToCodeType 
  where matchSpaces' ((s,ct):sms) sm = matchSpaces' sms $ matchSpace s ct sm
        matchSpaces' [] sm = sm

data AuxFile = SampleInput deriving Eq
             
data Visibility = Show
                | Hide

data ODEMethod = RK45 | BDF | Adams

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
  logging = [],
  comments = [],
  doxVerbosity = Verbose,
  dates = Hide,
  onSfwrConstraint = Exception,
  onPhysConstraint = Warning,
  inputStructure = Bundled,
  constStructure = Inline,
  constRepr = Const,
  conceptMatch = matchConcepts ([] :: [(QDefinition, [CodeConcept])]),
  spaceMatch = spaceToCodeType, 
  auxFiles = [SampleInput],
  odeMethod = [RK45]
}

-- medium hacks ---
relToQD :: ExprRelat c => ChunkDB -> c -> QDefinition
relToQD sm r = convertRel sm (r ^. relat)

convertRel :: ChunkDB -> Expr -> QDefinition
convertRel sm (BinaryOp Eq (C x) r) = ec (symbResolve sm x) r
convertRel _ _ = error "Conversion failed"

asVC :: Func -> QuantityDict
asVC (FDef (FuncDef n _ _ _ _ _)) = implVar n (nounPhraseSP n) Real (Variable n)
asVC (FDef (CtorDef n _ _ _ _)) = implVar n (nounPhraseSP n) Real (Variable n)
asVC (FData (FuncData n _ _)) = implVar n (nounPhraseSP n) Real (Variable n)

funcUID :: Func -> UID
funcUID f = asVC f ^. uid

-- FIXME: hack. Use for implementation-stage functions that need to be displayed in the SRS.
funcUID' :: Func -> UID
funcUID' f = asVC' f ^. uid

-- FIXME: Part of above hack
asVC' :: Func -> QuantityDict
asVC' (FDef (FuncDef n _ _ _ _ _)) = vc n (nounPhraseSP n) (Variable n) Real
asVC' (FDef (CtorDef n _ _ _ _)) = vc n (nounPhraseSP n) (Variable n) Real
asVC' (FData (FuncData n _ _)) = vc n (nounPhraseSP n) (Variable n) Real

-- name of variable/function maps to module name
type ModExportMap = Map.Map String String

-- name of variable/function maps to class name
type ClassDefinitionMap = Map.Map String String

modExportMap :: CodeSystInfo -> Choices -> ModExportMap
modExportMap cs@CSI {
  pName = prn,
  inputs = ins,
  extInputs = extIns,
  derivedInputs = ds,
  constants = cns
  } chs@Choices {
    modularity = m
  } = Map.fromList $ concatMap mpair (mods cs)
    ++ getExpInput prn chs ins
    ++ getExpConstants prn chs cns
    ++ getExpDerived prn chs ds
    ++ getExpConstraints prn chs (getConstraints (cMap cs) ins)
    ++ getExpInputFormat prn chs extIns
    ++ getExpCalcs prn chs (execOrder cs)
    ++ getExpOutput prn chs (outputs cs)
  where mpair (Mod n _ _ cls fs) = (concatMap (map codeName . stateVars) cls ++ 
          map fname (fs ++ concatMap methods cls)) `zip` repeat (defModName m n)
        defModName Unmodular _ = prn
        defModName _ nm = nm

clsDefMap :: CodeSystInfo -> Choices -> ClassDefinitionMap
clsDefMap cs@CSI {
  inputs = ins,
  extInputs = extIns,
  derivedInputs = ds,
  constants = cns,
  mods = ms
  } chs = Map.fromList $ concatMap mclasses ms
    ++ getInputCls chs ins
    ++ getConstantsCls chs cns
    ++ getDerivedCls chs ds
    ++ getConstraintsCls chs (getConstraints (cMap cs) ins)
    ++ getInputFormatCls chs extIns
  where mclasses (Mod _ _ _ cls _) = concatMap (\c -> let cln = className c in
          map (svclass cln) (stateVars c) ++ map (mthclass cln) (methods c)) cls
        svclass cln sv = (codeName sv, cln)
        mthclass cln m = (fname m, cln)
        

-- Determines the derived inputs, which can be immediately calculated from the 
-- knowns (inputs and constants). If there are DDs, the derived inputs will 
-- come from those. If there are none, then the QDefinitions are used instead.
getDerivedInputs :: [DataDefinition] -> [QDefinition] -> [Input] -> [Const] ->
  ChunkDB -> [QDefinition]
getDerivedInputs ddefs defs' ins cnsts sm =
  let refSet = ins ++ map quantvar cnsts
  in  if null ddefs then filter ((`subsetOf` refSet) . flip codevars sm . (^.equat)) defs'
      else filter ((`subsetOf` refSet) . flip codevars sm . (^.defnExpr)) (map qdFromDD ddefs)

type Known = CodeVarChunk
type Need  = CodeVarChunk

getExecOrder :: [Def] -> [Known] -> [Need] -> ChunkDB -> [Def]
getExecOrder d k' n' sm  = getExecOrder' [] d k' (n' \\ k')
  where getExecOrder' ord _ _ []   = ord
        getExecOrder' ord defs' k n = 
          let new  = filter ((`subsetOf` k) . flip codevars' sm . codeEquat) 
                defs'
              cnew = map quantvar new
              kNew = k ++ cnew
              nNew = n \\ cnew
          in  if null new 
              then error ("Cannot find path from inputs to outputs: " ++
                        show (map (^. uid) n)
                        ++ " given Defs as " ++ show (map (^. uid) defs')
                        ++ " and Knowns as " ++ show (map (^. uid) k))
              else getExecOrder' (ord ++ new) (defs' \\ new) kNew nNew
  
type ModExp = (String, String)
type ClassDef = (String, String)

-- | If No inputs, no inputs variables are exported
-- If Unbundled, no input variables are exported
-- If Unmodular and Bundled, module is named after program
-- If Modular and Bundled, inports are exported by InputParameters module
-- In Unmodular Bundled and (Modular Combined) Bundled cases, an InputParameters
--   constructor is generated, thus "InputParameters" is added to map
getExpInput :: Name -> Choices -> [Input] -> [ModExp]
getExpInput _ _ [] = []
getExpInput prn chs ins = inExp (modularity chs) (inputStructure chs) 
  where inExp _ Unbundled = []
        inExp Unmodular Bundled = (ipName, prn) : inVarDefs prn
        inExp (Modular Separated) Bundled = inVarDefs ipName
        inExp (Modular Combined) Bundled = (ipName , ipName) : inVarDefs ipName
        inVarDefs n = map codeName ins `zip` repeat n
        ipName = "InputParameters"

-- | If no inputs, input variables not defined in any class
-- If Unbundled, input variables not defined in any class
-- If Bundled and input modules are Combined, input variables and input constructor are defined in InputParameters
-- If Bundled and input modules are Separated, input variables are defined in InputParameters but no constructor is generated
getInputCls :: Choices -> [Input] -> [ClassDef]
getInputCls _ [] = []
getInputCls chs ins = inCls (inputModule chs) (inputStructure chs) 
  where inCls _ Unbundled = []
        inCls Combined Bundled = (ipName, ipName) : inVarDefs
        inCls Separated Bundled = inVarDefs
        inVarDefs = map codeName ins `zip` repeat ipName
        ipName = "InputParameters"

-- | If no constants, no constants are exported
-- If Unmodular and Bundled, constants exported by module named after program
-- If Modular and Store Bundled, constants exported by Constants module
-- If Modular and WithInputs and inputs are Bundled, constants exported by InputParameters module
-- If Unbundled, constants are not exported by any module
getExpConstants :: Name -> Choices -> [Const] -> [ModExp]
getExpConstants _ _ [] = []
getExpConstants n chs cs = cExp (modularity chs) (constStructure chs) 
  (inputStructure chs)
  where cExp Unmodular (Store Bundled) _ = zipCs $ repeat n
        cExp Unmodular WithInputs Bundled = zipCs $ repeat n
        cExp _ (Store Bundled) _ = zipCs $ repeat "Constants"
        cExp _ WithInputs Bundled = zipCs $ repeat "InputParameters"
        cExp _ _ _ = []
        zipCs = zip (map codeName cs)

-- | If no constants, state variables for the constants are not defined in any class
-- If constants are Bundled, state variables for the constants are in Constants
-- If constants are Bundled WithInputs, state variables for the constants are in InputParameters
-- If constants are Unbundled, state variables for the constants are not defined in any class
getConstantsCls :: Choices -> [Const] -> [ClassDef]
getConstantsCls _ [] = []
getConstantsCls chs cs = cnCls (constStructure chs) (inputStructure chs)
  where cnCls (Store Bundled) _ = zipCs $ repeat "Constants"
        cnCls WithInputs Bundled = zipCs $ repeat "InputParameters"
        cnCls _ _ = []
        zipCs = zip (map codeName cs)

-- | If no derived inputs, no derived inputs function is generated
-- If input modules are separated, derived_values will always be exported.
-- If input modules are combined and inputs are bundled, derived_values will be a private method, not exported
-- If input modules are combined and inputs are unbundled, derived_values will be exported.
-- Similar logic for input_constraints and get_input below
getExpDerived :: Name -> Choices -> [Derived] -> [ModExp]
getExpDerived _ _ [] = []
getExpDerived n chs _ = dMod (modularity chs) (inputStructure chs)
  where dMod (Modular Separated) _ = [(dvNm, "DerivedValues")]
        dMod _ Bundled = []
        dMod Unmodular _ = [(dvNm, n)]
        dMod (Modular Combined) _ = [(dvNm, "InputParameters")]
        dvNm = "derived_values"

-- | If no derived inputs, derived_values is not defined in any class
-- If input modules are Combined and inputs are Bundled, derived_values is defined in InputParameters class
-- Otherwise, derived_values is not defined in any class
-- Similar logic for input_constraints and get_input below.
getDerivedCls :: Choices -> [Derived] -> [ClassDef]
getDerivedCls _ [] = []
getDerivedCls chs _ = dCls (inputModule chs) (inputStructure chs)
  where dCls Combined Bundled = [("derived_values", "InputParameters")]
        dCls _ _ = []

getExpConstraints :: Name -> Choices -> [Constraint] -> [ModExp]
getExpConstraints _ _ [] = []
getExpConstraints n chs _ = cMod (modularity chs) (inputStructure chs)
  where cMod (Modular Separated) _ = [(icNm, "InputConstraints")]
        cMod _ Bundled = []
        cMod Unmodular _ = [(icNm, n)]
        cMod (Modular Combined) _ = [(icNm, "InputParameters")]
        icNm = "input_constraints"

getConstraintsCls :: Choices -> [Constraint] -> [ClassDef]
getConstraintsCls _ [] = []
getConstraintsCls chs _ = cCls (inputModule chs) (inputStructure chs)
  where cCls Combined Bundled = [("input_constraints", "InputParameters")]
        cCls _ _ = []
        
getExpInputFormat :: Name -> Choices -> [Input] -> [ModExp]
getExpInputFormat _ _ [] = []
getExpInputFormat n chs _ = fMod (modularity chs) (inputStructure chs)
  where fMod (Modular Separated) _ = [(giNm, "InputFormat")]
        fMod _ Bundled = []
        fMod Unmodular _ = [(giNm, n)]
        fMod (Modular Combined) _ = [(giNm, "InputParameters")]
        giNm = "get_input"

getInputFormatCls :: Choices -> [Input] -> [ClassDef]
getInputFormatCls _ [] = []
getInputFormatCls chs _ = ifCls (inputModule chs) (inputStructure chs)
  where ifCls Combined Bundled = [("get_input", "InputParameters")]
        ifCls _ _ = []

-- | Functions are exported by module named after program if Unmodular
-- Function is exported by Calculations module if Modular
getExpCalcs :: Name -> Choices -> [Def] -> [ModExp]
getExpCalcs n chs = map (\d -> (codeName d, calMod))
  where calMod = cMod $ modularity chs
        cMod Unmodular = n
        cMod _ = "Calculations"

-- | No output function is exported if there are no outputs.
-- Function is exported by module named after program if Unmodular
-- Function is exported by OutputFormat module if Modular
getExpOutput :: Name -> Choices -> [Output] -> [ModExp]
getExpOutput _ _ [] = []
getExpOutput n chs _ = [("write_output", oMod $ modularity chs)]
  where oMod Unmodular = n
        oMod _ = "OutputFormat"

subsetOf :: (Eq a) => [a] -> [a] -> Bool
xs `subsetOf` ys = all (`elem` ys) xs

-- | Get a list of Constraints for a list of CodeChunks
getConstraints :: (HasUID c) => ConstraintMap -> [c] -> [Constraint]
getConstraints cm cs = concat $ mapMaybe (\c -> Map.lookup (c ^. uid) cm) cs

-- | Get a list of CodeChunks from an equation, where the CodeChunks are correctly parameterized by either Var or Func
codevarsandfuncs :: Expr -> ChunkDB -> ModExportMap -> [CodeChunk]
codevarsandfuncs e m mem = map resolve $ dep e
  where resolve x 
          | Map.member (funcPrefix ++ x) mem = codeChunk $ funcResolve m x
          | otherwise = codeChunk $ varResolve m x

-- | Get a list of CodeChunks from a constraint, where the CodeChunks are correctly parameterized by either Var or Func
constraintvarsandfuncs :: Constraint -> ChunkDB -> ModExportMap -> [CodeChunk]
constraintvarsandfuncs (Range _ ri) m mem = map resolve $ nub $ namesRI ri
  where resolve x 
          | Map.member (funcPrefix ++ x) mem = codeChunk $ funcResolve m x
          | otherwise = codeChunk $ varResolve m x
constraintvarsandfuncs _ _ _ = []
