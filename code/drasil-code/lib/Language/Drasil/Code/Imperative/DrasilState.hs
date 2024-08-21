{-# LANGUAGE TemplateHaskell, TupleSections #-}
module Language.Drasil.Code.Imperative.DrasilState (
  GenState, DrasilState(..), designLog, MatchedSpaces, ModExportMap,
  ClassDefinitionMap, ScopeType(..), modExportMap, clsDefMap, addToDesignLog,
  addLoggedSpace, genICName
) where

import Language.Drasil
import Drasil.GOOL (VisibilityTag(..), CodeType)

import Data.Containers.ListUtils (nubOrd)

import Language.Drasil.Chunk.ConstraintMap (ConstraintCE)
import Language.Drasil.Code.ExtLibImport (ExtLibState)
import Language.Drasil.Choices (Choices(..), Architecture (..), DataInfo(..),
  AuxFile, Modularity(..), ImplementationType(..), Comments, Verbosity,
  MatchedConceptMap, ConstantRepr, ConstantStructure(..), ConstraintBehaviour, Logging, 
  Structure(..), InternalConcept(..))
import Language.Drasil.CodeSpec (Input, Const, Derived, Output, Def,
  CodeSpec(..),  getConstraints)
import Language.Drasil.Mod (Mod(..), Name, Version, Class(..),
  StateVariable(..), fname)

import Control.Lens ((^.), makeLenses, over)
import Control.Monad.State (State, gets)
import Data.List (nub)
import Data.Set (Set)
import Data.Map (Map, fromList)
import Text.PrettyPrint.HughesPJ (Doc, ($$))

-- | Type for the mapping between 'Space's and 'CodeType's.
type MatchedSpaces = Space -> GenState CodeType

-- | Map from calculation function name to the 'ExtLibState' containing the contents of the function.
type ExtLibMap = Map String ExtLibState

-- | Variable/function name maps to module name.
type ModExportMap = Map String String

-- | Variable/function name maps to class name.
type ClassDefinitionMap = Map String String

-- | Variable scope
data ScopeType = Local | Global | MainFn

-- | Abbreviation used throughout generator.
type GenState = State DrasilState

-- | Private State, used to push these options around the generator.
data DrasilState = DrasilState {
  codeSpec :: CodeSpec,
  -- Choices
  modular :: Modularity,
  implType :: ImplementationType,
  inStruct :: Structure,
  conStruct :: ConstantStructure,
  conRepr :: ConstantRepr,
  concMatches :: MatchedConceptMap,
  spaceMatches :: MatchedSpaces,
  onSfwrC :: ConstraintBehaviour,
  onPhysC :: ConstraintBehaviour,
  commented :: [Comments],
  doxOutput :: Verbosity,
  date :: String,
  logName :: String,
  logKind :: [Logging],
  auxiliaries :: [AuxFile],
  sampleData :: [Expr],
  dsICNames :: InternalConcept -> Name,
  -- Reference materials
  modules :: [Mod],
  extLibNames :: [(Name,Version)],
  extLibMap :: ExtLibMap,
  libPaths :: [FilePath],
  eMap :: ModExportMap,
  libEMap :: ModExportMap,
  clsMap :: ClassDefinitionMap,
  defSet :: Set Name,
  getVal :: Int,

  -- Stateful
  currentModule :: String,
  currentClass :: String,
  _designLog :: Doc,
  _loggedSpaces :: [(Space, CodeType)],
  currentScope :: ScopeType
}
makeLenses ''DrasilState

-- | Adds a message to the design log if the given 'Space'-'CodeType' match has not
-- already been logged.
addToDesignLog :: Space -> CodeType -> Doc -> DrasilState -> DrasilState
addToDesignLog s t l ds = if (s,t) `elem` (ds ^. loggedSpaces) then ds
  else over designLog ($$ l) ds

-- | Adds a 'Space'-'CodeType' pair to the loggedSpaces list in 'DrasilState' to prevent a duplicate
-- log from being generated for that 'Space'-'CodeType' pair.
addLoggedSpace :: Space -> CodeType -> DrasilState -> DrasilState
addLoggedSpace s t = over loggedSpaces ((s,t):)

-- | Builds the module export map, mapping each function and state variable name
-- in the generated code to the name of the generated module that exports it.
modExportMap :: CodeSpec -> Choices -> [Mod] -> ModExportMap
modExportMap cs@CodeSpec {
  pName = prn,
  inputs = ins,
  extInputs = extIns,
  derivedInputs = ds,
  constants = cns
  } chs@Choices {
    architecture = m
  } ms = fromList $ nubOrd $ concatMap mpair ms
    ++ getExpInput prn chs ins
    ++ getExpConstants prn chs cns
    ++ getExpDerived prn chs ds
    ++ getExpConstraints prn chs (getConstraints (cMap cs) ins)
    ++ getExpInputFormat prn chs extIns
    ++ getExpCalcs prn chs (execOrder cs)
    ++ getExpOutput prn chs (outputs cs)
  where mpair (Mod n _ _ cls fs) = map
          (, defModName (modularity m) n)
          (map className cls
            ++ concatMap (map (codeName . stVar) . filter ((== Pub) . svVisibility) . stateVars) cls
            ++ map fname (fs ++ concatMap methods cls))
        defModName Unmodular _ = prn
        defModName _ nm = nm

-- | Builds the class definition map, mapping each generated method and state
-- variable name to the name of the generated class where it is defined.
clsDefMap :: CodeSpec -> Choices -> [Mod] -> ClassDefinitionMap
clsDefMap cs@CodeSpec {
  inputs = ins,
  extInputs = extIns,
  derivedInputs = ds,
  constants = cns
  } chs ms = fromList $ nub $ concatMap modClasses ms
    ++ getInputCls chs ins
    ++ getConstantsCls chs cns
    ++ getDerivedCls chs ds
    ++ getConstraintsCls chs (getConstraints (cMap cs) ins)
    ++ getInputFormatCls chs extIns
    where modClasses (Mod _ _ _ cls _) = concatMap (\cl ->
            let cln = className cl in
            (cln, cln) : map (\sv -> (codeName (stVar sv), cln)) (stateVars cl)
              ++ map (\m -> (fname m, cln)) (methods cl)) cls

-- | Module exports.
type ModExp = (String, String)
-- | Class definitions.
type ClassDef = (String, String)

-- | Gets exported inputs for InputParameters module.
-- If there are no inputs, no input variables are exported.
-- If 'Unbundled', no input variables are exported.
-- If 'Unmodular' and 'Bundled', module is named after program.
-- If 'Modular' and 'Bundled', inputs are exported by InputParameters module.
-- In 'Unmodular' 'Bundled' and 'Modular' 'Bundled' cases, an InputParameters
-- constructor is generated, thus "InputParameters" is added to map.
getExpInput :: Name -> Choices -> [Input] -> [ModExp]
getExpInput _ _ [] = []
getExpInput prn chs ins = inExp (modularity $ architecture chs) (inputStructure $ dataInfo chs)
  where inExp _ Unbundled = []
        inExp Unmodular Bundled = (ipName, prn) : inVarDefs prn
        inExp Modular Bundled = (ipName , ipName) : inVarDefs ipName
        inVarDefs n = map ((, n) . codeName) ins
        ipName = icNames chs InputParameters

-- | Gets input variables for classes for InputParameters module.
-- If no inputs, input variables will not be defined in any class.
-- If 'Unbundled', input variables will not be defined in any class.
-- If 'Bundled', input variables and input constructor are defined in InputParameters.
getInputCls :: Choices -> [Input] -> [ClassDef]
getInputCls _ [] = []
getInputCls chs ins = inCls (inputStructure $ dataInfo chs)
  where inCls Unbundled = []
        inCls Bundled = (ipName, ipName) : inVarDefs
        inVarDefs = map ((, ipName) . codeName) ins
        ipName = icNames chs InputParameters

-- | Gets constants to be exported for InputParameters or Constants module.
-- If there are no constants, constants will not be exported.
-- If 'Unmodular' and 'Bundled', constants will be exported by the module named after the program.
-- If 'Modular' and 'Store' 'Bundled', constants will be exported by the Constants module.
-- If 'Modular' 'WithInputs' and inputs are 'Bundled', constants will be exported by the InputParameters module.
-- If 'Unbundled', constants are not exported by any module.
getExpConstants :: Name -> Choices -> [Const] -> [ModExp]
getExpConstants _ _ [] = []
getExpConstants n chs cs = cExp (modularity $ architecture chs) (constStructure $ dataInfo chs)
  (inputStructure $ dataInfo chs)
  where cExp Unmodular (Store Bundled) _ = zipCs $ repeat n
        cExp Unmodular WithInputs Bundled = zipCs $ repeat n
        cExp _ (Store Bundled) _ = zipCs $ repeat (icNames chs Constants)
        cExp _ WithInputs Bundled = zipCs $ repeat (icNames chs InputParameters)
        cExp _ _ _ = []
        zipCs = zip (map codeName cs)

-- | Gets state variables for constants in a class for InputParameters or Constants module.
-- If there are no constants, state variables for the constants are not defined in any class.
-- If constants are 'Bundled', state variables for the constants are in Constants.
-- If constants are 'Bundled' 'WithInputs', state variables for the constants are in InputParameters.
-- If constants are 'Unbundled', state variables for the constants are not defined in any class.
getConstantsCls :: Choices -> [Const] -> [ClassDef]
getConstantsCls _ [] = []
getConstantsCls chs cs = cnCls (constStructure $ dataInfo chs) (inputStructure $ dataInfo chs)
  where cnCls (Store Bundled) _ = zipCs Constants
        cnCls WithInputs Bundled = zipCs InputParameters
        cnCls _ _ = []
        zipCs ic = map ((, icNames chs ic) . codeName) cs

-- | Get derived input functions (for @derived_values@).
-- If there are no derived inputs, a derived inputs function is not generated.
-- If inputs are 'Bundled', derived_values will be a private method, not exported.
-- If inputs are 'Unbundled', derived_values will be exported.
-- Similar logic for input_constraints and get_input below.
getExpDerived :: Name -> Choices -> [Derived] -> [ModExp]
getExpDerived _ _ [] = []
getExpDerived n chs _ = dMod (modularity $ architecture chs) (inputStructure $ dataInfo chs)
  where dMod _ Bundled = []
        dMod Unmodular _ = [(dvNm, n)]
        dMod Modular _ = [(dvNm, icNames chs InputParameters)]
        dvNm = icNames chs DerivedValuesFn

-- | Get derived values defined in a class (for @derived_values@).
-- If there are no derived inputs, derived_values is not defined in any class.
-- If inputs are 'Bundled', derived_values is defined in an InputParameters class.
-- Otherwise, derived_values is not defined in any class.
-- Similar logic for input_constraints and get_input below.
getDerivedCls :: Choices -> [Derived] -> [ClassDef]
getDerivedCls _ [] = []
getDerivedCls chs _ = dCls (inputStructure $ dataInfo chs)
  where dCls Bundled = [(icNames chs DerivedValuesFn, icNames chs InputParameters)]
        dCls _ = []

-- | Get input constraints to be exported (for @input_constraints@).
-- See 'getExpDerived' for full logic details.
getExpConstraints :: Name -> Choices -> [ConstraintCE] -> [ModExp]
getExpConstraints _ _ [] = []
getExpConstraints n chs _ = cMod (modularity $ architecture chs) (inputStructure $ dataInfo chs)
  where cMod _ Bundled = []
        cMod Unmodular _ = [(icNm, n)]
        cMod Modular _ = [(icNm, icNames chs InputParameters)]
        icNm =  icNames chs InputConstraintsFn

-- | Get constraints defined in a class (for @input_constraints@).
-- See 'getDerivedCls' for full logic details.
getConstraintsCls :: Choices -> [ConstraintCE] -> [ClassDef]
getConstraintsCls _   [] = []
getConstraintsCls chs _  = cCls (inputStructure $ dataInfo chs)
  where cCls Bundled = [(icNames chs InputConstraintsFn, icNames chs InputParameters)]
        cCls _ = []

-- | Get input format to be exported (for @get_input@).
-- See 'getExpDerived' for full logic details.
getExpInputFormat :: Name -> Choices -> [Input] -> [ModExp]
getExpInputFormat _ _ [] = []
getExpInputFormat n chs _ = fMod (modularity $ architecture chs) (inputStructure $ dataInfo chs)
  where fMod _ Bundled = []
        fMod Unmodular _ = [(giNm, n)]
        fMod Modular _ = [(giNm, icNames chs InputParameters)]
        giNm = icNames chs GetInput

-- | Get input format defined in a class (for @get_input@).
-- See 'getDerivedCls' for full logic details.
getInputFormatCls :: Choices -> [Input] -> [ClassDef]
getInputFormatCls _ [] = []
getInputFormatCls chs _ = ifCls (inputStructure $ dataInfo chs)
  where ifCls Bundled = [(icNames chs GetInput, icNames chs InputParameters)]
        ifCls _ = []

-- | Gets exported calculations.
-- Functions are exported by module named after program if 'Unmodular'.
-- Function is exported by Calculations module if program is 'Modular'.
getExpCalcs :: Name -> Choices -> [Def] -> [ModExp]
getExpCalcs n chs = map (\d -> (codeName d, calMod))
  where calMod = cMod $ modularity $ architecture chs
        cMod Unmodular = n
        cMod _ = icNames chs Calculations

-- | Get exported outputs (for @write_output@).
-- No output function is exported if there are no outputs.
-- Function is exported by module named after program if 'Unmodular'.
-- Function is exported by OutputFormat module if program is 'Modular'.
getExpOutput :: Name -> Choices -> [Output] -> [ModExp]
getExpOutput _ _ [] = []
getExpOutput n chs _ = [(icNames chs WriteOutput, oMod $ modularity $ architecture chs)]
  where oMod Unmodular = n
        oMod _ = icNames chs OutputFormat

-- | Get InternalConcept name using DrasilState
genICName :: InternalConcept -> GenState Name
genICName ic = gets (`dsICNames` ic)
