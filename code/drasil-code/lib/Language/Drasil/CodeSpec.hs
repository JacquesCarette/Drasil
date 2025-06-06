{-# LANGUAGE GADTs, TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
-- | Defines the CodeSpec structure and related functions.
module Language.Drasil.CodeSpec where

import Language.Drasil hiding (None, new)
import Language.Drasil.Display (Symbol(Variable))
import Database.Drasil
import qualified System.Drasil as SI
import System.Drasil (HasSystem(..))

import Theory.Drasil (DataDefinition, qdEFromDD, getEqModQdsFromIm)

import Language.Drasil.Chunk.ConstraintMap (ConstraintCEMap, ConstraintCE, constraintMap)
import Language.Drasil.Chunk.CodeDefinition (CodeDefinition, qtov, qtoc, odeDef,
  auxExprs)
import Language.Drasil.Choices (Choices(..), Maps(..), ODE(..), ExtLib(..))
import Language.Drasil.CodeExpr.Development (expr, eNamesRI, eDep)
import Language.Drasil.Chunk.CodeBase
import Language.Drasil.Mod (Func(..), FuncData(..), FuncDef(..), Mod(..), Name)

import Utils.Drasil (subsetOf)

import Control.Lens ((^.), makeLenses, Lens', makeClassyFor)
import Data.List (intercalate, nub, (\\))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

import Prelude hiding (const)

-- | Program input.
type Input = CodeVarChunk
-- | Program output.
type Output = CodeVarChunk
-- | Constants in the problem.
type Const = CodeDefinition
-- | Derived inputs.
type Derived = CodeDefinition
-- | Mathematical definition.
type Def = CodeDefinition
-- | Maps constants to their respective 'CodeDefinition'.
type ConstantMap = Map.Map UID CodeDefinition

-- | Old Code specifications. Holds information needed to generate code.
data OldCodeSpec = OldCodeSpec {
  -- | Program name.
  _pName :: Name,
  -- | Authors.
  _authors :: People,
  -- | All inputs.
  _inputs :: [Input],
  -- | Explicit inputs (values to be supplied by a file).
  _extInputs :: [Input],
  -- | Derived inputs (each calculated from explicit inputs in a single step).
  _derivedInputs :: [Derived],
  -- | All outputs.
  _outputs :: [Output],
  -- | List of files that must be in same directory for running the executable.
  _configFiles :: [FilePath],
  -- | Mathematical definitions, ordered so that they form a path from inputs to
  -- outputs.
  _execOrder :: [Def],
  -- | Map from 'UID's to constraints for all constrained chunks used in the problem.
  _cMap :: ConstraintCEMap,
  -- | List of all constants used in the problem.
  _constants :: [Const],
  -- | Map containing all constants used in the problem.
  _constMap :: ConstantMap,
  -- | Additional modules required in the generated code, which Drasil cannot yet
  -- automatically define.
  _mods :: [Mod],  -- medium hack
  -- | The database of all chunks used in the problem.
  _systemdb :: ChunkDB
  }

makeClassyFor "HasOldCodeSpec" "oldCodeSpec"
  [   ("_pName", "pNameO")
    , ("_authors", "authorsO")
    , ("_inputs", "inputsO")
    , ("_extInputs", "extInputsO")
    , ("_derivedInputs", "derivedInputsO")
    , ("_outputs", "outputsO")
    , ("_configFiles", "configFilesO") 
    , ("_execOrder", "execOrderO")
    , ("_cMap", "cMapO")
    , ("_constants", "constantsO")
    , ("_constMap", "constMapO")
    , ("_mods", "modsO")
    , ("_systemdb", "systemdbO")
    ] ''OldCodeSpec

-- | New Code Specification. Holds system information and a reference to `OldCodeSpec`.
data CodeSpec = CS {
  _system' :: SI.System,
  _oldCode :: OldCodeSpec
}
makeLenses ''CodeSpec

instance HasSystem CodeSpec where
  system :: Lens' CodeSpec SI.System
  system = system'
  background :: Lens' CodeSpec SI.Background
  background = system . SI.background
  purpose :: Lens' CodeSpec SI.Purpose
  purpose = system . SI.purpose
  scope :: Lens' CodeSpec SI.Scope
  scope = system . SI.scope
  motivation :: Lens' CodeSpec SI.Motivation
  motivation = system . SI.motivation

instance HasOldCodeSpec CodeSpec where
  oldCodeSpec :: Lens' CodeSpec OldCodeSpec
  oldCodeSpec = oldCode

  pNameO :: Lens' CodeSpec Name
  pNameO = oldCode . pNameO

  authorsO :: Lens' CodeSpec People
  authorsO = oldCode . authorsO

  inputsO :: Lens' CodeSpec [Input]
  inputsO = oldCode . inputsO

  extInputsO :: Lens' CodeSpec [Input]
  extInputsO = oldCode . extInputsO

  derivedInputsO :: Lens' CodeSpec [Derived]
  derivedInputsO = oldCode . derivedInputsO

  outputsO :: Lens' CodeSpec [Output]
  outputsO = oldCode . outputsO

  configFilesO :: Lens' CodeSpec [FilePath]
  configFilesO = oldCode . configFilesO

  execOrderO :: Lens' CodeSpec [Def]
  execOrderO = oldCode . execOrderO

  cMapO :: Lens' CodeSpec ConstraintCEMap
  cMapO = oldCode . cMapO

  constantsO :: Lens' CodeSpec [Const]
  constantsO = oldCode . constantsO

  constMapO :: Lens' CodeSpec ConstantMap
  constMapO = oldCode . constMapO

  modsO :: Lens' CodeSpec [Mod]
  modsO = oldCode . modsO

  systemdbO :: Lens' CodeSpec ChunkDB
  systemdbO = oldCode . systemdbO

-- | Converts a list of chunks that have 'UID's to a Map from 'UID' to the associated chunk.
assocToMap :: HasUID a => [a] -> Map.Map UID a
assocToMap = Map.fromList . map (\x -> (x ^. uid, x))

-- | Get ODE from ExtLib
getODE :: [ExtLib] -> Maybe ODE
getODE [] = Nothing
getODE (Math ode: _) = Just ode
-- getODE (_:xs) = getODE xs

-- | Maps ODE to their respective 'CodeDefinition'.
mapODE :: Maybe ODE -> [CodeDefinition]
mapODE Nothing = []
mapODE (Just ode) = map odeDef $ odeInfo ode

-- | Creates a 'CodeSpec' using the provided 'System', 'Choices', and 'Mod's.
-- The 'CodeSpec' consists of the system information and a corresponding 'OldCodeSpec'.
codeSpec :: SI.System -> Choices -> [Mod] -> CodeSpec
codeSpec si chs ms = CS {
  _system' = si,
  _oldCode = oldcodeSpec si chs ms
}

-- | Generates an 'OldCodeSpec' from 'System', 'Choices', and a list of 'Mod's.
-- This function extracts various components (e.g., inputs, outputs, constraints, etc.)
-- from 'System' to populate the 'OldCodeSpec' structure.
oldcodeSpec :: SI.System -> Choices -> [Mod] -> OldCodeSpec
oldcodeSpec SI.SI{ SI._sys = sys
                   , SI._authors = as
                   , SI._instModels = ims
                   , SI._datadefs = ddefs
                   , SI._configFiles = cfp
                   , SI._inputs = ins
                   , SI._outputs = outs
                   , SI._constraints = cs
                   , SI._constants = cnsts
                   , SI._systemdb = db } chs ms =
  let n = programName sys
      inputs' = map quantvar ins
      const' = map qtov (filter ((`Map.notMember` conceptMatch (maps chs)) . (^. uid))
        cnsts)
      derived = map qtov $ getDerivedInputs ddefs inputs' const' db
      rels = (map qtoc (getEqModQdsFromIm ims ++ mapMaybe qdEFromDD ddefs)) \\ derived
        ++ mapODE (getODE $ extLibs chs)
      -- TODO: When we have better DEModels, we should be deriving our ODE information
      --       directly from the instance models (ims) instead of directly from the choices.
      outs' = map quantvar outs
      allInputs = nub $ inputs' ++ map quantvar derived
      exOrder = getExecOrder rels (allInputs ++ map (quantvar . (^. defLhs)) cnsts) outs' db
  in OldCodeSpec {
        _pName = n,
        _authors = as,
        _inputs = allInputs,
        _extInputs = inputs',
        _derivedInputs = derived,
        _outputs = outs',
        _configFiles = cfp,
        _execOrder = exOrder,
        _cMap = constraintMap cs,
        _constants = const',
        _constMap = assocToMap const',
        _mods = ms,
        _systemdb = db
      } 


-- medium hacks ---

-- | Convert a 'Func' to an implementation-stage 'QuantityDict' representing the
-- function.
asVC :: Func -> QuantityDict
asVC (FDef (FuncDef n _ _ _ _ _)) = implVar n (nounPhraseSP n) Real (Variable n)
asVC (FDef (CtorDef n _ _ _ _))   = implVar n (nounPhraseSP n) Real (Variable n)
asVC (FData (FuncData n _ _))     = implVar n (nounPhraseSP n) Real (Variable n)

-- | Get a 'UID' of a chunk corresponding to a 'Func'.
funcUID :: Func -> UID
funcUID f = asVC f ^. uid

-- | Determines the derived inputs, which can be immediately calculated from the
-- knowns (inputs and constants). If there are DDs, the derived inputs will
-- come from those. If there are none, then the 'QDefinition's are used instead.
getDerivedInputs :: [DataDefinition] -> [Input] -> [Const] ->
  ChunkDB -> [SimpleQDef]
getDerivedInputs ddefs ins cnsts sm =
  filter ((`subsetOf` refSet) . flip codevars sm . expr . (^. defnExpr)) (mapMaybe qdEFromDD ddefs)
  where refSet = ins ++ map quantvar cnsts

-- | Known values.
type Known = CodeVarChunk
-- | Calculated values.
type Need  = CodeVarChunk

-- | Orders a list of definitions such that they form a path between 'Known' values
-- and values that 'Need' to be calculated.
getExecOrder :: [Def] -> [Known] -> [Need] -> ChunkDB -> [Def]
getExecOrder d k' n' sm  = getExecOrder' [] d k' (n' \\ k')
  where getExecOrder' ord _ _ []   = ord
        getExecOrder' ord defs' k n =
          let new  = filter (\def -> (`subsetOf` k) (concatMap (`codevars'` sm)
                (def ^. codeExpr : def ^. auxExprs) \\ [quantvar def])) defs'
              cnew = map quantvar new
              kNew = k ++ cnew
              nNew = n \\ cnew
          in  if null new
              then error ("The following outputs cannot be computed: " ++
                       intercalate ", " (map showUID n) ++ "\n"
                     ++ "Unused definitions are: "
                       ++ intercalate ", " (map showUID defs') ++ "\n"
                     ++ "Known values are: "
                       ++ intercalate ", " (map showUID k))
              else getExecOrder' (ord ++ new) (defs' \\ new) kNew nNew


-- | Get a list of 'Constraint's for a list of 'CodeChunk's.
getConstraints :: (HasUID c) => ConstraintCEMap -> [c] -> [ConstraintCE]
getConstraints cm cs = concat $ mapMaybe (\c -> Map.lookup (c ^. uid) cm) cs

-- | Get a list of 'CodeChunk's from a constraint.
constraintvars :: ConstraintCE -> ChunkDB -> [CodeChunk]
constraintvars (Range _ ri) m =
  map (codeChunk . varResolve m) $ nub $ eNamesRI ri
constraintvars (Elem _ ri) m =
  map (codeChunk . varResolve m) $ eDep ri
