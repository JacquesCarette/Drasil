{-# LANGUAGE TemplateHaskell #-}
-- | Defines the CodeSpec structure and related functions.
module Language.Drasil.CodeSpec (
  -- * Types
  Input, Output, Const, Derived, ConstantMap,
  CodeSpec,
  -- * Typeclasses
  HasCodeSpec(..),
  -- * Constructors
  mkCodeSpec,
  -- * ODEs
  getODE, mapODE,
  -- * Hacks
  asVC, funcUID, getDerivedInputs, getConstraints, constraintvars
) where

import Prelude hiding (const)
import Control.Lens ((^.), makeClassy, set)
import Data.List (nub, (\\))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.List.NonEmpty as NE

import Drasil.FileHandling.Legacy (RelativeFile)
import Language.Drasil hiding (None)
import Drasil.Database (ChunkDB, UID, HasUID(..), insertAll, mkUid, HasChunkRefs(..))
import Drasil.Code.CodeExpr.Development (expr, eNamesRI, eDep)
import qualified Drasil.SRS as S
import Drasil.System (HasSystemMeta(..), systemdb)
import Drasil.SRS (HasSmithEtAlSRS(..))
import Theory.Drasil (DataDefinition, qdEFromDD, getEqModQdsFromIm)
import Data.List.Extras (subsetOf)

import Drasil.Code.CodeVar (CodeVarChunk, quantvar)
import Language.Drasil.Chunk.ConstraintMap (ConstraintCEMap, ConstraintCE, constraintMap)
import Language.Drasil.Chunk.CodeDefinition (CodeDefinition, qtov, qtoc, odeDef)
import Language.Drasil.Choices (Choices(..), Maps(..), ODE(..), ExtLib(..),
  odeLibReqs, odeInfoReqs)
import Language.Drasil.Chunk.CodeBase (codevars, varResolve)
import Language.Drasil.Mod (Func(..), FuncData(..), FuncDef(..), Mod(..))
import Language.Drasil.ICOSolutionSearch (Def, solveExecOrder)

-- | Program input.
type Input = CodeVarChunk
-- | Program output.
type Output = CodeVarChunk
-- | Constants in the problem.
type Const = CodeDefinition
-- | Derived inputs.
type Derived = CodeDefinition
-- | Maps constants to their respective 'CodeDefinition'.
type ConstantMap = Map.Map UID CodeDefinition

-- | Code Specification. Holds system information and options.
data CodeSpec = CS {
  _srs :: S.SmithEtAlSRS,
  -- | All inputs.
  _inputs :: [Input],
  -- | Explicit inputs (values to be supplied by a file).
  _extInputs :: [Input],
  -- | Derived inputs (each calculated from explicit inputs in a single step).
  _derivedInputs :: [Derived],
  -- | All outputs.
  _outputs :: [Output],
  -- | List of files that must be in same directory for running the executable.
  _configFiles :: [RelativeFile],
  -- | Mathematical definitions, ordered so that they form a path from inputs to
  -- outputs.
  _execOrder :: [Def],
  -- | Map from 'UID's to constraints for all constrained chunks used in the problem.
  _cMap :: ConstraintCEMap,
  -- | List of all constants used in the problem.
  _constDefns :: [Const],
  -- | Map containing all constants used in the problem.
  _constMap :: ConstantMap,
  -- | Additional modules required in the generated code, which Drasil cannot yet
  -- automatically define.
  _mods :: [Mod]  -- medium hack
}
makeClassy ''CodeSpec

instance HasSmithEtAlSRS CodeSpec where
  smithEtAlSRS = srs

instance HasSystemMeta CodeSpec where
  systemMeta = srs . systemMeta

instance HasUID CodeSpec where
  uid = srs . uid

instance HasChunkRefs CodeSpec where
  chunkRefs x = chunkRefs (x ^. srs)

instance NamedIdea CodeSpec where
  term = srs . term

instance Idea CodeSpec where
  getA x = getA (x ^. srs)

instance CommonIdea CodeSpec where
  abrv x = abrv (x ^. srs)

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
mkCodeSpec :: S.SmithEtAlSRS -> Choices -> CodeSpec
mkCodeSpec si@S.ICO{ S._inputs = ins
                    , S._outputs = outs
                    , S._constraints = cs
                    , S._constants = cnsts } chs =
  let els = extLibs chs
      libReqs = concatMap odeLibReqs els
      infoReqs = concatMap odeInfoReqs els
      db' = insertAll (libReqs ++ infoReqs) $ si ^. systemdb
      sys = set systemdb db' si
      ddefs = sys ^. dataDefns
      db = sys ^. systemdb
      inputs' = map quantvar $ NE.toList ins
      const' = map qtov (filter ((`Map.notMember` conceptMatch (maps chs)) . (^. uid))
        cnsts)
      derived = map qtov $ getDerivedInputs ddefs inputs' const' db
      rels = (map qtoc (getEqModQdsFromIm (sys ^. instModels) ++ mapMaybe qdEFromDD ddefs) \\ derived)
        ++ mapODE (getODE $ extLibs chs)
        ++ map qtoc (handWiredDefs chs)
      -- TODO: When we have better DEModels, we should be deriving our ODE information
      --       directly from the instance models (ims) instead of directly from the choices.
      outs' = map quantvar $ NE.toList outs
      allInputs = inputs' ++ map quantvar derived
      exOrder = solveExecOrder rels (allInputs ++ map quantvar cnsts) outs' db
  in CS {
        _srs = sys,
        _inputs = allInputs,
        _extInputs = inputs',
        _derivedInputs = derived,
        _outputs = outs',
        _configFiles = defaultConfigFiles chs,
        _execOrder = exOrder,
        _cMap = constraintMap cs,
        _constDefns = const',
        _constMap = assocToMap const',
        _mods = extraMods chs
      }

-- medium hacks ---

-- | Convert a 'Func' to an implementation-stage 'DefinedQuantityDict' representing the
-- function.
asVC :: Func -> DefinedQuantityDict
asVC (FDef (FuncDef n d _ _ _ _)) = quantNoUnit (mkUid n) (nounPhraseSP n) (S d) (variable n) Real
asVC (FDef (CtorDef n d _ _ _))   = quantNoUnit (mkUid n) (nounPhraseSP n) (S d) (variable n) Real
asVC (FData (FuncData n d _))     = quantNoUnit (mkUid n) (nounPhraseSP n) (S d) (variable n) Real

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

-- | Get a list of 'Constraint's for a list of 'CodeChunk's.
getConstraints :: (HasUID c) => ConstraintCEMap -> [c] -> [ConstraintCE]
getConstraints cm cs = concat $ mapMaybe (\c -> Map.lookup (c ^. uid) cm) cs

-- | Get a list of 'CodeVarChunk's from a constraint.
constraintvars :: ConstraintCE -> ChunkDB -> [CodeVarChunk]
constraintvars (Range _ ri) m = map (varResolve m) $ nub $ eNamesRI ri
constraintvars (Elem _ ri)  m = map (varResolve m) $ eDep ri
