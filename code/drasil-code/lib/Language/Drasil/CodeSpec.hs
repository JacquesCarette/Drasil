{-# LANGUAGE GADTs #-}
-- | Defines the CodeSpec structure and related functions.
module Language.Drasil.CodeSpec where

import Language.Drasil hiding (None, new)
import Language.Drasil.Development (showUID)
import Language.Drasil.Display (Symbol(Variable))
import Database.Drasil
import SysInfo.Drasil hiding (sysinfodb)
import Theory.Drasil (DataDefinition, qdEFromDD, getEqModQdsFromIm)

import Language.Drasil.Chunk.ConstraintMap (ConstraintCEMap, ConstraintCE, constraintMap)
import Language.Drasil.Chunk.CodeDefinition (CodeDefinition, qtov, qtoc, odeDef,
  auxExprs)
import Language.Drasil.Choices (Choices(..), Maps(..), ODE(..), ExtLib(..))
import Language.Drasil.CodeExpr.Development (expr, eNamesRI)
import Language.Drasil.Chunk.CodeBase
import Language.Drasil.Mod (Func(..), FuncData(..), FuncDef(..), Mod(..), Name)

import Utils.Drasil (subsetOf)

import Control.Lens ((^.))
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

-- | Code specifications. Holds information needed to generate code.
data CodeSpec where
  CodeSpec :: (HasName a) => {
  -- | Program name.
  pName :: Name,
  -- | Authors.
  authors :: [a],
  -- | Purpose.
  purpose :: Purpose,
  -- | Example Background.
  background :: Background,
  -- | All inputs.
  inputs :: [Input],
  -- | Explicit inputs (values to be supplied by a file).
  extInputs :: [Input],
  -- | Derived inputs (each calculated from explicit inputs in a single step).
  derivedInputs :: [Derived],
  -- | All outputs.
  outputs :: [Output],
  -- | List of files that must be in same directory for running the executable.
  configFiles :: [FilePath],
  -- | Mathematical definitions, ordered so that they form a path from inputs to 
  -- outputs.
  execOrder :: [Def],
  -- | Map from 'UID's to constraints for all constrained chunks used in the problem.
  cMap :: ConstraintCEMap,
  -- | List of all constants used in the problem.
  constants :: [Const],
  -- | Map containing all constants used in the problem.
  constMap :: ConstantMap,
  -- | Additional modules required in the generated code, which Drasil cannot yet 
  -- automatically define.
  mods :: [Mod],  -- medium hack
  -- | The database of all chunks used in the problem.
  sysinfodb :: ChunkDB
  } -> CodeSpec

-- | Maps constants to their respective 'CodeDefinition'.
type ConstantMap = Map.Map UID CodeDefinition

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

-- | Defines a 'CodeSpec' based on the 'SystemInformation', 'Choices', and 'Mod's
-- defined by the user.
codeSpec :: SystemInformation -> Choices -> [Mod] -> CodeSpec
codeSpec SI {_sys         = sys
           , _authors     = as
           , _purpose     = ps
           , _background  = bk
           , _instModels  = ims
           , _datadefs    = ddefs
           , _configFiles = cfp
           , _inputs      = ins
           , _outputs     = outs
           , _constraints = cs
           , _constants   = cnsts
           , _sysinfodb   = db} chs ms =
  let n = programName sys
      inputs' = map quantvar ins
      const' = map qtov (filter ((`Map.notMember` conceptMatch (maps chs)) . (^. uid))
        cnsts)
      derived = map qtov $ getDerivedInputs ddefs inputs' const' db
      rels = (map qtoc (getEqModQdsFromIm ims ++ mapMaybe qdEFromDD ddefs) \\ derived)
        ++ mapODE (getODE $ extLibs chs)
      -- TODO: When we have better DEModels, we should be deriving our ODE information
      --       directly from the instance models (ims) instead of directly from the choices.
      outs' = map quantvar outs
      allInputs = nub $ inputs' ++ map quantvar derived
      exOrder = getExecOrder rels (allInputs ++ map quantvar cnsts) outs' db
  in  CodeSpec {
        pName = n,
        authors = as,
        purpose = ps,
        background = bk,
        inputs = allInputs,
        extInputs = inputs',
        derivedInputs = derived,
        outputs = outs',
        configFiles = cfp,
        execOrder = exOrder,
        cMap = constraintMap cs,
        constants = const',
        constMap = assocToMap const',
        mods = ms,
        sysinfodb = db
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
