{-# LANGUAGE GADTs #-}
-- | Defines the CodeSpec structure and related functions.
module Language.Drasil.CodeSpec where

import Language.Drasil
import Database.Drasil (ChunkDB, SystemInformation(SI), symbResolve,
  _authors, _constants, _constraints, _datadefs, _configFiles,
  _inputs, _outputs, _sys, _sysinfodb)
import Language.Drasil.Development (namesRI, EqBinOp(Eq))
import Theory.Drasil (DataDefinition, qdFromDD)

import Language.Drasil.Chunk.Code (CodeChunk, CodeVarChunk, CodeIdea(codeChunk),
  ConstraintMap, programName, quantvar, codevars, codevars',
  varResolve, constraintMap)
import Language.Drasil.Chunk.CodeDefinition (CodeDefinition, qtov, qtoc, odeDef,
  auxExprs, codeEquat)
import Language.Drasil.Choices (Choices(..))
import Language.Drasil.Mod (Func(..), FuncData(..), FuncDef(..), Mod(..), Name)

import Control.Lens ((^.))
import Data.List (intercalate, nub, (\\))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

import Prelude hiding (const)

type Input = CodeVarChunk
type Output = CodeVarChunk
type Const = CodeDefinition
type Derived = CodeDefinition
type Def = CodeDefinition

data CodeSpec where
  CodeSpec :: (HasName a) => {
  -- Program name
  pName :: Name,
  -- Authors
  authors :: [a],
  -- All inputs
  inputs :: [Input],
  -- Explicit inputs (values to be supplied by a file)
  extInputs :: [Input],
  -- Derived inputs (each calculated from explicit inputs in a single step)
  derivedInputs :: [Derived],
  -- All outputs
  outputs :: [Output],
  -- List of files that must be in same directory for running the executable
  configFiles :: [FilePath],
  -- Mathematical definitions, ordered so that they form a path from inputs to 
  -- outputs.
  execOrder :: [Def],
  -- Map from UIDs to constraints for all constrained chunks used in the problem
  cMap :: ConstraintMap,
  -- List of all constants used in the problem
  constants :: [Const],
  -- Map containing all constants used in the problem.
  constMap :: ConstantMap,
  -- Additional modules required in the generated code, which Drasil cannot yet 
  -- automatically define.
  mods :: [Mod],  -- medium hack
  -- The database of all chunks used in the problem.
  sysinfodb :: ChunkDB
  } -> CodeSpec

type ConstantMap = Map.Map UID CodeDefinition

-- Converts a list of chunks that have UIDs to a Map from UID to the associated chunk.
assocToMap :: HasUID a => [a] -> Map.Map UID a
assocToMap = Map.fromList . map (\x -> (x ^. uid, x))

-- | Defines a CodeSpec based on the SystemInformation, Choices, and Mods 
-- defined by the user.
codeSpec :: SystemInformation -> Choices -> [Mod] -> CodeSpec
codeSpec SI {_sys = sys
              , _authors = as
              , _datadefs = ddefs
              , _configFiles = cfp
              , _inputs = ins
              , _outputs = outs
              , _constraints = cs
              , _constants = cnsts
              , _sysinfodb = db} chs ms =
  let n = programName sys
      inputs' = map quantvar ins
      const' = map qtov (filter ((`Map.notMember` conceptMatch chs) . (^. uid))
        cnsts)
      derived = getDerivedInputs ddefs inputs' const' db
      rels = map qtoc (map qdFromDD ddefs \\ derived) ++ map odeDef (odes chs)
      outs' = map quantvar outs
      allInputs = nub $ inputs' ++ map quantvar derived
      exOrder = getExecOrder rels (allInputs ++ map quantvar cnsts) outs' db
  in  CodeSpec {
        pName = n,
        authors = as,
        inputs = allInputs,
        extInputs = inputs',
        derivedInputs = map qtov derived,
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

-- Converts a chunk with a defining relation to a QDefinition
relToQD :: ExprRelat c => ChunkDB -> c -> QDefinition
relToQD sm r = convertRel sm (relat r)

-- Converts an Expr representing a definition (i.e. an equality where the left 
-- side is just a variable) to a QDefinition.
convertRel :: ChunkDB -> Expr -> QDefinition
convertRel sm (EqBinaryOp Eq (C x) r) = ec (symbResolve sm x) r
convertRel _ _ = error "Conversion failed"

-- | Convert a Func to an implementation-stage QuantityDict representing the 
-- function.
asVC :: Func -> QuantityDict
asVC (FDef (FuncDef n _ _ _ _ _)) = implVar n (nounPhraseSP n) Real (Variable n)
asVC (FDef (CtorDef n _ _ _ _)) = implVar n (nounPhraseSP n) Real (Variable n)
asVC (FData (FuncData n _ _)) = implVar n (nounPhraseSP n) Real (Variable n)

-- | Get a UID of a chunk corresponding to a Func
funcUID :: Func -> UID
funcUID f = asVC f ^. uid

-- | FIXME: hack. Use for implementation-stage functions that need to be displayed in the SRS.
funcUID' :: Func -> UID
funcUID' f = asVC' f ^. uid

-- | FIXME: Part of above hack
asVC' :: Func -> QuantityDict
asVC' (FDef (FuncDef n _ _ _ _ _)) = vc n (nounPhraseSP n) (Variable n) Real
asVC' (FDef (CtorDef n _ _ _ _)) = vc n (nounPhraseSP n) (Variable n) Real
asVC' (FData (FuncData n _ _)) = vc n (nounPhraseSP n) (Variable n) Real

-- Determines the derived inputs, which can be immediately calculated from the 
-- knowns (inputs and constants). If there are DDs, the derived inputs will 
-- come from those. If there are none, then the QDefinitions are used instead.
getDerivedInputs :: [DataDefinition] -> [Input] -> [Const] ->
  ChunkDB -> [QDefinition]
getDerivedInputs ddefs ins cnsts sm =
  filter ((`subsetOf` refSet) . flip codevars sm . (^. defnExpr)) (map qdFromDD ddefs)
  where refSet = ins ++ map quantvar cnsts

type Known = CodeVarChunk
type Need  = CodeVarChunk

-- Orders a list of definitions such that they form a path between Known values 
-- and values that Need to be calculated.
getExecOrder :: [Def] -> [Known] -> [Need] -> ChunkDB -> [Def]
getExecOrder d k' n' sm  = getExecOrder' [] d k' (n' \\ k')
  where getExecOrder' ord _ _ []   = ord
        getExecOrder' ord defs' k n =
          let new  = filter (\def -> (`subsetOf` k) (concatMap (`codevars'` sm)
                (codeEquat def : def ^. auxExprs) \\ [quantvar def])) defs'
              cnew = map quantvar new
              kNew = k ++ cnew
              nNew = n \\ cnew
          in  if null new
              then error ("The following outputs cannot be computed: " ++
                       intercalate ", " (map (^. uid) n) ++ "\n"
                     ++ "Unused definitions are: "
                       ++ intercalate ", " (map (^. uid) defs') ++ "\n"
                     ++ "Known values are: "
                       ++ intercalate ", " (map (^. uid) k))
              else getExecOrder' (ord ++ new) (defs' \\ new) kNew nNew

-- Returns true if the first list contains only elements that are present in 
-- the second list.
subsetOf :: (Eq a) => [a] -> [a] -> Bool
xs `subsetOf` ys = all (`elem` ys) xs

-- | Get a list of Constraints for a list of CodeChunks
getConstraints :: (HasUID c) => ConstraintMap -> [c] -> [Constraint]
getConstraints cm cs = concat $ mapMaybe (\c -> Map.lookup (c ^. uid) cm) cs

-- | Get a list of CodeChunks from a constraint
constraintvars :: Constraint -> ChunkDB -> [CodeChunk]
constraintvars (Range _ ri) m = map (codeChunk . varResolve m) $ nub $
  namesRI ri
constraintvars _ _ = []
