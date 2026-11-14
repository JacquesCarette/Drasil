{-# LANGUAGE TemplateHaskell #-}
-- | Defines the CodeSpec structure and related functions.
module Language.Drasil.CodeSpec (
  -- * Types
  Input, Output, Const, Derived, ConstantMap,
  AbstractICOProgram, CodeSpec,
  -- * Typeclasses
  HasAbstractICOProgram(..), HasCodeSpec(..),
  -- * Constructors
  mkAbstractICOProgram, mkCodeSpec,
  -- * ODEs
  getODE, mapODE,
  -- * Hacks
  asVC, getConstraints, constraintvars
) where

import Prelude hiding (const)
import Control.Lens ((^.), makeClassy)
import Data.List (nub)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

import Drasil.FileHandling.Legacy (RelativeFile)
import Language.Drasil hiding (None)
import Drasil.Database (ChunkDB, UID, HasUID(..), mkUid)
import Drasil.Code.CodeExpr.Development (eNamesRI, eDep)
import Drasil.System (HasSystemMeta(..), HasProjectName(..), SystemMeta)

import Drasil.Code.CodeVar (CodeVarChunk, quantvar)
import Language.Drasil.Chunk.ConstraintMap (ConstraintCEMap, ConstraintCE)
import Language.Drasil.Chunk.CodeDefinition (CodeDefinition, odeDef)
import Language.Drasil.Choices (ODE(..), ExtLib(..))
import Language.Drasil.Chunk.CodeBase (varResolve)
import Language.Drasil.Mod (Func(..), FuncData(..), FuncDef(..), Mod(..))

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

data AbstractICOProgram
  = AICOP
  { _inputs :: [Input]             -- ^ All inputs.
  , _extInputs :: [Input]          -- ^ Explicit inputs (values to be supplied
                                   --   by a file).
  , _derivedInputs :: [Derived]    -- ^ Derived inputs (each calculated from
                                   --   explicit inputs in a single step).
  , _outputs :: [Output]           -- ^ All outputs.
  , _cMap :: ConstraintCEMap       -- ^ Map from 'UID's to constraints for all
                                   -- constrained chunks used in the problem.
  , _execOrder :: [CodeDefinition] -- ^ Mathematical definitions, ordered so
                                   --   that they form a path from inputs to
                                   --   outputs.
  , _constDefns :: [Const]         -- ^ List of all constants used in the problem.
  }
makeClassy ''AbstractICOProgram

mkAbstractICOProgram
  :: [Input]          -- ^ Explicit inputs (values to be supplied by a file).
  -> [Derived]        -- ^ Derived inputs, each depending only on explicit
                      --   inputs and/or constants.
  -> [Output]         -- ^ All outputs.
  -> [CodeDefinition] -- ^ Mathematical definitions, ordered so that they form a
                      --   path from inputs to outputs: each depends only on
                      --   inputs, constants, and earlier definitions.
  -> ConstraintCEMap  -- ^ Map from 'UID's to constraints for all constrained
                      --   chunks used in the problem.
  -> [Const]          -- ^ All constants used in the problem.
  -> AbstractICOProgram
mkAbstractICOProgram ins derived outs exOrder cmap cnsts
  = AICOP
  { _inputs = ins <> (quantvar <$> derived)
  , _extInputs = ins
  , _derivedInputs = derived
  , _outputs = outs
  , _execOrder = exOrder
  , _cMap = cmap
  , _constDefns = cnsts
  }

-- | Code Specification. Holds system information and options.
data CodeSpec
  = CS
  { _csSysMeta :: SystemMeta
  , _absICOProg :: AbstractICOProgram -- ^ The abstract ICO problem.
  , _configFiles :: [RelativeFile] -- ^ List of files that must be in same
                                   --   directory for running the executable.
  , _constMap :: ConstantMap       -- ^ Map containing all constants used in
                                   --   the problem.
  , _mods :: [Mod]                 -- ^ Additional modules required in the
                                   --   generated code, which Drasil cannot
                                   --   yet automatically define.
  }
makeClassy ''CodeSpec

instance HasSystemMeta CodeSpec where
  systemMeta = csSysMeta

instance HasProjectName CodeSpec where
  projectName = systemMeta . projectName

instance HasAbstractICOProgram CodeSpec where
  abstractICOProgram = absICOProg

-- | Converts a list of chunks that have 'UID's to a Map from 'UID' to the associated chunk.
assocToMap :: HasUID a => [a] -> Map.Map UID a
assocToMap = Map.fromList . fmap (\x -> (x ^. uid, x))

-- | Get ODE from ExtLib
getODE :: [ExtLib] -> Maybe ODE
getODE [] = Nothing
getODE (Math ode: _) = Just ode
-- getODE (_:xs) = getODE xs

-- | Maps ODE to their respective 'CodeDefinition'.
mapODE :: Maybe ODE -> [CodeDefinition]
mapODE Nothing = []
mapODE (Just ode) = odeDef <$> odeInfo ode

-- | Creates a 'CodeSpec'. The full input list (explicit inputs, then derived
-- inputs) and the constant map are derived from the arguments. *Nothing is
-- checked* (yet), but code generation assumes that the arguments meet the
-- conditions noted.
mkCodeSpec
  :: SystemMeta       -- ^ System metadata. Its 'ChunkDB' must contain every
                      --   chunk the other arguments refer to, plus 'codeDQDs'
                      --   and the chunks the chosen external libraries need
                      --   ('odeInfoReqs' and 'odeLibReqs').
  -> AbstractICOProgram -- ^ The abstract ICO program to refine.
  -> [RelativeFile]   -- ^ Files that must be in the same directory for running
                      --   the executable.
  -> [Mod]            -- ^ Additional code modules required in the generated code.
  -> CodeSpec
mkCodeSpec sysMeta aicop cfgs ms
  = CS
  { _csSysMeta = sysMeta
  , _absICOProg = aicop
  , _configFiles = cfgs
  , _constMap = assocToMap $ aicop ^. constDefns
  , _mods = ms
  }

-- medium hacks ---

-- | Convert a 'Func' to an implementation-stage 'DefinedQuantityDict' representing the
-- function.
asVC :: Func -> DefinedQuantityDict
asVC (FDef (FuncDef n d _ _ _ _)) = quantNoUnit (mkUid n) (nounPhraseSP n) (S d) (variable n) Real
asVC (FDef (CtorDef n d _ _ _))   = quantNoUnit (mkUid n) (nounPhraseSP n) (S d) (variable n) Real
asVC (FData (FuncData n d _))     = quantNoUnit (mkUid n) (nounPhraseSP n) (S d) (variable n) Real

-- | Get a list of 'Constraint's for a list of 'CodeChunk's.
getConstraints :: (HasUID c) => ConstraintCEMap -> [c] -> [ConstraintCE]
getConstraints cm cs = concat $ mapMaybe (\c -> Map.lookup (c ^. uid) cm) cs

-- | Get a list of 'CodeVarChunk's from a constraint.
constraintvars :: ConstraintCE -> ChunkDB -> [CodeVarChunk]
constraintvars (Range _ ri) m = fmap (varResolve m) $ nub $ eNamesRI ri
constraintvars (Elem _ ri)  m = varResolve m <$> eDep ri
constraintvars (Eqn _ e)  m = varResolve m <$> eDep e
