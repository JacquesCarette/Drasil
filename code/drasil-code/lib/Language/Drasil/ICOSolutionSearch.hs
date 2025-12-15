module Language.Drasil.ICOSolutionSearch (
    Def, Known, Need, solveExecOrder
) where

import Control.Lens ((^.))
import Data.List ((\\), intercalate, partition)

import Drasil.Database (ChunkDB, showUID, HasUID)
import Language.Drasil.Chunk.CodeDefinition (CodeDefinition, auxExprs)
import Utils.Drasil (subsetOf)

import Drasil.Code.CodeVar (DefiningCodeExpr(..), CodeVarChunk)
import Language.Drasil.Chunk.CodeBase (codevars', quantvar)

-- | Mathematical definition.
type Def = CodeDefinition
-- | Known values.
type Known = CodeVarChunk
-- | Calculated values.
type Need  = CodeVarChunk

-- | Topologically sort a list of 'Def's to form a path from 'Known' values to
-- values that 'Need' to be calculated.
solveExecOrder :: [Def] -> [Known] -> [Need] -> ChunkDB -> [Def]
solveExecOrder allDefs knowns needs = solve [] allDefs knowns (needs \\ knowns)

solve :: [Def] -> [Def] -> [Known] -> [Need] -> ChunkDB -> [Def]
solve foundOrder allDefs knowns needs db
  -- Successfully found a path
  | null needs = foundOrder
  -- Path impossible (missing pieces)
  | null nextCalcs = prettyError allDefs knowns needs
  -- Continuously looks for the next possible set of 'Needs' that can be
  -- computed until all are consumed.
  | otherwise = solve
                  (foundOrder ++ nextCalcs)
                  notReady
                  (knowns ++ newlyCalculated)
                  (needs \\ newlyCalculated)
                  db
  where
    (nextCalcs, notReady) = partition (computable db knowns) allDefs
    newlyCalculated = map quantvar nextCalcs

computable :: ChunkDB -> [Known] -> Def -> Bool
computable db knowns def = requiredInputs `subsetOf` knowns
  where
    inputs = concatMap (`codevars'` db) (def ^. codeExpr : def ^. auxExprs)
    -- FIXME: This allows variables to be defined in terms of themselves, but is
    -- this the right spot for this sanity check?
    requiredInputs = inputs \\ [quantvar def]

prettyError :: [Def] -> [Known] -> [Need] -> a
prettyError defs knowns needs = error $
  "The following outputs cannot be computed: " ++ lm needs ++ "\n" ++
  "Unused definitions are: " ++ lm defs ++ "\n" ++
  "Known values are: " ++ lm knowns
  where
    lm :: HasUID c => [c] -> String
    lm = intercalate ", " . map showUID
