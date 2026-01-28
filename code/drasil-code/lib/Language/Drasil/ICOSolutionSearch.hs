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

-- | Find a calculation path from a list of 'Known' values to values that 'Need'
-- to be calculated, i.e., topologically sort a list of 'Def's.
solveExecOrder :: [Def] -> [Known] -> [Need] -> ChunkDB -> [Def]
solveExecOrder allDefs knowns needs =
  topologicalSort [] allDefs knowns (needs \\ knowns)

-- | Topologically sort a list of 'Def's. First parameter is the found path.
topologicalSort :: [Def] -> [Def] -> [Known] -> [Need] -> ChunkDB -> [Def]
topologicalSort foundOrder allDefs knowns needs db
  -- Successfully found a path
  | null needs = foundOrder
  -- Path impossible (missing pieces)
  | null nextCalcs = prettyError allDefs knowns needs
  -- Continuously looks for the next possible set of 'Needs' that can be
  -- computed until all are consumed.
  | otherwise = topologicalSort
                  (foundOrder ++ nextCalcs)
                  notReady
                  (knowns ++ newlyCalculated)
                  (needs \\ newlyCalculated)
                  db
  where
    (nextCalcs, notReady) = partition (computable db knowns) allDefs
    newlyCalculated = map quantvar nextCalcs

-- | Check if a 'Def' is computable given a list of 'Known's.
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
