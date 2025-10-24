module Language.Drasil.ICOSolutionSearch where
    
import Language.Drasil (CodeVarChunk, showUID, DefiningCodeExpr(codeExpr))
import Language.Drasil.Chunk.CodeDefinition (CodeDefinition, auxExprs)
import Database.Drasil (ChunkDB)
import Utils.Drasil (subsetOf)
import Language.Drasil.Chunk.CodeBase (codevars', quantvar)
import Data.List ((\\), intercalate)
import Control.Lens ((^.))

-- | Mathematical definition.
type Def = CodeDefinition
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
