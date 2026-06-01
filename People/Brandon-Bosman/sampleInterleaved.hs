import Data.Set (Set, isSubsetOf, unions, empty, fromList, size)
import Data.List (intercalate, sortBy, find, delete)
import Data.Ord (comparing)

data StmtType = Parse | Constraint | Derived
  deriving Eq

instance Ord StmtType where
  compare a b = comparing ordVal a b
    where ordVal Constraint = 1
          ordVal Derived = 2
          ordVal Parse = 3

-- | Sort input statements.  Inputs are:
-- Statements, structured as (statement type, provides, needs, body)
-- Output structure is body
sortInputStatements :: [(StmtType, Set String, Set String, String)] -> [String]
sortInputStatements stmts = map snd (sortIStmts [] sortedStmts)
  where
    sortIStmts :: [(Set String, String)] -> 
      [(StmtType, Set String, Set String, String)] -> [(Set String, String)]
    sortIStmts ord [] = ord
    sortIStmts ord smts = let 
      know = unions $ map fst ord
      -- Pick the first element that has all its dependencies,
      -- and add it to the ordering.
      smt = find (\(_, _, needs, _) -> needs `isSubsetOf` know) smts
      rest = smt >>= (\s -> pure $ delete s smts)
      in case (smt, rest) of
        (Just s, Just r) -> sortIStmts (ord ++ [cleanup s]) r
        (_, _) -> error "Cannot resolve input dependencies."
    cleanup = \(_, provides, _, bod) -> (provides, bod)
    -- Represents an 'ideal' sorting of the statements: constraints first, then
    -- derived values, then parsing, all sorted by number of dependencies.
    sortedStmts = sortBy (\(sTp1, _, needs1, _) (sTp2, _, needs2, _) ->
                            case compare sTp1 sTp2 of
                              EQ -> comparing size needs1 needs2
                              o  -> o) stmts

tests :: [[(StmtType, Set String, Set String, String)]]
tests = [
  [
    (Parse, fromList ["a"], empty, "a = 1"),
    (Parse, fromList ["b"], empty, "b = 2"),
    (Constraint, empty, fromList ["a"], "a > 0"),
    (Constraint, empty, fromList ["a", "b"], "a > 2 * b"),
    (Constraint, empty, fromList ["c"], "c < 100"),
    (Derived, fromList ["c"], fromList ["a"], "c = a / 2")
  ],
  [
    (Parse, fromList ["a"], empty, "a = 1"),
    (Parse, fromList ["b"], empty, "b = 2"),
    (Constraint, empty, fromList ["a", "b"], "a > b"),
    (Constraint, empty, fromList ["b"], "b < 30")
  ]]

runTests :: IO ()
runTests = do
  let results = map (\stmts -> "Test:" : sortInputStatements stmts ++ [""]) tests
  putStrLn $ intercalate "\n" (concat results)