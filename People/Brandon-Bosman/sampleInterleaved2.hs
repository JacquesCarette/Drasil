import Data.Set (Set, isSubsetOf, unions, empty, fromList, toList, size)
import Data.List (partition, intercalate, sortBy)
import Data.Ord (comparing)

-- | Sort input statements.  Inputs are:
-- Statements, structured as (provides, needs, body)
-- Output structure is (provides, body)
sortInputStatements :: [(Set String, Set String, String)] -> [String]
sortInputStatements stmts = map snd (sortIStmts [] stmts)
  where 
    sortIStmts :: [(Set String, String)] -> [(Set String, Set String, String)] -> [(Set String, String)]
    sortIStmts ord [] = ord
    sortIStmts ord smts = let
      know = unions $ map fst ord
      (newStmts, oldStmts) = partition (\(_, needs, _) -> needs `isSubsetOf` know) smts
      in 
        if not $ null newStmts then
          -- Sort statements by number of dependencies
          let sortedNewStmts = sortBy (\(_, needs1, _) (_, needs2, _) -> comparing size needs1 needs2) newStmts
              newOrd = map (\(provides, _, bod) -> (provides, bod)) sortedNewStmts in
          sortIStmts (ord ++ newOrd) oldStmts
        else error "Cannot resolve input dependencies."

tests :: [[(Set String, Set String, String)]]
tests = [
  [(fromList ["a"], empty, "a = 1"), (fromList ["b"], empty, "b = 2"), (empty, fromList ["a"], "a > 0"), (empty, fromList ["a", "b"], "a > 2 * b"), (empty, fromList ["c"], "c < 100"), (fromList ["c"], fromList ["a"], "c = a / 2")],
  [(fromList ["a"], empty, "a = 1"), (fromList ["b"], empty, "b = 2"), (empty, fromList ["a", "b"], "a > b"), (empty, fromList ["b"], "b < 30")]
  ]

runTests :: IO ()
runTests = do
  let results = map (\stmts -> "Test:" : sortInputStatements stmts ++ [""]) tests
  putStrLn $ intercalate "\n" (concat results)