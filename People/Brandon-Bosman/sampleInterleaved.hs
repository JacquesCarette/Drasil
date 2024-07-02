import Data.Set (Set, isSubsetOf, unions, empty, fromList, toList, size)
import Data.List (partition, intercalate, sortBy)
import Data.Ord (comparing)

-- | Sort input statements.  Inputs are:
-- Given inputs, structured as (provides, body)
-- Constraint checks, structured as (needs, body)
-- Derived values, structured as (provides, needs, body)
-- Output structure is (provides, body)
sortInputStatements :: [(Set String, String)] -> [(Set String, String)] -> [(Set String, Set String, String)] -> [String]
sortInputStatements ps cs ds = map snd (sortIStmts [] ps cs ds)
  where 
    sortIStmts :: [(Set String, String)] -> [(Set String, String)] -> [(Set String, String)] -> [(Set String, Set String, String)] -> [(Set String, String)]
    sortIStmts ord [] [] [] = ord
    sortIStmts ord ps cs ds = let
      know = unions $ map fst ord
      (newCs, oldCs) = partition (\(needs, _) -> needs `isSubsetOf` know) cs
      (newDs, oldDs) = partition (\(_, needs, _) -> needs `isSubsetOf` know) ds
      in 
        -- If constraints can be checked, do that
        if not $ null newCs then
              -- Sort statements by number of dependencies
          let sortedNewCs = sortBy (\(needs1, _) (needs2, _) -> comparing size needs1 needs2) newCs
              newOrd = map (\(_, bod) -> (empty, bod)) sortedNewCs in
          sortIStmts (ord ++ newOrd) ps oldCs ds
        -- Or if derived inputs can be calculated, do that
        else if not $ null newDs then
              -- Sort statements by number of dependencies
          let sortedNewds = sortBy (\(_, needs1, _) (_, needs2, _) -> comparing size needs1 needs2) newDs
              newOrd = map (\(prov, _, bod) -> (prov, bod)) newDs in
          sortIStmts (ord ++ newOrd) ps cs oldDs
        -- If nothing else, parse the next input
        else case ps of
          (p:ps') -> sortIStmts (ord ++ [p]) ps' cs ds
          -- If no inputs to parse, give up
          _ -> error "Cannot resolve input dependencies."

tests :: [([(Set String, String)], [(Set String, String)], [(Set String, Set String, String)])]
tests = [
  ([(fromList ["a"], "a = 1"), (fromList ["b"], "b = 2")], [(fromList ["a"], "a > 0"), (fromList ["a", "b"], "a > 2 * b"), (fromList ["c"], "c < 100")], [(fromList ["c"], fromList ["a"], "c = a / 2")]),
  ([(fromList ["a"], "a = 1"), (fromList ["b"], "b = 2")], [(fromList ["a", "b"], "a > b"), (fromList ["b"], "b < 30")], [])
  ]

runTests :: IO ()
runTests = do
  let results = map (\(p, c, d) -> "Test:" : sortInputStatements p c d ++ [""]) tests
  putStrLn $ intercalate "\n" (concat results)