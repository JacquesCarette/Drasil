-- sort-type-parameters.hs
-- Written by Brandon Bosman
-- Generates perl commands for sorting the type parameters of GOOL's typeclasses

import Data.Map (Map, fromList, toList)
import Data.List (elemIndex, sortOn)
import Data.Maybe (fromMaybe)
import Control.Monad (liftM)

-- The parameters, listed in their current order.
classParams :: Map String [String]
classParams = fromList
  [ ("BodySym", words "r  bod block")
  , ("BlockSym", words "r block stmt")
  , ("BlockSym", words "r block stmt")
  , ("VariableSym", words "r typ var")
  , ("VariableElim", words "r typ var")
  , ("ValueSym", words "r typ val")
  , ("Literal", words "r typ val")
  , ("VariableValue", words "r var val")
  , ("NamedArgs", words "r var val")
  , ("MixedCall", words "r typ var val")
  , ("MixedCtorCall", words "r typ var val")
  , ("PosCall", words "r typ val")
  , ("PosCtorCall", words "r typ val")
  , ("BinderSym", words "r typ binder")
  , ("BinderElim", words "r typ binder")
  , ("ValueExpression", words "r typ binder var val")
  , ("Array", words "r var val") -- Careful with this one
  , ("ListStatement", words "r val stmt")
  , ("NativeVector", words "r typ val")
  , ("InternalList", words "r var val block")
  , ("ValueStatement", words "r val stmt")
  , ("AssignStatement", words "r var val stmt")
  , ("DeclStatement", words "r scope var val stmt bod")
  , ("PrintConsole", words "r val stmt")
  , ("ReadConsole", words "r var stmt")
  , ("FileHandling", words "r var val stmt")
  , ("PrintFile", words "r val stmt")
  , ("ReadFile", words "r var val stmt")
  , ("StringStatement", words "r var val stmt")
  , ("InOutCall", words "r var val stmt")
  , ("FuncAppStatement", words "r var val stmt")
  , ("ControlStatement", words "r var val stmt bod")
  , ("ParameterSym", words "r var param")
  , ("InOutFunc", words "r var mthd bod")
  , ("DocInOutFunc", words "r var mthd bod")
  , ("MethodSym", words "r vis typ var param mthd bod")

  , ("ProgramSym", words "r prg file")
  , ("FileSym", words "r file mod")
  , ("ClassSym", words "r cls mthd stvr")
  , ("Initializers", words "r var val")
  , ("OOMethodSym", words "r vis typ var param val mthd attch bod")
  , ("StateVarSym", words "r vis var val stvr attch")
  , ("OOVariableSym", words "r typ var val")
  , ("OOValueExpression", words "r typ var val")
  , ("InternalValueExp", words "r typ var val")
  , ("OODeclStatement", words "r scope var val stmt")
  , ("OOFuncAppStatement", words "r var val stmt")
  , ("ObserverPattern", words "r typ stmt")
  , ("StrategyPattern", words "r var val bod block")
  , ("OOFunctionSym", words "r typ val")
  , ("GetSet", words "r var val")
  , ("OOProg", words "r vis scope typ binder var param val stmt cls mthd stvr attch prg file mod bod block")

  , ("ProcProg", words "r vis scope typ binder var param val stmt mthd prg file mod bod block")

  , ("CommonRenderSym", words "r vis scope typ binder var param val stmt mthd bod block")
  , ("RenderVariable", words "r typ var")
  , ("RenderValue", words "r typ var val")
  , ("InternalListFunc", words "r typ val")
  , ("InternalAssignStmt", words "r var val stmt")
  , ("InternalIOStmt", words "r val stmt")
  , ("InternalControlStmt", words "r val stmt")
  , ("RenderParam", words "r var param")
  , ("ParamElim", words "r typ param")
  , ("OORenderSym", words "r vis scope typ binder var param val stmt cls mthd stvr attch file mod bod block")
  , ("RenderFile", words "r file mod")
  , ("InternalGetSet", words "r typ var val")
  , ("OORenderMethod", words "r vis typ param mthd attch bod")
  , ("RenderClass", words "r vis cls mthd stvr")
  , ("ProcRenderSym", words "r vis scope typ binder var param val stmt mthd file mod bod block")
  , ("RenderFile", words "r file mod")
  , ("ProcRenderMethod", words "r vis typ param mthd bod")
  ]

-- The desired global ordering.
globalOrder :: [String]
globalOrder = words "r prg file mod cls stvr mthd attch vis param bod block stmt var scope val binder typ"

permutations :: Map String [Int]
permutations = let
  getPerm :: [String] -> [Int]
  getPerm params = let
    sorted :: [String]
    sorted = sortOn (\param -> fromMaybe 0 (elemIndex param globalOrder)) params
    in map (\param -> fromMaybe 0 (elemIndex param params)) sorted

  in fmap getPerm classParams

makeCommands :: DirOrFile -> IO ()
makeCommands dirOrFile = do
  let classList = toList permutations
      command (className, perm) = makeCommand dirOrFile (makeSed className perm)
  putStrLn "Classes:"
  putStrLn $ unlines $ map command classList

makeSed :: String -> [Int] -> String
makeSed className perm = let
  paramNum = length perm
  captureGroups = concat $ replicate paramNum "( [a-zA-Z]+)"
  reOrders = concatMap (\num -> "$" ++ show (num + 2)) perm

  in "perl -pi -e 's/\\b(" ++ className ++ ")" ++ captureGroups ++ "/\\1" ++ reOrders ++ "/g'"

prettyPrintMap :: (Show k, Show a) => Map k a -> String
prettyPrintMap m = let
  l = toList m
  unpack (key, val) = show key  ++ ": " ++ show val
  in unlines (map unpack l)

data DirOrFile = Dir String | File String

makeCommand :: DirOrFile -> String -> String
makeCommand dirOrFile sed = case dirOrFile of
  Dir  dir  -> "find " ++ dir ++ " -type f -name '*.hs' -exec " ++ sed ++ " {} +"
  File file -> sed ++ " " ++ file
