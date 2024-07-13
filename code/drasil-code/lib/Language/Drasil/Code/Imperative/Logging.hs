module Language.Drasil.Code.Imperative.Logging (
  maybeLog, logBody, loggedMethod, varLogFile
) where

import Language.Drasil.Code.Imperative.DrasilState (GenState, DrasilState(..))
import Language.Drasil.Choices (Logging(..))

import GOOL.Drasil (Label, MSBody, MSBlock, SVariable, SValue, MSStatement,
  OOProg, BodySym(..), BlockSym(..), TypeSym(..), var,
  VariableElim(..), Literal(..), VariableValue(..), StatementSym(..),
  DeclStatement(..), IOStatement(..), lensMStoVS, ScopeSym(..))

import Control.Lens.Zoom (zoom)
import Control.Monad.State (get)

-- | Generates a statement that logs the given variable's value, if the user
-- chose to turn on logging of variable assignments.
maybeLog :: (OOProg r) => SVariable r -> r (Scope r) -> GenState [MSStatement r]
maybeLog v scp = do
  g <- get
  sequence [loggedVar v scp | LogVar `elem` logKind g]

-- | Generates a statement that logs the name of the given variable, its current
-- value, and the current module name.
loggedVar :: (OOProg r) => SVariable r -> r (Scope r) -> GenState (MSStatement r)
loggedVar v scp = do
  g <- get
  return $ multi [
    openFileA (varLogFile scp) (litString $ logName g),
    zoom lensMStoVS v >>= (\v' -> printFileStr (valLogFile scp) ("var '" ++
      variableName v' ++ "' assigned ")),
    printFile (valLogFile scp) (valueOf v),
    printFileStrLn (valLogFile scp) (" in module " ++ currentModule g),
    closeFile (valLogFile scp)]

-- | Generates the body of a function with the given name, list of parameters,
-- and blocks to include in the body. If the user chose to turn on logging of
-- function calls, statements that log how the function was called are added to
-- the beginning of the body.
logBody :: (OOProg r) => Label -> [SVariable r] -> [MSBlock r] ->
  GenState (MSBody r)
logBody n vars b = do
  g <- get
  return $ body $ [loggedMethod (logName g) n vars | LogFunc `elem` logKind g] ++ b

-- | Generates a block that logs, to the given 'FilePath', the name of a function,
-- and the names and values of the passed list of variables. Intended to be
-- used as the first block in the function, to log that it was called and what
-- inputs it was called with.
loggedMethod :: (OOProg r) => FilePath -> Label -> [SVariable r] -> MSBlock r
loggedMethod lName n vars = block [
      varDec (varLogFile local),
      openFileA (varLogFile local) (litString lName),
      printFileStrLn (valLogFile local) ("function " ++ n ++ " called with inputs: {"),
      multi $ printInputs vars,
      printFileStrLn (valLogFile local) "  }",
      closeFile (valLogFile local) ]
  where
    printInputs [] = []
    printInputs [v] = [
      zoom lensMStoVS v >>= (\v' -> printFileStr (valLogFile local) ("  " ++
        variableName v' ++ " = ")),
      printFileLn (valLogFile local) (valueOf v)]
    printInputs (v:vs) = [
      zoom lensMStoVS v >>= (\v' -> printFileStr (valLogFile local) ("  " ++
        variableName v' ++ " = ")),
      printFile (valLogFile local) (valueOf v),
      printFileStrLn (valLogFile local) ", "] ++ printInputs vs

-- | The variable representing the log file in write mode.
varLogFile :: (OOProg r) => r (Scope r) -> SVariable r
varLogFile = var "outfile" outfile

-- | The value of the variable representing the log file in write mode.
valLogFile :: (OOProg r) => r (Scope r) -> SValue r
valLogFile s = valueOf $ varLogFile s
