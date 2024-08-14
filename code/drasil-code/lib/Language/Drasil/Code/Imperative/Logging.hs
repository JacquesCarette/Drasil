module Language.Drasil.Code.Imperative.Logging (
  maybeLog, logBody, loggedMethod, varLogFile
) where

import Language.Drasil.Code.Imperative.DrasilState (GenState, DrasilState(..))
import Language.Drasil.Choices (Logging(..))

import Drasil.GOOL (Label, MSBody, MSBlock, SVariable, SValue, MSStatement,
  SharedProg, BodySym(..), BlockSym(..), TypeSym(..), var, VariableElim(..),
  Literal(..), VariableValue(..), StatementSym(..), DeclStatement(..),
  IOStatement(..), lensMStoVS, ScopeSym(..))

import Control.Lens.Zoom (zoom)
import Control.Monad.State (get)

-- | Generates a statement that logs the given variable's value, if the user
-- chose to turn on logging of variable assignments.
maybeLog :: (SharedProg r) => SVariable r -> GenState [MSStatement r]
maybeLog v = do
  g <- get
  sequence [loggedVar v | LogVar `elem` logKind g]

-- | Generates a statement that logs the name of the given variable, its current
-- value, and the current module name.
loggedVar :: (SharedProg r) => SVariable r -> GenState (MSStatement r)
loggedVar v = do
  g <- get
  return $ multi [
    openFileA varLogFile (litString $ logName g),
    zoom lensMStoVS v >>= (\v' -> printFileStr valLogFile ("var '" ++
      variableName v' ++ "' assigned ")),
    printFile valLogFile (valueOf v),
    printFileStrLn valLogFile (" in module " ++ currentModule g),
    closeFile valLogFile]

-- | Generates the body of a function with the given name, list of parameters,
-- and blocks to include in the body. If the user chose to turn on logging of
-- function calls, statements that log how the function was called are added to
-- the beginning of the body.
logBody :: (SharedProg r) => Label -> [SVariable r] -> [MSBlock r] ->
  GenState (MSBody r)
logBody n vars b = do
  g <- get
  return $ body $ [loggedMethod (logName g) n vars | LogFunc `elem` logKind g] ++ b

-- | Generates a block that logs, to the given 'FilePath', the name of a function,
-- and the names and values of the passed list of variables. Intended to be
-- used as the first block in the function, to log that it was called and what
-- inputs it was called with.
loggedMethod :: (SharedProg r) => FilePath -> Label -> [SVariable r] -> MSBlock r
loggedMethod lName n vars = block [
      varDec varLogFile, --local
      openFileA varLogFile (litString lName),
      printFileStrLn valLogFile ("function " ++ n ++ " called with inputs: {"),
      multi $ printInputs vars,
      printFileStrLn valLogFile "  }",
      closeFile valLogFile]
  where
    printInputs [] = []
    printInputs [v] = [
      zoom lensMStoVS v >>= (\v' -> printFileStr valLogFile ("  " ++
        variableName v' ++ " = ")),
      printFileLn valLogFile (valueOf v)]
    printInputs (v:vs) = [
      zoom lensMStoVS v >>= (\v' -> printFileStr valLogFile ("  " ++
        variableName v' ++ " = ")),
      printFile valLogFile (valueOf v),
      printFileStrLn valLogFile ", "] ++ printInputs vs

-- | The variable representing the log file in write mode.
varLogFile :: (SharedProg r) => SVariable r
varLogFile = var "outfile" outfile

-- | The value of the variable representing the log file in write mode.
valLogFile :: (SharedProg r) => SValue r
valLogFile = valueOf varLogFile
