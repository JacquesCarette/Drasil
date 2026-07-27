module Language.Drasil.Code.Imperative.Logging (
  logBody, loggedMethod, varLogFile
) where

import Control.Lens ((^.))
import Control.Lens.Zoom (zoom)
import Control.Monad.State (get)

import Language.Drasil.Code.Imperative.DrasilState (GenState, HasChoices(..))
import Language.Drasil.Choices (Logging(..))

import Drasil.GOOL (Label, Body, Block, SVariable, SValue, MS, BodySym(..),
  BlockSym(..), TypeSym(..), var, VariableElim(..), Literal(..),
  VariableValue(..), StatementSym(..), DeclStatement(..), FileHandling(..),
  PrintFile(..), lensMStoVS, ScopeSym(..), VariableSym)

-- | Generates the body of a function with the given name, list of parameters,
-- and blocks to include in the body. If the user chose to turn on logging of
-- function calls, statements that log how the function was called are added to
-- the beginning of the body.
logBody
  ::
    ( Literal r
    , VariableValue r
    , DeclStatement r stmt
    , FileHandling r stmt
    , PrintFile r stmt
    , BodySym r stmt
    , VariableElim r
    )
  => Label -> [SVariable r] -> [MS (r Block)] -> GenState (MS (r Body))
logBody n vars b = do
  g <- get
  return $ body $
    [loggedMethod (g ^. logName) n vars | LogFunc `elem` g ^. logKind] ++ b

-- | Generates a block that logs, to the given 'FilePath', the name of a function,
-- and the names and values of the passed list of variables. Intended to be
-- used as the first block in the function, to log that it was called and what
-- inputs it was called with.
loggedMethod
  ::
    ( Literal r
    , VariableValue r
    , DeclStatement r stmt
    , FileHandling r stmt
    , PrintFile r stmt
    , BlockSym r stmt
    , VariableElim r
    )
  => FilePath -> Label -> [SVariable r] -> MS (r Block)
loggedMethod lName n vars = block [
      varDec varLogFile local,
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
varLogFile :: (VariableSym r) => SVariable r
varLogFile = var "outfile" outfile

-- | The value of the variable representing the log file in write mode.
valLogFile :: (VariableValue r) => SValue r
valLogFile = valueOf varLogFile
