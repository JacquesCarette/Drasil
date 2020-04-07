module Language.Drasil.Code.Imperative.Logging (
  maybeLog, logBody, loggedMethod, varLogFile
) where

import Language.Drasil.Code.Imperative.DrasilState (DrasilState(..))
import Language.Drasil.CodeSpec hiding (codeSpec)

import GOOL.Drasil (Label, MSBody, MSBlock, SVariable, SValue, MSStatement, 
  ProgramSym, BodySym(..), BlockSym(..), TypeSym(..), VariableSym(..), 
  ValueSym(..), StatementSym(..), lensMStoVS)

import Control.Lens.Zoom (zoom)
import Data.Maybe (maybeToList)
import Control.Applicative ((<$>))
import Control.Monad.Reader (Reader, ask)

maybeLog :: (ProgramSym repr) => SVariable repr ->
  Reader DrasilState [MSStatement repr]
maybeLog v = do
  g <- ask
  l <- chooseLogging (logKind g) v
  return $ maybeToList l

chooseLogging :: (ProgramSym repr) => Logging -> (SVariable repr -> 
  Reader DrasilState (Maybe (MSStatement repr)))
chooseLogging LogVar v = Just <$> loggedVar v
chooseLogging LogAll v = Just <$> loggedVar v
chooseLogging _      _ = return Nothing

loggedVar :: (ProgramSym repr) => SVariable repr -> 
  Reader DrasilState (MSStatement repr)
loggedVar v = do
    g <- ask
    return $ multi [
      openFileA varLogFile (litString $ logName g),
      zoom lensMStoVS v >>= (\v' -> printFileStr valLogFile ("var '" ++ 
        variableName v' ++ "' assigned to ")),
      printFile valLogFile (valueOf v),
      printFileStrLn valLogFile (" in module " ++ currentModule g),
      closeFile valLogFile ]

logBody :: (ProgramSym repr) => Label -> [SVariable repr] -> [MSBlock repr] -> 
  Reader DrasilState (MSBody repr)
logBody n vars b = do
  g <- ask
  let loggedBody LogFunc = loggedMethod (logName g) n vars b
      loggedBody LogAll  = loggedMethod (logName g) n vars b
      loggedBody _       = b
  return $ body $ loggedBody $ logKind g

loggedMethod :: (ProgramSym repr) => Label -> Label -> [SVariable repr] -> 
  [MSBlock repr] -> [MSBlock repr]
loggedMethod lName n vars b = block [
      varDec varLogFile,
      openFileA varLogFile (litString lName),
      printFileStrLn valLogFile ("function " ++ n ++ " called with inputs: {"),
      multi $ printInputs vars,
      printFileStrLn valLogFile "  }",
      closeFile valLogFile ]
      : b
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

varLogFile :: (ProgramSym repr) => SVariable repr
varLogFile = var "outfile" outfile

valLogFile :: (ProgramSym repr) => SValue repr
valLogFile = valueOf varLogFile