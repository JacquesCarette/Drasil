module Language.Drasil.Code.Imperative.Logging (
  maybeLog, logBody, loggedMethod, varLogFile
) where

import Language.Drasil.Code.Imperative.DrasilState (GenState, DrasilState(..))
import Language.Drasil.Choices (Logging(..))

import GOOL.Drasil (Label, MSBody, MSBlock, SVariable, SValue, MSStatement, 
  OOProg, BodySym(..), BlockSym(..), TypeSym(..), VariableSym(..), 
  VariableElim(..), Literal(..), VariableValue(..), StatementSym(..), 
  DeclStatement(..), IOStatement(..), lensMStoVS)

import Control.Lens.Zoom (zoom)
import Data.Maybe (maybeToList)
import Control.Applicative ((<$>))
import Control.Monad.State (get)

maybeLog :: (OOProg r) => SVariable r -> GenState [MSStatement r]
maybeLog v = do
  g <- get
  l <- chooseLogging (logKind g) v
  return $ maybeToList l

chooseLogging :: (OOProg r) => [Logging] -> (SVariable r -> 
  GenState (Maybe (MSStatement r)))
chooseLogging l v = if LogVar `elem` l then Just <$> loggedVar v 
  else return Nothing

loggedVar :: (OOProg r) => SVariable r -> GenState (MSStatement r)
loggedVar v = do
  g <- get
  return $ multi [
    openFileA varLogFile (litString $ logName g),
    zoom lensMStoVS v >>= (\v' -> printFileStr valLogFile ("var '" ++ 
      variableName v' ++ "' assigned ")),
    printFile valLogFile (valueOf v),
    printFileStrLn valLogFile (" in module " ++ currentModule g),
    closeFile valLogFile ]

logBody :: (OOProg r) => Label -> [SVariable r] -> [MSBlock r] -> 
  GenState (MSBody r)
logBody n vars b = do
  g <- get
  let loggedBody l = if LogFunc `elem` l then loggedMethod (logName g) n vars b
        else b
  return $ body $ loggedBody $ logKind g

loggedMethod :: (OOProg r) => Label -> Label -> [SVariable r] -> 
  [MSBlock r] -> [MSBlock r]
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

varLogFile :: (OOProg r) => SVariable r
varLogFile = var "outfile" outfile

valLogFile :: (OOProg r) => SValue r
valLogFile = valueOf varLogFile