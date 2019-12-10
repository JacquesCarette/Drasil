module Language.Drasil.Code.Imperative.Logging (
  maybeLog, logBody, loggedMethod, varLogFile
) where

import Language.Drasil.Code.Imperative.State (DrasilState(..))
import Language.Drasil.CodeSpec hiding (codeSpec, Mod(..))

import GOOL.Drasil (Label, RenderSym(..), BodySym(..), BlockSym(..), 
  TypeSym(..), VariableSym(..), ValueSym(..), StatementSym(..), GS)

import Data.Maybe (maybeToList)
import Control.Applicative ((<$>))
import Control.Monad.Reader (Reader, ask)

maybeLog :: (RenderSym repr) => repr (Variable repr) ->
  Reader DrasilState [GS (repr (Statement repr))]
maybeLog v = do
  g <- ask
  l <- chooseLogging (logKind g) v
  return $ maybeToList l

chooseLogging :: (RenderSym repr) => Logging -> (repr (Variable repr) -> 
  Reader DrasilState (Maybe (GS (repr (Statement repr)))))
chooseLogging LogVar v = Just <$> loggedVar v
chooseLogging LogAll v = Just <$> loggedVar v
chooseLogging _      _ = return Nothing

loggedVar :: (RenderSym repr) => repr (Variable repr) -> 
  Reader DrasilState (GS (repr (Statement repr)))
loggedVar v = do
    g <- ask
    return $ multi [
      openFileA varLogFile (litString $ logName g),
      printFileStr valLogFile ("var '" ++ variableName v ++ "' assigned to "),
      printFile valLogFile (valueOf v),
      printFileStrLn valLogFile (" in module " ++ currentModule g),
      closeFile valLogFile ]

logBody :: (RenderSym repr) => Label -> [repr (Variable repr)] -> 
  [GS (repr (Block repr))] -> Reader DrasilState (GS (repr (Body repr)))
logBody n vars b = do
  g <- ask
  let loggedBody LogFunc = loggedMethod (logName g) n vars b
      loggedBody LogAll  = loggedMethod (logName g) n vars b
      loggedBody _       = b
  return $ body $ loggedBody $ logKind g

loggedMethod :: (RenderSym repr) => Label -> Label -> [repr (Variable repr)] -> 
  [GS (repr (Block repr))] -> [GS (repr (Block repr))]
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
      printFileStr valLogFile ("  " ++ variableName v ++ " = "), 
      printFileLn valLogFile (valueOf v)]
    printInputs (v:vs) = [
      printFileStr valLogFile ("  " ++ variableName v ++ " = "), 
      printFile valLogFile (valueOf v), 
      printFileStrLn valLogFile ", "] ++ printInputs vs

varLogFile :: (RenderSym repr) => repr (Variable repr)
varLogFile = var "outfile" outfile

valLogFile :: (RenderSym repr) => repr (Value repr)
valLogFile = valueOf varLogFile