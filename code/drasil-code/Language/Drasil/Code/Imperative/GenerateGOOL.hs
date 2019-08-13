module Language.Drasil.Code.Imperative.GenerateGOOL (
  genModule, genDoxConfig, publicClass, fApp, fAppInOut, mkParam
) where

import Language.Drasil
import Language.Drasil.Code.Code as C (CodeType(List, Object))
import Language.Drasil.Code.Imperative.State (State(..))
import Language.Drasil.Code.Imperative.GOOL.Symantics (Label, ProgramSym(..), 
  RenderSym(..), AuxiliarySym(..), StateTypeSym(..), VariableSym(..), 
  ValueSym(..), ValueExpression(..), StatementSym(..), ParameterSym(..),
  MethodSym(..), StateVarSym(..), ClassSym(..), ModuleSym(..))
import Language.Drasil.CodeSpec (CodeSpec(..), CodeSystInfo(..), Comments(..), 
  Name)

import qualified Data.Map as Map (lookup)
import Data.Maybe (fromMaybe, maybe)
import Control.Monad.Reader (Reader, ask, withReader)

genModule :: (RenderSym repr) => Name -> String
  -> Maybe (Reader State [repr (Method repr)])
  -> Maybe (Reader State [repr (Class repr)])
  -> Reader State (repr (RenderFile repr))
genModule n desc maybeMs maybeCs = do
  g <- ask
  let ls = fromMaybe [] (Map.lookup n (dMap $ codeSpec g))
      updateState = withReader (\s -> s { currentModule = n })
      -- Below line of code cannot be simplified because authors has a generic type
      as = case csi (codeSpec g) of CSI {authors = a} -> map name a
  cs <- maybe (return []) updateState maybeCs
  ms <- maybe (return []) updateState maybeMs
  let commMod | CommentMod `elem` commented g                   = docMod desc 
                  as (date g)
              | CommentFunc `elem` commented g && not (null ms) = docMod "" []  
                  (date g)
              | otherwise                                       = id
  return $ commMod $ fileDoc $ buildModule n ls ms cs

genDoxConfig :: (AuxiliarySym repr) => String -> repr (Program repr) ->
  Reader State [repr (Auxiliary repr)]
genDoxConfig n p = do
  g <- ask
  let cms = commented g
  return [doxConfig n p | not (null cms)]

publicClass :: (RenderSym repr) => String -> Label -> Maybe Label -> 
  [repr (StateVar repr)] -> [repr (Method repr)] -> 
  Reader State (repr (Class repr))
publicClass desc n l vs ms = do
  g <- ask
  return $ if CommentClass `elem` commented g 
    then docClass desc (pubClass n l vs ms) 
    else pubClass n l vs ms

fApp :: (RenderSym repr) => String -> String -> repr (StateType repr) -> 
  [repr (Value repr)] -> Reader State (repr (Value repr))
fApp m s t vl = do
  g <- ask
  return $ if m /= currentModule g then extFuncApp m s t vl else funcApp s t vl

fAppInOut :: (RenderSym repr) => String -> String -> [repr (Value repr)] -> 
  [repr (Variable repr)] -> [repr (Variable repr)] -> 
  Reader State (repr (Statement repr))
fAppInOut m n ins outs both = do
  g <- ask
  return $ if m /= currentModule g then extInOutCall m n ins outs both
    else inOutCall n ins outs both

mkParam :: (RenderSym repr) => repr (Variable repr) -> repr (Parameter repr)
mkParam v = paramFunc (getType $ variableType v) v
  where paramFunc (C.List _) = pointerParam
        paramFunc (C.Object _) = pointerParam
        paramFunc _ = stateParam