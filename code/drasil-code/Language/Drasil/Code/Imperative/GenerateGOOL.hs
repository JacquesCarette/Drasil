module Language.Drasil.Code.Imperative.GenerateGOOL (ClassType(..),
  genModule, genDoxConfig, primaryClass, auxClass, fApp, fAppInOut, mkParam
) where

import Language.Drasil
import Language.Drasil.Code.Imperative.DrasilState (DrasilState(..))
import Language.Drasil.Code.Imperative.GOOL.Symantics (AuxiliarySym(..))
import Language.Drasil.CodeSpec (CodeSpec(..), CodeSystInfo(..), Comments(..), 
  Name)
  
import GOOL.Drasil (Label, ProgramSym, FileSym(..), TypeSym(..), 
  VariableSym(..), ValueSym(..), ValueExpression(..), StatementSym(..), 
  ParameterSym(..), MethodSym(..), StateVarSym(..), ClassSym(..), ModuleSym(..),
  CodeType(..), GOOLState, FS, CS, MS, VS, lensMStoVS)

import Control.Lens.Zoom (zoom)
import qualified Data.Map as Map (lookup)
import Data.Maybe (catMaybes)
import Control.Monad.Reader (Reader, ask, withReader)

genModule :: (ProgramSym repr) => Name -> String
  -> [Reader DrasilState (Maybe (MS (repr (Method repr))))]
  -> [Reader DrasilState (Maybe (CS (repr (Class repr))))]
  -> Reader DrasilState (FS (repr (RenderFile repr)))
genModule n desc maybeMs maybeCs = do
  g <- ask
  let updateState = withReader (\s -> s { currentModule = n })
      -- Below line of code cannot be simplified because authors has a generic type
      as = case csi (codeSpec g) of CSI {authors = a} -> map name a
  cs <- mapM updateState maybeCs
  ms <- mapM updateState maybeMs
  let commMod | CommentMod `elem` commented g                   = docMod desc 
                  as (date g)
              | CommentFunc `elem` commented g && not (null ms) = docMod "" []  
                  (date g)
              | otherwise                                       = id
  return $ commMod $ fileDoc $ buildModule n [] (catMaybes ms) (catMaybes cs)

genDoxConfig :: (AuxiliarySym repr) => String -> GOOLState ->
  Reader DrasilState [repr (Auxiliary repr)]
genDoxConfig n s = do
  g <- ask
  let cms = commented g
      v = doxOutput g
  return [doxConfig n s v | not (null cms)]

data ClassType = Primary | Auxiliary

mkClass :: (ProgramSym repr) => ClassType -> String -> Label -> Maybe Label -> 
  [CS (repr (StateVar repr))] -> Reader DrasilState [MS (repr (Method repr))] 
  -> Reader DrasilState (CS (repr (Class repr)))
mkClass s desc n l vs mths = do
  g <- ask
  ms <- mths
  let getFunc Primary = buildClass
      getFunc Auxiliary = extraClass
      f = getFunc s
  return $ if CommentClass `elem` commented g 
    then docClass desc (f n l vs ms) 
    else f n l vs ms

primaryClass :: (ProgramSym repr) => String -> Label -> Maybe Label -> 
  [CS (repr (StateVar repr))] -> Reader DrasilState [MS (repr (Method repr))] 
  -> Reader DrasilState (CS (repr (Class repr)))
primaryClass = mkClass Primary

auxClass :: (ProgramSym repr) => String -> Label -> Maybe Label -> 
  [CS (repr (StateVar repr))] -> Reader DrasilState [MS (repr (Method repr))] 
  -> Reader DrasilState (CS (repr (Class repr)))
auxClass = mkClass Auxiliary

fApp :: (ProgramSym repr) => String -> String -> VS (repr (Type repr)) -> 
  [VS (repr (Value repr))] -> Reader DrasilState (VS (repr (Value repr)))
fApp m s t vl = do
  g <- ask
  let cm = currentModule g
  return $ if m /= cm then extFuncApp m s t vl else if Map.lookup s 
    (eMap $ codeSpec g) == Just cm then funcApp s t vl else selfFuncApp s t vl

fAppInOut :: (ProgramSym repr) => String -> String -> [VS (repr (Value repr))] 
  -> [VS (repr (Variable repr))] -> [VS (repr (Variable repr))] -> 
  Reader DrasilState (MS (repr (Statement repr)))
fAppInOut m n ins outs both = do
  g <- ask
  let cm = currentModule g
  return $ if m /= cm then extInOutCall m n ins outs both else if Map.lookup n
    (eMap $ codeSpec g) == Just cm then inOutCall n ins outs both else 
    selfInOutCall n ins outs both

mkParam :: (ProgramSym repr) => VS (repr (Variable repr)) -> 
  MS (repr (Parameter repr))
mkParam v = zoom lensMStoVS v >>= (\v' -> paramFunc (getType $ variableType v') 
  v)
  where paramFunc (List _) = pointerParam
        paramFunc (Object _) = pointerParam
        paramFunc _ = param