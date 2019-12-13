module Language.Drasil.Code.Imperative.GenerateGOOL (
  genModule, genDoxConfig, publicClass, fApp, fAppInOut, mkParam
) where

import Language.Drasil
import Language.Drasil.Code.Imperative.State (DrasilState(..))
import Language.Drasil.Code.Imperative.GOOL.Symantics (AuxiliarySym(..))
import Language.Drasil.CodeSpec (CodeSpec(..), CodeSystInfo(..), Comments(..), 
  Name)
  
import GOOL.Drasil (Label, RenderSym(..), TypeSym(..), 
  VariableSym(..), ValueSym(..), ValueExpression(..), StatementSym(..), 
  ParameterSym(..), MethodSym(..), StateVarSym(..), ClassSym(..), ModuleSym(..),
  CodeType(..), GOOLState, GS, FS, MS, lensMStoGS)

import qualified Data.Map as Map (lookup)
import Data.Maybe (fromMaybe, maybe)
import Control.Lens.Zoom (zoom)
import Control.Monad.Reader (Reader, ask, withReader)

genModule :: (RenderSym repr) => Name -> String
  -> Maybe (Reader DrasilState [MS (repr (Method repr))])
  -> Maybe (Reader DrasilState [FS (repr (Class repr))])
  -> Reader DrasilState (FS (repr (RenderFile repr)))
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

genDoxConfig :: (AuxiliarySym repr) => String -> GOOLState ->
  Reader DrasilState [repr (Auxiliary repr)]
genDoxConfig n s = do
  g <- ask
  let cms = commented g
      v = doxOutput g
  return [doxConfig n s v | not (null cms)]

publicClass :: (RenderSym repr) => String -> Label -> Maybe Label -> 
  [GS (repr (StateVar repr))] -> Reader DrasilState [MS (repr (Method repr))] 
  -> Reader DrasilState (FS (repr (Class repr)))
publicClass desc n l vs mths = do
  g <- ask
  ms <- mths
  return $ if CommentClass `elem` commented g 
    then docClass desc (pubClass n l vs ms) 
    else pubClass n l vs ms

fApp :: (RenderSym repr) => String -> String -> GS (repr (Type repr)) -> 
  [GS (repr (Value repr))] -> Reader DrasilState (GS (repr (Value repr)))
fApp m s t vl = do
  g <- ask
  let cm = currentModule g
  return $ if m /= cm then extFuncApp m s t vl else if Map.lookup s 
    (eMap $ codeSpec g) == Just cm then funcApp s t vl else selfFuncApp m s t vl

fAppInOut :: (RenderSym repr) => String -> String -> [GS (repr (Value repr))] 
  -> [GS (repr (Variable repr))] -> [GS (repr (Variable repr))] -> 
  Reader DrasilState (GS (repr (Statement repr)))
fAppInOut m n ins outs both = do
  g <- ask
  let cm = currentModule g
  return $ if m /= cm then extInOutCall m n ins outs both else if Map.lookup n
    (eMap $ codeSpec g) == Just cm then inOutCall n ins outs both else 
    selfInOutCall m n ins outs both

mkParam :: (RenderSym repr) => GS (repr (Variable repr)) -> 
  MS (repr (Parameter repr))
mkParam v = zoom lensMStoGS v >>= (\v' -> paramFunc (getType $ variableType v') v)
  where paramFunc (List _) = pointerParam
        paramFunc (Object _) = pointerParam
        paramFunc _ = param