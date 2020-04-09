module Language.Drasil.Code.Imperative.GenerateGOOL (ClassType(..),
  genModuleWithImports, genModule, genDoxConfig, primaryClass, auxClass, fApp, 
  ctorCall, fAppInOut
) where

import Language.Drasil
import Language.Drasil.Code.Imperative.DrasilState (DrasilState(..))
import Language.Drasil.Code.Imperative.GOOL.ClassInterface (AuxiliarySym(..))
import Language.Drasil.CodeSpec (CodeSpec(..), Comments(..))
import Language.Drasil.Mod (Name)
  
import GOOL.Drasil (Label, SFile, VSType, SVariable, SValue, MSStatement, 
  SMethod, CSStateVar, SClass, ProgramSym, FileSym(..), ValueExpression(..), 
  FuncAppStatement(..), ClassSym(..), ModuleSym(..), GOOLState)

import qualified Data.Map as Map (lookup)
import Data.Maybe (catMaybes)
import Control.Monad.Reader (Reader, ask, withReader)

genModuleWithImports :: (ProgramSym repr) => Name -> String -> [String] -> 
  [Reader DrasilState (Maybe (SMethod repr))] -> 
  [Reader DrasilState (Maybe (SClass repr))] -> Reader DrasilState (SFile repr)
genModuleWithImports n desc is maybeMs maybeCs = do
  g <- ask
  let updateState = withReader (\s -> s { currentModule = n })
      -- Below line of code cannot be simplified because authors has a generic type
      as = case codeSpec g of CodeSpec {authors = a} -> map name a
  cs <- mapM updateState maybeCs
  ms <- mapM updateState maybeMs
  let commMod | CommentMod `elem` commented g                   = docMod desc 
                  as (date g)
              | CommentFunc `elem` commented g && not (null ms) = docMod "" []  
                  (date g)
              | otherwise                                       = id
  return $ commMod $ fileDoc $ buildModule n is (catMaybes ms) (catMaybes cs)

genModule :: (ProgramSym repr) => Name -> String -> 
  [Reader DrasilState (Maybe (SMethod repr))] -> 
  [Reader DrasilState (Maybe (SClass repr))] -> Reader DrasilState (SFile repr)
genModule n desc = genModuleWithImports n desc []

genDoxConfig :: (AuxiliarySym repr) => String -> GOOLState ->
  Reader DrasilState [repr (Auxiliary repr)]
genDoxConfig n s = do
  g <- ask
  let cms = commented g
      v = doxOutput g
  return [doxConfig n s v | not (null cms)]

data ClassType = Primary | Auxiliary

mkClass :: (ProgramSym repr) => ClassType -> String -> Label -> Maybe Label -> 
  [CSStateVar repr] -> Reader DrasilState [SMethod repr] -> 
  Reader DrasilState (SClass repr)
mkClass s desc n i vs mths = do
  g <- ask
  ms <- mths
  let getFunc Primary = getFunc' i
      getFunc Auxiliary = extraClass n Nothing
      getFunc' Nothing = buildClass n Nothing
      getFunc' (Just intfc) = implementingClass n [intfc]
      f = getFunc s
  return $ if CommentClass `elem` commented g 
    then docClass desc (f vs ms) 
    else f vs ms

primaryClass :: (ProgramSym repr) => String -> Label -> Maybe Label -> 
  [CSStateVar repr] -> Reader DrasilState [SMethod repr] -> 
  Reader DrasilState (SClass repr)
primaryClass = mkClass Primary

auxClass :: (ProgramSym repr) => String -> Label -> Maybe Label -> 
  [CSStateVar repr] -> Reader DrasilState [SMethod repr] -> 
  Reader DrasilState (SClass repr)
auxClass = mkClass Auxiliary

fApp :: (ProgramSym repr) => String -> String -> VSType repr -> [SValue repr] 
  -> [(SVariable repr, SValue repr)] -> Reader DrasilState (SValue repr)
fApp m s t vl ns = do
  g <- ask
  let cm = currentModule g
  return $ if m /= cm then extFuncAppMixedArgs m s t vl ns else if Map.lookup s 
    (eMap g) == Just cm then funcAppMixedArgs s t vl ns else 
    selfFuncAppMixedArgs s t vl ns

ctorCall :: (ProgramSym repr) => String -> VSType repr -> [SValue repr] -> 
  [(SVariable repr, SValue repr)] -> Reader DrasilState (SValue repr)
ctorCall m t vl ns = do
  g <- ask
  let cm = currentModule g
  return $ if m /= cm then extNewObjMixedArgs m t vl ns else 
    newObjMixedArgs t vl ns

fAppInOut :: (ProgramSym repr) => String -> String -> [SValue repr] -> 
  [SVariable repr] -> [SVariable repr] -> Reader DrasilState (MSStatement repr)
fAppInOut m n ins outs both = do
  g <- ask
  let cm = currentModule g
  return $ if m /= cm then extInOutCall m n ins outs both else if Map.lookup n
    (eMap g) == Just cm then inOutCall n ins outs both else 
    selfInOutCall n ins outs both
