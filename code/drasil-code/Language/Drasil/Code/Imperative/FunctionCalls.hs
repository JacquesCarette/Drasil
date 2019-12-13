module Language.Drasil.Code.Imperative.FunctionCalls (
  getAllInputCalls, getInputCall, getDerivedCall, getConstraintCall, 
  getCalcCall, getOutputCall
) where

import Language.Drasil
import Language.Drasil.Code.Imperative.GenerateGOOL (fApp, fAppInOut)
import Language.Drasil.Code.Imperative.Import (mkVal, mkVar)
import Language.Drasil.Code.Imperative.Logging (maybeLog)
import Language.Drasil.Code.Imperative.Parameters (getCalcParams, 
  getConstraintParams, getDerivedIns, getDerivedOuts, getInputFormatIns, 
  getInputFormatOuts, getOutputParams)
import Language.Drasil.Code.Imperative.State (DrasilState(..))
import Language.Drasil.Chunk.Code (CodeIdea(codeName), codeType)
import Language.Drasil.Chunk.CodeDefinition (CodeDefinition)
import Language.Drasil.Chunk.CodeQuantity (HasCodeType)
import Language.Drasil.CodeSpec (CodeSpec(..))

import GOOL.Drasil (RenderSym(..), TypeSym(..), ValueSym(..), 
  StatementSym(..), GS, convType)

import Data.List ((\\), intersect)
import qualified Data.Map as Map (lookup)
import Data.Maybe (maybe, catMaybes)
import Control.Monad.Reader (Reader, ask)
import Control.Lens ((^.))

getAllInputCalls :: (RenderSym repr) => Reader DrasilState 
  [GS (repr (Statement repr))]
getAllInputCalls = do
  gi <- getInputCall
  dv <- getDerivedCall
  ic <- getConstraintCall
  return $ catMaybes [gi, dv, ic]

getInputCall :: (RenderSym repr) => Reader DrasilState 
  (Maybe (GS (repr (Statement repr))))
getInputCall = getInOutCall "get_input" getInputFormatIns getInputFormatOuts

getDerivedCall :: (RenderSym repr) => Reader DrasilState 
  (Maybe (GS (repr (Statement repr))))
getDerivedCall = getInOutCall "derived_values" getDerivedIns getDerivedOuts

getConstraintCall :: (RenderSym repr) => Reader DrasilState 
  (Maybe (GS (repr (Statement repr))))
getConstraintCall = do
  val <- getFuncCall "input_constraints" void getConstraintParams
  return $ fmap valState val

getCalcCall :: (RenderSym repr) => CodeDefinition -> Reader DrasilState 
  (Maybe (GS (repr (Statement repr))))
getCalcCall c = do
  g <- ask
  val <- getFuncCall (codeName c) (convType $ codeType c) (getCalcParams c)
  v <- maybe (error $ (c ^. uid) ++ " missing from VarMap") mkVar 
    (Map.lookup (c ^. uid) (vMap $ codeSpec g))
  l <- maybeLog v
  return $ fmap (multi . (: l) . varDecDef v) val

getOutputCall :: (RenderSym repr) => Reader DrasilState 
  (Maybe (GS (repr (Statement repr))))
getOutputCall = do
  val <- getFuncCall "write_output" void getOutputParams
  return $ fmap valState val

getFuncCall :: (RenderSym repr, HasUID c, HasCodeType c, CodeIdea c) => String 
  -> GS (repr (Type repr)) -> Reader DrasilState [c] -> 
  Reader DrasilState (Maybe (GS (repr (Value repr))))
getFuncCall n t funcPs = do
  mm <- getCall n
  let getFuncCall' Nothing = return Nothing
      getFuncCall' (Just m) = do
        cs <- funcPs
        pvals <- mapM mkVal cs
        val <- fApp m n t pvals
        return $ Just val
  getFuncCall' mm

getInOutCall :: (RenderSym repr, HasCodeType c, CodeIdea c, Eq c) => String -> 
  Reader DrasilState [c] -> Reader DrasilState [c] ->
  Reader DrasilState (Maybe (GS (repr (Statement repr))))
getInOutCall n inFunc outFunc = do
  mm <- getCall n
  let getInOutCall' Nothing = return Nothing
      getInOutCall' (Just m) = do
        ins' <- inFunc
        outs' <- outFunc
        ins <- mapM mkVar (ins' \\ outs')
        outs <- mapM mkVar (outs' \\ ins')
        both <- mapM mkVar (ins' `intersect` outs')
        stmt <- fAppInOut m n (map valueOf ins) outs both
        return $ Just stmt
  getInOutCall' mm

getCall :: String -> Reader DrasilState (Maybe String)
getCall n = do
  g <- ask
  let getCallExported Nothing = getCallDefined (Map.lookup n $ defMap $ 
        codeSpec g)
      getCallExported m = return m
      getCallDefined Nothing = return Nothing
      getCallDefined (Just m) = if m == currentModule g then return (Just m)
        else return Nothing
  getCallExported $ Map.lookup n (eMap $ codeSpec g)
