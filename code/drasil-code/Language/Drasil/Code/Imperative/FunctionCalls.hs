module Language.Drasil.Code.Imperative.FunctionCalls (
  getInputCall, getDerivedCall, getConstraintCall, getCalcCall, getOutputCall
) where

getInputCall :: (RenderSym repr) => Reader State (Maybe (repr (Statement repr)))
getInputCall = getInOutCall "get_input" getInputFormatIns getInputFormatOuts 
  (return [])

getDerivedCall :: (RenderSym repr) => Reader State 
  (Maybe (repr (Statement repr)))
getDerivedCall = getInOutCall "derived_values" getDerivedIns getDerivedOuts 
  (return [])

getConstraintCall :: (RenderSym repr) => Reader State 
  (Maybe (repr (Statement repr)))
getConstraintCall = do
  val <- getFuncCall "input_constraints" void getConstraintParams
  return $ fmap valState val

getCalcCall :: (RenderSym repr) => CodeDefinition -> Reader State 
  (Maybe (repr (Statement repr)))
getCalcCall c = do
  g <- ask
  val <- getFuncCall (codeName c) (convType $ codeType c) (getCalcParams c)
  v <- maybe (error $ (c ^. uid) ++ " missing from VarMap") mkVar 
    (Map.lookup (c ^. uid) (vMap $ codeSpec g))
  l <- maybeLog v
  return $ fmap (multi . (: l) . varDecDef v) val

getOutputCall :: (RenderSym repr) => Reader State 
  (Maybe (repr (Statement repr)))
getOutputCall = do
  val <- getFuncCall "write_output" void getOutputParams
  return $ fmap valState val

getFuncCall :: (RenderSym repr, HasUID c, HasCodeType c, CodeIdea c) => String 
  -> repr (StateType repr) -> Reader State [c] -> 
  Reader State (Maybe (repr (Value repr)))
getFuncCall n t funcPs = do
  g <- ask
  let getCall Nothing = return Nothing
      getCall (Just m) = do
        cs <- funcPs
        pvals <- mapM mkVal cs
        val <- fApp m n t pvals
        return $ Just val
  getCall $ Map.lookup n (eMap $ codeSpec g)

getInOutCall :: (RenderSym repr, HasCodeType c, CodeIdea c) => String -> 
  Reader State [c] -> Reader State [c] -> Reader State [c] -> 
  Reader State (Maybe (repr (Statement repr)))
getInOutCall n inFunc outFunc bothFunc = do
  g <- ask
  let getCall Nothing = return Nothing
      getCall (Just m) = do
        ins' <- inFunc
        ins <- mapM mkVar ins'
        outs' <- outFunc
        outs <- mapM mkVar outs'
        both' <- bothFunc
        both <- mapM mkVar both'
        stmt <- fAppInOut m n (map valueOf ins) outs both
        return $ Just stmt
  getCall $ Map.lookup n (eMap $ codeSpec g)