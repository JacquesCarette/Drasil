module Language.Drasil.Code.Imperative.GenerateGOOL (
  genModule, genDoxConfig, publicClass, publicMethod, publicInOutFunc, fApp, 
  fAppInOut, value, variable
) where

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

publicMethod :: (RenderSym repr, HasUID c, HasCodeType c, CodeIdea c) => 
  repr (MethodType repr) -> Label -> String -> [c] -> Maybe String -> 
  [repr (Block repr)] -> Reader State (repr (Method repr))
publicMethod = genMethod public static_

publicInOutFunc :: (RenderSym repr, HasUID c, HasCodeType c, CodeIdea c) => 
  Label -> String -> [c] -> [c] -> [c] -> [repr (Block repr)] -> 
  Reader State (repr (Method repr))
publicInOutFunc = genInOutFunc public static_

genMethod :: (RenderSym repr, HasUID c, HasCodeType c, CodeIdea c) => 
  repr (Scope repr) -> repr (Permanence repr) -> repr (MethodType repr) -> 
  Label -> String -> [c] -> Maybe String -> [repr (Block repr)] -> 
  Reader State (repr (Method repr))
genMethod s pr t n desc p r b = do
  g <- ask
  vars <- mapM mkVar p
  let ps = map mkParam vars
      doLog = logKind g
      loggedBody LogFunc = loggedMethod (logName g) n vars b
      loggedBody LogAll  = loggedMethod (logName g) n vars b
      loggedBody _       = b
      bod = body $ loggedBody doLog
      fn = function n s pr t ps bod
  pComms <- mapM (paramComment . (^. uid)) p
  return $ if CommentFunc `elem` commented g
    then docFunc desc pComms r fn else fn

genInOutFunc :: (RenderSym repr, HasUID c, HasCodeType c, CodeIdea c) => 
  repr (Scope repr) -> repr (Permanence repr) -> Label -> String -> [c] ->
  [c] -> [c] -> [repr (Block repr)] -> Reader State (repr (Method repr))
genInOutFunc s pr n desc ins outs both b = do
  g <- ask
  inVs <- mapM mkVar ins
  outVs <- mapM mkVar outs
  bothVs <- mapM mkVar both
  let doLog = logKind g
      loggedBody LogFunc = loggedMethod (logName g) n inVs b
      loggedBody LogAll  = loggedMethod (logName g) n inVs b
      loggedBody _       = b
      bod = body $ loggedBody doLog
      fn = inOutFunc n s pr inVs outVs bothVs bod
  pComms <- mapM (paramComment . (^. uid)) ins
  oComms <- mapM (paramComment . (^. uid)) outs
  bComms <- mapM (paramComment . (^. uid)) both
  return $ if CommentFunc `elem` commented g 
    then docInOutFunc desc pComms oComms bComms fn else fn

loggedMethod :: (RenderSym repr) => Label -> Label -> [repr (Variable repr)] -> 
  [repr (Block repr)] -> [repr (Block repr)]
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

value :: (RenderSym repr) => UID -> String -> repr (StateType repr) -> 
  Reader State (repr (Value repr))
value u s t = do
  g <- ask
  let cs = codeSpec g
      mm = constMap cs
  maybe (do { v <- variable s t; return $ valueOf v }) 
    (convExpr . codeEquat) (Map.lookup u mm)

variable :: (RenderSym repr) => String -> repr (StateType repr) -> 
  Reader State (repr (Variable repr))
variable s t = do
  g <- ask
  let cs = csi $ codeSpec g
  if s `elem` map codeName (inputs cs) 
    then inputVariable (inStruct g) s t
    else return $ var s t
  
inputVariable :: (RenderSym repr) => Structure -> String -> 
  repr (StateType repr) -> Reader State (repr (Variable repr))
inputVariable Unbundled s t = return $ var s t
inputVariable Bundled s t = do
  ip <- mkVar (codevar inParams)
  return $ ip $-> var s t