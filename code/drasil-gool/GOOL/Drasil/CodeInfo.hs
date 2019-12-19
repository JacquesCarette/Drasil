module GOOL.Drasil.CodeInfo (CodeInfo(..)) where

import GOOL.Drasil.Symantics (Label, ProgramSym(..), RenderSym, 
  PermanenceSym(..), BodySym(..), BlockSym(..), ControlBlockSym(..), 
  TypeSym(..), VariableSym(..), ValueSym(..), NumericExpression(..), 
  BooleanExpression(..), ValueExpression(..), Selector(..), 
  InternalSelector(..), objMethodCall, FunctionSym(..), SelectorFunction(..),
  StatementSym(..), ControlStatementSym(..), ScopeSym(..), MethodTypeSym(..),
  ParameterSym(..), MethodSym(..), StateVarSym(..), ClassSym(..), ModuleSym(..),
  BlockCommentSym(..))

newtype CodeInfo a = CI {unCI :: a} deriving Eq

instance Functor CodeInfo where
  fmap f (CI x) = CI (f x)

instance Applicative CodeInfo where
  pure = CI
  (CI f) <*> (CI x) = CI (f x)

instance Monad CodeInfo where
  return = CI
  CI x >>= f = f x

instance ProgramSym CodeInfo where
  type Program CodeInfo = GOOLState
  prog _ _ = get

instance RenderSym CodeInfo

instance FileSym CodeInfo where
  type RenderFile CodeInfo = FileData
  fileDoc = G.fileDoc Header cppHdrExt top bottom
  
  docMod = G.docMod

  commentedMod cmnt mod = on3StateValues (\m cmt mn -> if mn then m else 
    on2CodeValues commentedModD m cmt) mod cmnt getCurrMain

instance PermanenceSym CodeInfo where
  type Permanence CodeInfo = BindData
  static_ = toCode $ bd Static staticDocD
  dynamic_ = toCode $ bd Dynamic dynamicDocD

instance BodySym CodeInfo where
  type Body CodeInfo = Doc
  body _ = toState $ toCode empty
  bodyStatements _ = toState $ toCode empty
  oneLiner _ = toState $ toCode empty

  addComments _ _ = toState $ toCode empty

  bodyDoc = unCI

instance BlockSym CodeInfo where
  type Block CodeInfo = Doc
  block _ = toState $ toCode empty

instance TypeSym CodeInfo where
  type Type CodeInfo = TypeData
  bool = cppBoolType
  int = G.int
  float = G.double
  char = G.char
  string = G.string
  infile = cppInfileType
  outfile = cppOutfileType
  listType = G.listType
  listInnerType = G.listInnerType
  obj = G.obj
  enumType = G.enumType
  iterator = cppIterType . listType dynamic_
  void = G.void

  getType = cType . unCI
  getTypeString = typeString . unCI
  getTypeDoc = typeDoc . unCI

instance ControlBlockSym CodeInfo where
  runStrategy _ _ _ _ = toState $ toCode empty

  listSlice' _ _ _ _ _ = toState $ toCode empty

instance VariableSym CodeInfo where
  type Variable CodeInfo = VarData
  var = G.var
  staticVar = G.staticVar
  const _ _ = mkStateVar "" void empty
  extVar _ _ _ = mkStateVar "" void empty
  self _ = mkStateVar "" void empty
  enumVar _ _ = mkStateVar "" void empty
  classVar _ _ = mkStateVar "" void empty
  extClassVar _ _ = mkStateVar "" void empty
  objVar _ _ = mkStateVar "" void empty
  objVarSelf _ _ = mkStateVar "" void empty
  listVar _ _ _ = mkStateVar "" void empty
  listOf _ _ = mkStateVar "" void empty
  iterVar _ _ = mkStateVar "" void empty

  ($->) _ _ = mkStateVar "" void empty
  
  variableBind = varBind . unCI
  variableName = varName . unCI
  variableType = onCodeValue varType
  variableDoc = varDoc . unCI

instance ValueSym CodeInfo where
  type Value CodeInfo = ValData
  litTrue = G.litTrue
  litFalse = G.litFalse
  litChar = G.litChar
  litFloat = G.litFloat
  litInt = G.litInt
  litString = G.litString

  pi = mkStateVal float (text "M_PI")

  ($:) = enumElement

  valueOf = G.valueOf
  arg n = G.arg (litInt $ n+1) argsList
  enumElement en e = mkStateVal (enumType en) (text e)
  
  argsList = G.argsList "argv"

  valueType = onCodeValue valType
  valueDoc = valDoc . unCI

instance NumericExpression CodeInfo where
  (#~) _ = mkStateVal void empty
  (#/^) _ = mkStateVal void empty
  (#|) _ = mkStateVal void empty
  (#+) _ _ = mkStateVal void empty
  (#-) _ _ = mkStateVal void empty
  (#*) _ _ = mkStateVal void empty
  (#/) _ _ = mkStateVal void empty
  (#%) _ _ = mkStateVal void empty
  (#^) _ _ = mkStateVal void empty

  log _ = mkStateVal void empty
  ln _ = mkStateVal void empty
  exp _ = mkStateVal void empty
  sin _ = mkStateVal void empty
  cos _ = mkStateVal void empty
  tan _ = mkStateVal void empty
  csc _ = mkStateVal void empty
  sec _ = mkStateVal void empty
  cot _ = mkStateVal void empty
  arcsin _ = mkStateVal void empty
  arccos _ = mkStateVal void empty
  arctan _ = mkStateVal void empty
  floor _ = mkStateVal void empty
  ceil _ = mkStateVal void empty

instance BooleanExpression CodeInfo where
  (?!) _ = mkStateVal void empty
  (?&&) _ _ = mkStateVal void empty
  (?||) _ _ = mkStateVal void empty

  (?<) _ _ = mkStateVal void empty
  (?<=) _ _ = mkStateVal void empty
  (?>) _ _ = mkStateVal void empty
  (?>=) _ _ = mkStateVal void empty
  (?==) _ _ = mkStateVal void empty
  (?!=) _ _ = mkStateVal void empty
    
instance ValueExpression CodeInfo where
  inlineIf _ _ _ = mkStateVal void empty
  funcApp _ _ _ = mkStateVal void empty
  selfFuncApp _ _ _ _ = mkStateVal void empty
  extFuncApp _ _ _ _ = mkStateVal void empty
  newObj _ _ = mkStateVal void empty
  extNewObj _ _ _ = mkStateVal void empty

  exists _ = mkStateVal void empty
  notNull _ = mkStateVal void empty

instance Selector CodeInfo where
  objAccess _ _ = mkStateVal void empty
  ($.) _ _ = mkStateVal void empty

  selfAccess _ _ = mkStateVal void empty

  listIndexExists _ _ = mkStateVal void empty
  argExists _ = mkStateVal void empty
  
  indexOf _ _ = mkStateVal void empty
  
instance InternalSelector CodeInfo where
  objMethodCall' _ _ _ _ = mkStateVal void empty
  objMethodCallNoParams' _ _ _ = mkStateVal void empty

instance FunctionSym CodeInfo where
  type Function CodeInfo = FuncData
  func _ _ _ = funcFromData empty void
  
  get _ _ = mkStateVal void empty
  set _ _ _ = mkStateVal void empty

  listSize _ = mkStateVal void empty
  listAdd _ _ _ = mkStateVal void empty
  listAppend _ _ = mkStateVal void empty

  iterBegin _ = mkStateVal void empty
  iterEnd _ = mkStateVal void empty

instance SelectorFunction CodeInfo where
  listAccess _ _ = mkStateVal void empty
  listSet _ _ _ = mkStateVal void empty
  at _ _ = mkStateVal void empty

instance StatementSym CodeInfo where
  type Statement CodeInfo = (Doc, Terminator)
  assign _ _ = emptyState
  assignToListIndex _ _ _ = emptyState
  multiAssign _ _ = emptyState
  (&=) _ _ = emptyState
  (&-=) _ _ = emptyState
  (&+=) _ _ = emptyState
  (&++) _ = emptyState
  (&~-) _ = emptyState

  varDec = G.varDec static_ dynamic_
  varDecDef = G.varDecDef
  listDec _ _ = emptyState
  listDecDef _ _ = emptyState
  objDecDef _ _ = emptyState
  objDecNew _ _ = emptyState
  extObjDecNew _ _ _ = emptyState
  objDecNewNoParams _ = emptyState
  extObjDecNewNoParams _ _ = emptyState
  constDecDef = G.constDecDef

  print _ = emptyState
  printLn _ = emptyState
  printStr _ = emptyState
  printStrLn _ = emptyState

  printFile _ _ = emptyState
  printFileLn _ _ = emptyState
  printFileStr _ _ = emptyState
  printFileStrLn _ _ = emptyState

  getInput _ = emptyState
  discardInput = emptyState
  getFileInput _ _ = emptyState
  discardFileInput _ = emptyState

  openFileR _ _ = emptyState
  openFileW _ _ = emptyState
  openFileA _ _ = emptyState
  closeFile _ = emptyState

  getFileInputLine _ _ = emptyState
  discardFileLine _ = emptyState
  stringSplit _ _ _ = emptyState

  stringListVals _ _ = emptyState
  stringListLists _ _ = emptyState

  break = emptyState
  continue = emptyState

  returnState _ = emptyState
  multiReturn _ = emptyState

  valState _ = emptyState

  comment _ = emptyState

  free _ = emptyState

  throw _ = emptyState

  initState _ _ = emptyState
  changeState _ _ = emptyState

  initObserverList _ _ = emptyState
  addObserver _ = emptyState

  inOutCall _ _ _ _ = emptyState
  selfInOutCall _ _ _ _ _ = emptyState
  extInOutCall _ _ _ _ _ = emptyState

  multi _ = emptyState

instance ControlStatementSym CodeInfo where
  ifCond _ _ = emptyState
  ifNoElse _ = emptyState
  switch _ _ _ = emptyState
  switchAsIf _ _ _ = emptyState

  ifExists _ _ _ = emptyState

  for _ _ _ _ = emptyState
  forRange _ _ _ _ _ = emptyState
  forEach _ _ _ = emptyState
  while _ _ = emptyState

  tryCatch _ _ = emptyState

  checkState _ _ _ = emptyState

  notifyObservers _ _ = emptyState

  getFileInputAll _ _ = emptyState

instance ScopeSym CodeInfo where
  type Scope CodeInfo = (Doc, ScopeTag)
  private = toCode (privateDocD, Priv)
  public = toCode (publicDocD, Pub)

instance MethodTypeSym CodeInfo where
  type MethodType CodeInfo = TypeData
  mType t = t
  construct = G.construct

instance ParameterSym CodeInfo where
  type Parameter CodeInfo = ParamData
  param = onStateValue (\v -> paramFromData v (paramDocD v)) . zoom lensMStoGS
  pointerParam = onStateValue (\v -> paramFromData v (cppPointerParamDoc v)) .
    zoom lensMStoGS

instance MethodSym CodeInfo where
  type Method CodeInfo = MethodData
  method = G.method
  getMethod c v = zoom lensMStoGS v >>= (\v' -> method (getterName $ 
    variableName v') c public dynamic_ (toState $ variableType v') [] 
    (toState $ toCode empty))
  setMethod c v = zoom lensMStoGS v >>= (\v' -> method (setterName $ 
    variableName v') c public dynamic_ void [param v] (toState $ toCode empty))
  privMethod = G.privMethod
  pubMethod = G.pubMethod
  constructor n = G.constructor n n
  destructor n vars = on1StateValue1List (\m vs -> toCode $ mthd Pub 
    (emptyIfEmpty (vcat (map (statementDoc . onCodeValue destructSts) vs)) 
    (methodDoc m))) (pubMethod ('~':n) n void [] (toState (toCode empty)) :: MS (CodeInfo (Method CodeInfo))) (map (zoom lensMStoGS) vars)

  docMain = mainFunction

  function = G.function
  mainFunction _ = getPutReturn (setScope Pub) $ toCode $ mthd Pub empty

  docFunc = G.docFunc

  inOutMethod n c = cpphInOut (method n c)

  docInOutMethod n c = G.docInOutFunc (inOutMethod n c)

  inOutFunc n = cpphInOut (function n)

  docInOutFunc n = G.docInOutFunc (inOutFunc n)

instance StateVarSym CodeInfo where
  type StateVar CodeInfo = StateVarData
  stateVar s p v = on2StateValues (\dec -> on3CodeValues svd (onCodeValue snd s)
    (toCode $ stateVarDocD empty (permDoc p) (statementDoc dec)))
    (state $ varDec v) emptyState
  stateVarDef _ s p vr vl = on2StateValues (onCodeValue . svd (snd $ unCI s))
    (cpphStateVarDef empty p vr vl) emptyState
  constVar _ s vr _ = on2StateValues (\v -> on3CodeValues svd (onCodeValue snd 
    s) (on3CodeValues (constVarDocD empty) (bindDoc <$> static_) v 
    endStatement)) vr emptyState
  privMVar = G.privMVar
  pubMVar = G.pubMVar
  pubGVar = G.pubGVar

instance ClassSym CodeInfo where
  type Class CodeInfo = Doc
  buildClass n p _ vs mths = on2StateLists (\vars funcs -> cpphClass n p vars 
    funcs public private blockStart blockEnd endStatement) 
    (map (zoom lensFStoGS) vs) fs
    where fs = map (zoom lensFStoMS) $ mths ++ [destructor n vs]
  enum n es _ = cpphEnum n (enumElementsDocD es enumsEqualInts) blockStart 
    blockEnd endStatement
  privClass = G.privClass
  pubClass = G.pubClass

  docClass = G.docClass

  commentedClass = G.commentedClass

instance ModuleSym CodeInfo where
  type Module CodeInfo = ModData
  buildModule n ls = G.buildModule n (map include ls)

instance BlockCommentSym CodeInfo where
  type BlockComment CodeInfo = Doc
  blockComment lns = on2CodeValues (blockCmtDoc lns) blockCommentStart 
    blockCommentEnd
  docComment = onStateValue (\lns -> on2CodeValues (docCmtDoc lns) 
    docCommentStart docCommentEnd)

  blockCommentDoc = unCI