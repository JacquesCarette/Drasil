module Language.Drasil.Code.Import where

import Prelude hiding (return, id)
import Control.Lens hiding (makeFields, assign, uses)

import qualified Language.Drasil.Expr as E
import Language.Drasil.Chunk.Method
import Language.Drasil.Chunk.Module
import Language.Drasil.Chunk.Eq
import Language.Drasil.Expr.Extract
import Language.Drasil.Code.Imperative.AST as A
import Language.Drasil.Code.Imperative.Helpers
import Language.Drasil.Space as S
import Language.Drasil.Chunk as C
import Language.Drasil.Chunk.Quantity as Q

toCode :: NamedIdea c => c -> [ModuleChunk] -> AbstractCode
toCode prog mcs = AbsCode $ Pack (prog ^. id) (makeModules mcs [])

makeModules :: [ModuleChunk] -> [Class] -> [Class]
makeModules [] cs = cs
makeModules (mc:mcs) cs =
  if generated mc then
    -- if dependencies haven't been built into Class types, postpone
    if checkDependencies (uses mc) cs
    then makeModules mcs (makeModule mc cs:cs)
    else makeModules (mcs ++ [mc]) cs
  else makeModules mcs cs

-- check if all dependencies have already been built into Class types
checkDependencies :: [ModuleChunk] -> [Class] -> Bool
checkDependencies [] _ = True
checkDependencies (mc:mcs) classes =
  if generated mc
  then checkDependencies' (makeClassNameValid $ (modcc mc) ^. id) classes &&
         checkDependencies mcs classes
  else checkDependencies mcs classes
  where checkDependencies' :: String -> [Class] -> Bool
        checkDependencies' _ [] = False
        checkDependencies' mn ((Class cn _ _ _ _):cs) =
          if mn == cn
          then True
          else checkDependencies' mn cs
        checkDependencies' mn ((MainClass cn _ _):cs) =
          if mn == cn
          then True
          else checkDependencies' mn cs
        checkDependencies' mn ((Enum cn _ _):cs) =
          if mn == cn
          then True
          else checkDependencies' mn cs

makeModule :: ModuleChunk -> [Class] -> Class
makeModule mc cs = pubClass (makeClassNameValid $ (modcc mc) ^. id) noParent
  (makeFields (field mc)) (makeMethods (method mc) cs)

makeFields :: [VarChunk] -> [StateVar]
makeFields = map makeField

makeField :: VarChunk -> StateVar
makeField vc = pubMVar 2 (makeType (vc ^. Q.typ)) (vc ^. id)

makeMethods :: [MethodChunk] -> [Class] -> [Method]
makeMethods mcs cs = map (\x -> makeMethod x cs) mcs

makeMethod :: MethodChunk -> [Class] -> Method
makeMethod meth@(MeC { mType = MCalc (EC a b)}) _ =
  pubMethod (A.typ $ makeType $ a ^. Q.typ) ("calc_" ++ ((methcc meth) ^. id))
  (map (\vc -> param (vc ^. id) (makeType (vc ^. Q.typ))) (vars b))
  (oneLiner $ return $ makeExpr b)

makeMethod      (MeC { mType = MInput IOStd vc}) _ =
  pubMethod (A.typ $ makeType $ vc ^. Q.typ) ("in_" ++ (vc ^. id)) []
  [ Block [ varDec (vc ^. id) (makeType $ vc ^. Q.typ),
            assign (Var (vc ^. id)) (Input)
          ]
  ]

makeMethod      mc@(MeC { mType = MInput (IOFile f) vc}) classList =
  pubMethod (Void) ((methcc mc) ^. id)
    [param (vc ^. id) (makeType $ vc ^. Q.typ)]
  [ Block [ varDec ("inFile") (infile),
            ValState $ ObjAccess (Var "inFile") (FileOpen f)
          ],
    Block (makeAssignments $ vc ^. Q.typ)
  ]
  where makeAssignments :: Space -> [Statement]
        makeAssignments (S.Obj s) = map (\x -> assign x
          (InputFile (Var "inFile")))
          (map (ObjVar (Var $ vc ^. id))
            (getClassVars $ findClass s classList))
        makeAssignments _ = [assign (Var (vc ^. id))
          (InputFile (Var "inFile"))]

makeMethod mc@(MeC { mType = MOutput (IOFile f) vcs}) classList =
  pubMethod (Void) ((methcc mc) ^. id)
    (map (\x -> param (x ^. id) (makeType $ x ^. Q.typ)) vcs)
  [ Block [ varDec ("outFile") (outfile),
            ValState $ ObjAccess (Var "outFile") (FileOpen f)
          ],
    Block (map (\x -> PrintFileState (Var "outFile") True
      (makeType (x ^. Q.typ)) (Var (x ^. id))) vcs)
  ]

makeMethod (MeC { mType = MCustom b}) _ =
  MainMethod b

makeType :: Space -> StateType
makeType S.Rational = float
makeType S.Boolean  = bool
makeType S.Integer  = int
makeType S.Char     = char
makeType S.String   = string
makeType (S.Obj s)  = Type (makeClassNameValid s)

getObjName :: Space -> String
getObjName (S.Obj s)  = makeClassNameValid s
getObjName _ = error "Must be Obj type"

makeExpr :: E.Expr -> Value
makeExpr (E.V v)    = Var v
makeExpr (E.Dbl d)  = litFloat d
makeExpr (E.Int i)  = litInt i
makeExpr (E.C c)    = Var (c ^. id)
makeExpr (b E.:^ e) = (makeExpr b) #^ (makeExpr e)
makeExpr (b E.:* e) = (makeExpr b) #* (makeExpr e)
makeExpr (b E.:/ e) = (makeExpr b) #/ (makeExpr e)
makeExpr (b E.:+ e) = (makeExpr b) #+ (makeExpr e)
makeExpr (b E.:- e) = (makeExpr b) #- (makeExpr e)
makeExpr _          = error "Unimplemented expression in code generation"

findClass :: String -> [Class] -> Class
findClass _ [] = error "Class not found"
findClass n (c:cs) = if (makeClassNameValid n) == className c then c else findClass n cs

getClassVars :: Class -> [Value]
getClassVars (Enum _ _ _) = error "Enum does not have vars"
getClassVars c = map (\(StateVar l _ _ _ _) -> Var l) (classVars c)