{-# LANGUAGE GADTs #-}

module GOOL.Drasil.Data (Boolean, Other, Pair(..), pairList, Terminator(..), 
  ScopeTag(..), FileType(..), BindData(..), bd, FileData(..), fileD, file, 
  srcFile, hdrFile, isSource, isHeader, updateFileMod, FuncData(..), fd, 
  TypedFunc(..), funcType, funcDoc, ModData(..), md, updateModDoc, 
  MethodData(..), mthd, OpData(..), od, ParamData(..), pd, updateParamDoc, 
  ProgData(..), progD, emptyProg, StateVarData(..), svd, TypeData(..), td, btd, 
  ltd, TypedType(..), cType, typeString, typeDoc, updateTypedType, ValData(..), 
  vd, updateValDoc, TypedValue(..), otherVal, boolVal, valPrec, valType, valDoc,
  toOtherVal, Binding(..), VarData(..), vard, TypedVar(..), getVarData, 
  otherVar, varBind, varName, varType, varDoc, toOtherVar, typeToFunc, 
  typeToVal, typeToVar, funcToType, valToType, varToType
) where

import GOOL.Drasil.CodeType (CodeType(..))

import Control.Applicative (liftA2)
import Prelude hiding (LT,(<>))
import Text.PrettyPrint.HughesPJ (Doc, isEmpty)

class Pair p where
  pfst :: p x y a -> x a
  psnd :: p x y b -> y b
  pair :: x a -> y a -> p x y a

pairList :: (Pair p) => [x a] -> [y a] -> [p x y a]
pairList [] _ = []
pairList _ [] = []
pairList (x:xs) (y:ys) = pair x y : pairList xs ys

data Boolean
-- Temporary Other type to keep some things from breaking
data Other
 
data Terminator = Semi | Empty

data ScopeTag = Pub | Priv deriving Eq

data FileType = Combined | Source | Header deriving Eq

data Binding = Static | Dynamic deriving Eq

data BindData = BD {bind :: Binding, bindDoc :: Doc}

bd :: Binding -> Doc -> BindData
bd = BD

---- Files ----

data FileData = FileD {fileType :: FileType, filePath :: FilePath,
  fileMod :: ModData}

instance Eq FileData where
  FileD _ p1 _ == FileD _ p2 _ = p1 == p2

fileD :: FileType -> String -> ModData -> FileData
fileD = FileD

file :: String -> ModData -> FileData
file = FileD Combined

srcFile :: String -> ModData -> FileData
srcFile = FileD Source

hdrFile :: String -> ModData -> FileData
hdrFile = FileD Header

isSource :: FileData -> Bool
isSource = liftA2 (||) (Source ==) (Combined ==) . fileType

isHeader :: FileData -> Bool
isHeader = liftA2 (||) (Header ==) (Combined ==) . fileType

updateFileMod :: ModData -> FileData -> FileData
updateFileMod m f = fileD (fileType f) (filePath f) m

---- Functions ----

data FuncData = FD {fnType :: TypeData, fnDoc :: Doc}

fd :: TypeData -> Doc -> FuncData
fd = FD

data TypedFunc a where
  BF :: FuncData -> TypedFunc Boolean
  OF :: FuncData -> TypedFunc Other
  LF :: TypedFunc a -> TypedFunc [a]

getFuncData :: TypedFunc a -> FuncData
getFuncData (BF f) = f
getFuncData (OF f) = f
getFuncData (LF f) = getFuncData f

funcType :: TypedFunc a -> TypeData
funcDoc :: TypedFunc a -> Doc
funcType = fnType . getFuncData
funcDoc = fnDoc . getFuncData

---- Modules ----

data ModData = MD {name :: String, isMainMod :: Bool, modDoc :: Doc}

md :: String -> Bool -> Doc -> ModData
md = MD

updateModDoc :: Doc -> ModData -> ModData
updateModDoc d m = md (name m) (isMainMod m) d

---- Methods ----

data MethodData = MthD {isMainMthd :: Bool, mthdParams :: [ParamData], 
  mthdDoc :: Doc}

mthd :: Bool -> [ParamData] -> Doc -> MethodData
mthd = MthD 

---- Operators ----

data OpData = OD {opPrec :: Int, opDoc :: Doc}

od :: Int -> Doc -> OpData
od = OD

---- Parameters ----

data ParamData = PD {paramVar :: VarData, paramDoc :: Doc}

instance Eq ParamData where
  PD v1 _ == PD v2 _ = v1 == v2

pd :: VarData -> Doc -> ParamData
pd = PD 

updateParamDoc :: (Doc -> Doc) -> ParamData -> ParamData
updateParamDoc f v = pd (paramVar v) ((f . paramDoc) v)

---- Programs ----

data ProgData = ProgD {progName :: String, progMods :: [FileData]}

progD :: String -> [FileData] -> ProgData
progD n fs = ProgD n (filter (not . isEmpty . modDoc . fileMod) fs)

emptyProg :: ProgData
emptyProg = progD "" []

---- State Variables ----

data StateVarData = SVD {getStVarScp :: ScopeTag, stVarDoc :: Doc, 
  destructSts :: (Doc, Terminator)}

svd :: ScopeTag -> Doc -> (Doc, Terminator) -> StateVarData
svd = SVD

---- Types ----

data TypeData = TD {cdType :: CodeType, tpString :: String, tpDoc :: Doc}

instance Eq TypeData where
  TD t1 _ _ == TD t2 _ _ = t1 == t2

td :: CodeType -> String -> Doc -> TypedType Other
td c s d = otherType (TD c s d)

btd :: CodeType -> String -> Doc -> TypedType Boolean
btd c s d = boolType (TD c s d)

ltd :: TypedType a -> (String -> String) -> (Doc -> Doc) -> TypedType [a]
ltd t sf df = listType $ updateTypedType t List sf df

data TypedType a where
  BT :: TypeData -> TypedType Boolean
  OT :: TypeData -> TypedType Other
  LT :: TypedType a -> TypedType [a]

boolType :: TypeData -> TypedType Boolean
boolType = BT

listType :: TypedType a -> TypedType [a]
listType = LT

otherType :: TypeData -> TypedType Other
otherType = OT

getTypeData :: TypedType a -> TypeData
getTypeData (BT t) = t
getTypeData (OT t) = t
getTypeData (LT t) = getTypeData t

cType :: TypedType a -> CodeType
typeString :: TypedType a -> String
typeDoc :: TypedType a -> Doc
cType = cdType . getTypeData
typeString = tpString . getTypeData
typeDoc = tpDoc . getTypeData

updateTypedType :: TypedType a -> (CodeType -> CodeType) -> (String -> String) 
  -> (Doc -> Doc) -> TypedType a
updateTypedType (BT t) cf sf df = BT (updateTypeData t cf sf df)
updateTypedType (OT t) cf sf df = OT (updateTypeData t cf sf df)
updateTypedType (LT t) cf sf df = LT (updateTypedType t cf sf df)

updateTypeData :: TypeData -> (CodeType -> CodeType) -> (String -> String) 
  -> (Doc -> Doc) -> TypeData
updateTypeData t cf sf df = TD (cf $ cdType t) (sf $ tpString t) (df $ tpDoc t)

---- Values ----

data ValData = VD {vlPrec :: Maybe Int, vlType :: TypeData, vlDoc :: Doc}

vd :: Maybe Int -> TypeData -> Doc -> ValData
vd = VD

updateValDoc :: (Doc -> Doc) -> TypedValue a -> TypedValue a
updateValDoc f (BV v) = BV $ vd (vlPrec v) (vlType v) ((f . vlDoc) v)
updateValDoc f (OV v) = OV $ vd (vlPrec v) (vlType v) ((f . vlDoc) v)
updateValDoc f (LV v) = LV $ updateValDoc f v

data TypedValue a where
  BV :: ValData -> TypedValue Boolean
  OV :: ValData -> TypedValue Other
  LV :: TypedValue a -> TypedValue [a]

boolVal :: ValData -> TypedValue Boolean
boolVal = BV

otherVal :: ValData -> TypedValue Other
otherVal = OV

getValData :: TypedValue a -> ValData
getValData (BV v) = v
getValData (OV v) = v
getValData (LV v) = getValData v

valPrec :: TypedValue a -> Maybe Int
valType :: TypedValue a -> TypeData
valDoc :: TypedValue a -> Doc
valPrec = vlPrec . getValData
valType = vlType . getValData
valDoc = vlDoc . getValData

toOtherVal :: TypedValue a -> TypedValue Other
toOtherVal (BV v) = OV v
toOtherVal (OV v) = OV v
toOtherVal (LV v) = toOtherVal v

---- Variables ----

data VarData = VarD {vrBind :: Binding, vrName :: String, 
  vrType :: TypeData, vrDoc :: Doc}

instance Eq VarData where
  VarD p1 n1 t1 _ == VarD p2 n2 t2 _ = p1 == p2 && n1 == n2 && t1 == t2

vard :: Binding -> String -> TypeData -> Doc -> VarData
vard = VarD

data TypedVar a where
  BVr :: VarData -> TypedVar Boolean
  OVr :: VarData -> TypedVar Other
  LVr :: TypedVar a -> TypedVar [a]

getVarData :: TypedVar a -> VarData
getVarData (BVr v) = v
getVarData (OVr v) = v
getVarData (LVr v) = getVarData v

otherVar :: VarData -> TypedVar Other
otherVar = OVr

varBind :: TypedVar a -> Binding
varName :: TypedVar a -> String
varType :: TypedVar a -> TypeData
varDoc :: TypedVar a -> Doc
varBind = vrBind . getVarData
varName = vrName . getVarData
varType = vrType . getVarData
varDoc = vrDoc . getVarData

toOtherVar :: TypedVar a -> TypedVar Other
toOtherVar (BVr v) = OVr v
toOtherVar (OVr v) = OVr v
toOtherVar (LVr v) = toOtherVar v

---- Transformations ----

typeToFunc :: TypedType a -> Doc -> TypedFunc a
typeToFunc (BT t) d = BF (fd t d)
typeToFunc (OT t) d = OF (fd t d)
typeToFunc (LT t) d = LF (typeToFunc t d)

typeToVal :: Maybe Int -> TypedType a -> Doc -> TypedValue a
typeToVal p (BT t) d = BV (vd p t d)
typeToVal p (OT t) d = OV (vd p t d)
typeToVal p (LT t) d = LV (typeToVal p t d)

typeToVar :: Binding -> String -> TypedType a -> Doc -> TypedVar a
typeToVar b n (BT t) d = BVr (vard b n t d)
typeToVar b n (OT t) d = OVr (vard b n t d)
typeToVar b n (LT t) d = LVr (typeToVar b n t d)

funcToType :: TypedFunc a -> TypedType a
funcToType (BF f) = BT (fnType f)
funcToType (OF f) = OT (fnType f)
funcToType (LF f) = LT (funcToType f)

valToType :: TypedValue a -> TypedType a
valToType (BV v) = BT (vlType v)
valToType (OV v) = OT (vlType v)
valToType (LV v) = LT (valToType v)

varToType :: TypedVar a -> TypedType a
varToType (BVr v) = BT (vrType v)
varToType (OVr v) = OT (vrType v)
varToType (LVr v) = LT (varToType v)

-- Reminder for later
-- varToVal :: TypedVar a -> TypedValue a
-- varToVal (BVar x) = BVal x