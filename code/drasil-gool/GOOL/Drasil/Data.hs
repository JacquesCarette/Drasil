module GOOL.Drasil.Data (Pair(..), pairList, Terminator(..), ScopeTag(..), 
  FileType(..), BindData(..), bd, FileData(..), fileD, updateFileMod, 
  FuncData(..), fd, ModData(..), md, updateModDoc, MethodData(..), mthd, 
  updateMthdDoc, OpData(..), od, ParamData(..), pd, paramName, updateParamDoc, 
  ProgData(..), progD, emptyProg, StateVarData(..), svd, TypeData(..), td, 
  ValData(..), vd, updateValDoc, Binding(..), VarData(..), vard
) where

import GOOL.Drasil.CodeType (CodeType)

import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJ (Doc, isEmpty)

class Pair p where
  pfst :: p x y a -> x a
  psnd :: p x y b -> y b
  pair :: x a -> y a -> p x y a

pairList :: (Pair p) => [x a] -> [y a] -> [p x y a]
pairList [] _ = []
pairList _ [] = []
pairList (x:xs) (y:ys) = pair x y : pairList xs ys
 
data Terminator = Semi | Empty

data ScopeTag = Pub | Priv deriving Eq

data FileType = Combined | Source | Header deriving Eq

data Binding = Static | Dynamic deriving Eq

data BindData = BD {bind :: Binding, bindDoc :: Doc}

bd :: Binding -> Doc -> BindData
bd = BD

data FileData = FileD {filePath :: FilePath, fileMod :: ModData}

instance Eq FileData where
  FileD p1 _ == FileD p2 _ = p1 == p2

fileD :: String -> ModData -> FileData
fileD = FileD

updateFileMod :: ModData -> FileData -> FileData
updateFileMod m f = fileD (filePath f) m

data FuncData = FD {funcType :: TypeData, funcDoc :: Doc}

fd :: TypeData -> Doc -> FuncData
fd = FD

data ModData = MD {name :: String, isMainMod :: Bool, modDoc :: Doc}

md :: String -> Bool -> Doc -> ModData
md = MD

updateModDoc :: (Doc -> Doc) -> ModData -> ModData
updateModDoc f m = md (name m) (isMainMod m) (f $ modDoc m)

newtype MethodData = MthD {mthdDoc :: Doc}

mthd :: Doc -> MethodData
mthd = MthD 

updateMthdDoc :: (Doc -> Doc) -> MethodData -> MethodData
updateMthdDoc f m = mthd ((f . mthdDoc) m)

data OpData = OD {opPrec :: Int, opDoc :: Doc}

od :: Int -> Doc -> OpData
od = OD

data ParamData = PD {paramVar :: VarData, paramDoc :: Doc}

instance Eq ParamData where
  PD v1 _ == PD v2 _ = v1 == v2

pd :: VarData -> Doc -> ParamData
pd = PD 

paramName :: ParamData -> String
paramName = varName . paramVar

updateParamDoc :: (Doc -> Doc) -> ParamData -> ParamData
updateParamDoc f v = pd (paramVar v) ((f . paramDoc) v)

data ProgData = ProgD {progName :: String, progMods :: [FileData]}

progD :: String -> [FileData] -> ProgData
progD n fs = ProgD n (filter (not . isEmpty . modDoc . fileMod) fs)

emptyProg :: ProgData
emptyProg = progD "" []

data StateVarData = SVD {getStVarScp :: ScopeTag, stVarDoc :: Doc, 
  destructSts :: (Doc, Terminator)}

svd :: ScopeTag -> Doc -> (Doc, Terminator) -> StateVarData
svd = SVD

data TypeData = TD {cType :: CodeType, typeString :: String, typeDoc :: Doc}

instance Eq TypeData where
  TD t1 _ _ == TD t2 _ _ = t1 == t2

td :: CodeType -> String -> Doc -> TypeData
td = TD

data ValData = VD {valPrec :: Maybe Int, valType :: TypeData, valDoc :: Doc}

vd :: Maybe Int -> TypeData -> Doc -> ValData
vd = VD

updateValDoc :: (Doc -> Doc) -> ValData -> ValData
updateValDoc f v = vd (valPrec v) (valType v) ((f . valDoc) v)

data VarData = VarD {varBind :: Binding, varName :: String, 
  varType :: TypeData, varDoc :: Doc}

instance Eq VarData where
  VarD p1 n1 t1 _ == VarD p2 n2 t2 _ = p1 == p2 && n1 == n2 && t1 == t2

vard :: Binding -> String -> TypeData -> Doc -> VarData
vard = VarD