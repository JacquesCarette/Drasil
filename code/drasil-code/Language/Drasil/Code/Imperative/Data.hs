module Language.Drasil.Code.Imperative.Data (Pair(..), pairList,
  Terminator (..), ScopeTag(..), AuxData(..), ad, FileData(..), fileD, 
  updateFileMod, FuncData(..), fd, ModData(..), md, updateModDoc, 
  MethodData(..), mthd, PackData(..), packD, ParamData(..), pd, updateParamDoc, 
  StateVarData(..), svd, TypeData(..), td, ValData(..), vd, updateValDoc, 
  VarData(..), vard
) where

import Language.Drasil.Code.Code (CodeType)

import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJ (Doc)

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

data AuxData = AD {auxFilePath :: FilePath, auxDoc :: Doc}

ad :: String -> Doc -> AuxData
ad = AD

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

updateModDoc :: Doc -> ModData -> ModData
updateModDoc d m = md (name m) (isMainMod m) d

data MethodData = MthD {isMainMthd :: Bool, mthdParams :: [ParamData], 
  mthdDoc :: Doc}

mthd :: Bool -> [ParamData] -> Doc -> MethodData
mthd = MthD 

data PackData = PackD {packName :: String, packMods :: [FileData], 
  packAux :: [AuxData]}

packD :: String -> [FileData] -> [AuxData] -> PackData
packD = PackD

data ParamData = PD {paramName :: String, paramType :: TypeData, 
  paramDoc :: Doc}

instance Eq ParamData where
  PD n1 _ _ == PD n2 _ _ = n1 == n2

pd :: String -> TypeData -> Doc -> ParamData
pd = PD 

updateParamDoc :: (Doc -> Doc) -> ParamData -> ParamData
updateParamDoc f v = pd (paramName v) (paramType v) ((f . paramDoc) v)

data StateVarData = SVD {getStVarScp :: ScopeTag, stVarDoc :: Doc, 
  destructSts :: (Doc, Terminator)}

svd :: ScopeTag -> Doc -> (Doc, Terminator) -> StateVarData
svd = SVD

data TypeData = TD {cType :: CodeType, typeDoc :: Doc}

instance Eq TypeData where
  TD t1 _ == TD t2 _ = t1 == t2

td :: CodeType -> Doc -> TypeData
td = TD

data ValData = VD {valType :: TypeData, valDoc :: Doc}

vd :: TypeData -> Doc -> ValData
vd = VD

updateValDoc :: (Doc -> Doc) -> ValData -> ValData
updateValDoc f v = vd (valType v) ((f . valDoc) v)

data VarData = VarD {varName :: String, varType :: TypeData, varDoc :: Doc}

instance Eq VarData where
  VarD n1 t1 _ == VarD n2 t2 _ = n1 == n2 && t1 == t2 

vard :: String -> TypeData -> Doc -> VarData
vard = VarD