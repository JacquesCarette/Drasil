module GOOL.Drasil.Data (Pair(..), Terminator(..), ScopeTag(..), FileType(..), 
  isSource, Exception(..), exception, stdExc, BindData(..), bd, FileData(..), 
  fileD, updateFileMod, FuncData(..), fd, ModData(..), md, updateModDoc, 
  MethodData(..), mthd, updateMthdDoc, OpData(..), od, ParamData(..), pd, 
  paramName, updateParamDoc, ProgData(..), progD, emptyProg, StateVarData(..), 
  svd, TypeData(..), td, ValData(..), vd, updateValDoc, Binding(..), 
  VarData(..), vard
) where

import GOOL.Drasil.CodeType (CodeType)

import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJ (Doc, isEmpty)

class Pair p where
  pfst :: p x y a -> x a
  psnd :: p x y b -> y b
  pair :: x a -> y a -> p x y a
 
data Terminator = Semi | Empty

data ScopeTag = Pub | Priv deriving Eq

data FileType = Combined | Source | Header deriving Eq

isSource :: FileType -> Bool
isSource Header = False
isSource _ = True

data Binding = Static | Dynamic deriving Eq

data Exception = Exc {
  loc :: String,
  exc :: String
}

instance Eq Exception where
  (Exc l1 e1) == (Exc l2 e2) = l1 == l2 && e1 == e2

instance Show Exception where
  show (Exc l e) = l ++ "." ++ e

exception :: String -> String -> Exception
exception = Exc

stdExc :: String -> Exception
stdExc = Exc ""

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

data FuncData = FD {fType :: TypeData, funcDoc :: Doc}

fd :: TypeData -> Doc -> FuncData
fd = FD

data ModData = MD {name :: String, modDoc :: Doc}

md :: String -> Doc -> ModData
md = MD

updateModDoc :: (Doc -> Doc) -> ModData -> ModData
updateModDoc f m = md (name m) (f $ modDoc m)

newtype MethodData = MthD {mthdDoc :: Doc}

mthd :: Doc -> MethodData
mthd = MthD 

updateMthdDoc :: MethodData -> (Doc -> Doc) -> MethodData
updateMthdDoc m f = mthd ((f . mthdDoc) m)

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