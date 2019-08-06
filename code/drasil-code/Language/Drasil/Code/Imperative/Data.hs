module Language.Drasil.Code.Imperative.Data (Pair(..), pairList,
  Terminator(..), ScopeTag(..), FileType(..), AuxData(..), ad, emptyAux, 
  FileData(..), fileD, file, srcFile, hdrFile, isSource, isHeader, 
  updateFileMod, FuncData(..), fd, ModData(..), md, updateModDoc, 
  MethodData(..), mthd, OpData(..), od, PackData(..), packD, emptyPack, 
  ParamData(..), pd, updateParamDoc, ProgData(..), progD, emptyProg, 
  StateVarData(..), svd, TypeData(..), td, ValData(..), vd, updateValDoc, 
  VarData(..), vard
) where

import Language.Drasil.Code.Code (CodeType)

import Control.Applicative (liftA2)
import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJ (Doc, empty, isEmpty)

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

data AuxData = AD {auxFilePath :: FilePath, auxDoc :: Doc}

ad :: String -> Doc -> AuxData
ad = AD

emptyAux :: AuxData
emptyAux = ad "" empty

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

data OpData = OD {opPrec :: Int, opDoc :: Doc}

od :: Int -> Doc -> OpData
od = OD

data PackData = PackD {packProg :: ProgData, packAux :: [AuxData]}

packD :: ProgData -> [AuxData] -> PackData
packD = PackD

emptyPack :: PackData
emptyPack = packD emptyProg []

data ParamData = PD {paramName :: String, paramType :: TypeData, 
  paramDoc :: Doc}

instance Eq ParamData where
  PD n1 _ _ == PD n2 _ _ = n1 == n2

pd :: String -> TypeData -> Doc -> ParamData
pd = PD 

updateParamDoc :: (Doc -> Doc) -> ParamData -> ParamData
updateParamDoc f v = pd (paramName v) (paramType v) ((f . paramDoc) v)

data ProgData = ProgD {progName :: String, progMods :: [FileData]}

progD :: String -> [FileData] -> ProgData
progD n fs = ProgD n (filter (not . isEmpty . modDoc . fileMod) fs)

emptyProg :: ProgData
emptyProg = progD "" []

data StateVarData = SVD {getStVarScp :: ScopeTag, stVarDoc :: Doc, 
  destructSts :: (Doc, Terminator)}

svd :: ScopeTag -> Doc -> (Doc, Terminator) -> StateVarData
svd = SVD

data TypeData = TD {cType :: CodeType, typeDoc :: Doc}

instance Eq TypeData where
  TD t1 _ == TD t2 _ = t1 == t2

td :: CodeType -> Doc -> TypeData
td = TD

data ValData = VD {valPrec :: Maybe Int, valType :: TypeData, valDoc :: Doc}

vd :: Maybe Int -> TypeData -> Doc -> ValData
vd = VD

updateValDoc :: (Doc -> Doc) -> ValData -> ValData
updateValDoc f v = vd (valPrec v) (valType v) ((f . valDoc) v)

data VarData = VarD {varName :: String, varType :: TypeData, varDoc :: Doc}

instance Eq VarData where
  VarD n1 t1 _ == VarD n2 t2 _ = n1 == n2 && t1 == t2 

vard :: String -> TypeData -> Doc -> VarData
vard = VarD