module GOOL.Drasil.AST (Terminator(..), ScopeTag(..), FileType(..), 
  isSource, BindData(..), bd, FileData(..), fileD, updateFileMod, FuncData(..), 
  fd, ModData(..), md, updateModDoc, MethodData(..), mthd, updateMthdDoc, 
  OpData(..), od, ParamData(..), pd, paramName, updateParamDoc, ProgData(..), 
  progD, emptyProg, StateVarData(..), svd, TypeData(..), td, ValData(..), vd, 
  updateValDoc, Binding(..), onBinding, VarData(..), vard
) where

import GOOL.Drasil.CodeType (CodeType)

import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJ (Doc, isEmpty)

-- For how statement endings are printed
data Terminator = Semi | Empty

-- Used for state variables and methods
-- Eq is needed for organizing methods and state variables into public and 
-- private groups for C++ class rendering
data ScopeTag = Pub | Priv deriving Eq

-- In C++ Source and Header files are separate, other languages have a single 
-- (Combined) file
data FileType = Combined | Source | Header -- deriving Eq

isSource :: FileType -> Bool
isSource Header = False
isSource _ = True

-- Static means bound at compile-time, Dynamic at run-time, used in BindData 
-- and VarData
data Binding = Static | Dynamic

onBinding :: Binding -> a -> a -> a
onBinding Static s _ = s
onBinding Dynamic _ d = d

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

vard :: Binding -> String -> TypeData -> Doc -> VarData
vard = VarD