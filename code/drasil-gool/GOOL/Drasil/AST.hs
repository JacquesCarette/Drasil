module GOOL.Drasil.AST (Terminator(..), ScopeTag(..), FileType(..), isSource, 
  Binding(..), onBinding, BindData(bind, bindDoc), bd, 
  FileData(filePath, fileMod), fileD, updateFileMod, FuncData(fType, funcDoc), 
  fd, ModData(name, modDoc), md, updateModDoc, MethodData(mthdDoc), mthd, 
  updateMthdDoc, OpData(opPrec, opDoc), od, ParamData(paramVar, paramDoc), pd, 
  paramName, updateParamDoc, ProgData(progName, progMods), progD, emptyProg, 
  StateVarData(getStVarScp, stVarDoc, destructSts), svd, 
  TypeData(cType, typeString, typeDoc), td, ValData(valPrec, valType, valDoc), 
  vd, updateValDoc, VarData(varBind, varName, varType, varDoc), vard
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

-- Used as the underlying data type for Permanence in the C++ renderer
data BindData = BD {bind :: Binding, bindDoc :: Doc}

bd :: Binding -> Doc -> BindData
bd = BD

-- Used as the underlying data type for Files in all renderers
data FileData = FileD {filePath :: FilePath, fileMod :: ModData}

fileD :: FilePath -> ModData -> FileData
fileD = FileD

-- Replace a FileData's ModData with a new ModData
updateFileMod :: ModData -> FileData -> FileData
updateFileMod m f = fileD (filePath f) m

-- Used as the underlying data type for Functions in all renderers
data FuncData = FD {fType :: TypeData, funcDoc :: Doc}

fd :: TypeData -> Doc -> FuncData
fd = FD

-- Used as the underlying data type for Modules in all renderers
data ModData = MD {name :: String, modDoc :: Doc}

md :: String -> Doc -> ModData
md = MD

updateModDoc :: (Doc -> Doc) -> ModData -> ModData
updateModDoc f m = md (name m) (f $ modDoc m)

-- Used as the underlying data type for Methods in all renderers except C++
newtype MethodData = MthD {mthdDoc :: Doc}

mthd :: Doc -> MethodData
mthd = MthD 

updateMthdDoc :: MethodData -> (Doc -> Doc) -> MethodData
updateMthdDoc m f = mthd ((f . mthdDoc) m)

-- Used as the underlying data type for UnaryOp and BinaryOp in all renderers
data OpData = OD {opPrec :: Int, opDoc :: Doc}

od :: Int -> Doc -> OpData
od = OD

-- Used as the underlying data type for Parameters in all renderers
data ParamData = PD {paramVar :: VarData, paramDoc :: Doc}

pd :: VarData -> Doc -> ParamData
pd = PD 

paramName :: ParamData -> String
paramName = varName . paramVar

updateParamDoc :: (Doc -> Doc) -> ParamData -> ParamData
updateParamDoc f v = pd (paramVar v) ((f . paramDoc) v)

-- Used as the underlying data type for Programs in all renderers
data ProgData = ProgD {progName :: String, progMods :: [FileData]}

progD :: String -> [FileData] -> ProgData
progD n fs = ProgD n (filter (not . isEmpty . modDoc . fileMod) fs)

emptyProg :: ProgData
emptyProg = progD "" []

-- Used as the underlying data type for StateVars in the C++ renderer
data StateVarData = SVD {getStVarScp :: ScopeTag, stVarDoc :: Doc, 
  destructSts :: (Doc, Terminator)}

svd :: ScopeTag -> Doc -> (Doc, Terminator) -> StateVarData
svd = SVD

-- Used as the underlying data type for Types in all renderers
data TypeData = TD {cType :: CodeType, typeString :: String, typeDoc :: Doc}

td :: CodeType -> String -> Doc -> TypeData
td = TD

-- Used as the underlying data type for Values in all renderers
data ValData = VD {valPrec :: Maybe Int, valType :: TypeData, valDoc :: Doc}

vd :: Maybe Int -> TypeData -> Doc -> ValData
vd = VD

updateValDoc :: (Doc -> Doc) -> ValData -> ValData
updateValDoc f v = vd (valPrec v) (valType v) ((f . valDoc) v)

-- Used as the underlying data type for Variables in all renderers
data VarData = VarD {varBind :: Binding, varName :: String, 
  varType :: TypeData, varDoc :: Doc}

vard :: Binding -> String -> TypeData -> Doc -> VarData
vard = VarD