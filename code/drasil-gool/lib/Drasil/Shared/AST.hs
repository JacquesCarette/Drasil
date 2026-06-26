{-# LANGUAGE MultiParamTypeClasses #-}
module Drasil.Shared.AST (Terminator(..), VisibilityTag(..), ScopeTag(..),
  ScopeData(..), sd, QualifiedName, qualName, FileType(..), isSource,
  AttachmentTag(..), onAttachment, AttachmentData(attachment, attachmentDoc), ad, FileData(filePath,
  fileMod), fileD, updateFileMod, FuncData(fType, funcDoc), fd, ModData(name,
  modDoc), md, updateMod, MethodData(mthdDoc), mthd, updateMthd, OpData(opPrec,
  opDoc), od, ParamData(paramVar, paramDoc), pd, paramName, updateParam,
  ProgData(progName, progPurp, progMods), progD, emptyProg,
  StateVarData(getStVarScp, stVar, destructSts), svd, TypeData(cType,
  typeString, typeDoc), td, ValData(valPrec, valInt, valType, val), vd,
  updateValDoc, VarData(varBind, varName, varType, varDoc), vard,
  BinderD(bindName, bindType), bindFormD
  ) where

import Drasil.Shared.CodeType (CodeType)

import Prelude hiding ((<>))
import Text.PrettyPrint.HughesPJ (Doc, isEmpty)

-- For how statement endings are printed
data Terminator = Semi | Empty

-- Used for state variables and methods
-- Eq is needed for organizing methods and state variables into public and
-- private groups for C++ class rendering
data VisibilityTag = Pub | Priv deriving Eq

-- Used in method exception map and call map.
-- Qualification first, name second
-- Eq and Ord needed for map lookups
data QualifiedName = QN String String deriving (Eq, Ord)

qualName :: String -> String -> QualifiedName
qualName = QN

-- In C++ Source and Header files are separate, other languages have a single
-- (Combined) file
data FileType = Combined | Source | Header -- deriving Eq

isSource :: FileType -> Bool
isSource Header = False
isSource _ = True

-- | Denotes whether a member is bound to the class or its instances
data AttachmentTag = ClassLevel | InstanceLevel

onAttachment :: AttachmentTag -> a -> a -> a
onAttachment ClassLevel s _ = s
onAttachment InstanceLevel _ d = d

-- Used as the underlying data type for Attachment in the C++ renderer
data AttachmentData = BD {attachment :: AttachmentTag, attachmentDoc :: Doc}

ad :: AttachmentTag -> Doc -> AttachmentData
ad = BD

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

updateMod :: (Doc -> Doc) -> ModData -> ModData
updateMod f m = md (name m) (f $ modDoc m)

-- Used as the underlying data type for Methods in all renderers except C++
newtype MethodData = MthD {mthdDoc :: Doc}

mthd :: Doc -> MethodData
mthd = MthD

updateMthd :: MethodData -> (Doc -> Doc) -> MethodData
updateMthd m f = mthd ((f . mthdDoc) m)

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

updateParam :: (Doc -> Doc) -> ParamData -> ParamData
updateParam f v = pd (paramVar v) ((f . paramDoc) v)

-- Used as the underlying data type for Programs in all renderers
data ProgData = ProgD {progName :: String, progPurp :: String, progMods :: [FileData]}

progD :: String -> String -> [FileData] -> ProgData
progD n st fs = ProgD n st (filter (not . isEmpty . modDoc . fileMod) fs)

emptyProg :: ProgData
emptyProg = progD "" "" []

-- Used as the underlying data type for StateVars in the C++ renderer
data StateVarData = SVD {getStVarScp :: VisibilityTag, stVar :: Doc,
  destructSts :: (Doc, Terminator)}

svd :: VisibilityTag -> Doc -> (Doc, Terminator) -> StateVarData
svd = SVD

-- Used as the underlying data type for Scopes in all renderers
data ScopeTag = Local | Global deriving Eq

newtype ScopeData = SD {scopeTag :: ScopeTag}

sd :: ScopeTag -> ScopeData
sd = SD

-- Used as the underlying data type for Types in all renderers
data TypeData = TD {cType :: CodeType, typeString :: String, typeDoc :: Doc}

td :: CodeType -> String -> Doc -> TypeData
td = TD

-- Used as the underlying data type for Values in all renderers
-- valPrec is the precedence of the operator involved if the value is an expression,
-- valInt is the int the value is holding if the value is a litInt,
-- valType is the type of the value,
-- val is the printed representation of the value.
data ValData = VD {valPrec :: Maybe Int, valInt :: Maybe Integer, valType :: TypeData, val :: Doc}

vd :: Maybe Int -> Maybe Integer -> TypeData -> Doc -> ValData
vd = VD

updateValDoc :: (Doc -> Doc) -> ValData -> ValData
updateValDoc f v = vd (valPrec v) (valInt v) (valType v) ((f . val) v)

-- Used as the underlying data type for Variables in all renderers
data VarData = VarD {varBind :: AttachmentTag, varName :: String,
  varType :: TypeData, varDoc :: Doc}

vard :: AttachmentTag -> String -> TypeData -> Doc -> VarData
vard = VarD

-- Underlying type of Binder
data BinderD = BindFormD {bindName :: String, bindType :: TypeData}

bindFormD :: String -> TypeData -> BinderD
bindFormD = BindFormD

