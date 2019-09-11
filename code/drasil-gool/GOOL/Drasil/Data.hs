{-# LANGUAGE GADTs #-}

module GOOL.Drasil.Data (Boolean, Val, Pair(..), pairList, Terminator(..), 
  ScopeTag(..), FileType(..), BindData(..), bd, FileData(..), fileD, file, 
  srcFile, hdrFile, isSource, isHeader, updateFileMod, FuncData(..), fd, 
  ModData(..), md, updateModDoc, MethodData(..), mthd, OpData(..), od, 
  ParamData(..), pd, updateParamDoc, ProgData(..), progD, emptyProg, 
  StateVarData(..), svd, TypeData(..), td, ValData(..), vd, updateValDoc, bvd, 
  TypedValue(..), otherVal, boolVal, valPrec, valType, valDoc, Binding(..), 
  VarData(..), vard
) where

import GOOL.Drasil.CodeType (CodeType)

import Control.Applicative (liftA2)
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

data Boolean
-- Temporary Val type to keep some things from breaking
data Val
 
data Terminator = Semi | Empty

data ScopeTag = Pub | Priv deriving Eq

data FileType = Combined | Source | Header deriving Eq

data Binding = Static | Dynamic deriving Eq

data BindData = BD {bind :: Binding, bindDoc :: Doc}

bd :: Binding -> Doc -> BindData
bd = BD

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

data ParamData = PD {paramVar :: VarData, paramDoc :: Doc}

instance Eq ParamData where
  PD v1 _ == PD v2 _ = v1 == v2

pd :: VarData -> Doc -> ParamData
pd = PD 

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

data ValData = VD {vlPrec :: Maybe Int, vlType :: TypeData, vlDoc :: Doc}

vd :: Maybe Int -> TypeData -> Doc -> TypedValue Val
vd = VD

updateValDoc :: (Doc -> Doc) -> TypedValue Val -> TypedValue Val
updateValDoc f v = vd (valPrec v) (valType v) ((f . valDoc) v)

bvd :: Maybe Int -> TypeData -> Doc -> TypedValue Boolean
bvd p t d = boolVal (VD p t d)

data TypedValue a where
  BV :: ValData -> TypedValue Boolean
  OV :: ValData -> TypedValue Val

boolVal :: ValData -> TypedValue Boolean
boolVal = BV

otherVal :: ValData -> TypedValue Val
otherVal = OV

getValData :: TypedValue a -> ValData
getValData (BV v) = v
getValData (OV v) = v

valPrec :: TypedValue a -> Maybe Int
valType :: TypedValue a -> TypeData
valDoc :: TypedValue a -> Doc
valPrec = vlPrec . getValData 
valType = vlType . getValData
valDoc = vlDoc . getValData

data VarData = VarD {varBind :: Binding, varName :: String, 
  varType :: TypeData, varDoc :: Doc}

instance Eq VarData where
  VarD p1 n1 t1 _ == VarD p2 n2 t2 _ = p1 == p2 && n1 == n2 && t1 == t2

vard :: Binding -> String -> TypeData -> Doc -> VarData
vard = VarD

-- Reminder for later
-- varToVal :: TypedVar a -> TypedValue a
-- varToVal (BVar x) = BVal x