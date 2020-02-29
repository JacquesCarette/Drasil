{-# LANGUAGE GADTs #-}
module Language.Drasil.Mod (Class(..), Func(..), FuncData(..), FuncDef(..), 
  FuncStmt(..), Initializer, Mod(..), Name, ($:=), classDef, classImplements, 
  ctorDef, ffor, fdec, fname, fstdecl, funcData, funcDef, funcQD, 
  getFuncParams, packmod, packmodRequires, prefixFunctions
) where

import Language.Drasil
import Database.Drasil (ChunkDB)

import Language.Drasil.Chunk.Code (CodeChunk, codeName, codevars, codevars', 
  funcPrefix, quantvar)
import Language.Drasil.Chunk.CodeDefinition (CodeDefinition, qtoc)
import Language.Drasil.Code.Code (spaceToCodeType)
import Language.Drasil.Code.DataDesc (DataDesc, getInputs)
import Language.Drasil.Printers (toPlainName)

import GOOL.Drasil (CodeType)

import Data.List ((\\), nub)

type Name = String

-- Name, description, imports, classes, functions
data Mod = Mod Name String [String] [Class] [Func]

packmod :: Name -> String -> [Class] -> [Func] -> Mod
packmod n d = packmodRequires n d []

packmodRequires :: Name -> String -> [String] -> [Class] -> [Func] -> Mod
packmodRequires n = Mod (toPlainName n)

data Class = ClassDef {
  className :: Name, 
  implements :: Maybe Name,
  classDesc :: String,
  stateVars :: [CodeChunk],
  methods :: [Func]}

classDef :: Name -> String -> [CodeChunk] -> [Func] -> Class
classDef n = ClassDef n Nothing

classImplements :: Name -> Name -> String -> [CodeChunk] -> [Func] -> Class
classImplements n i = ClassDef n (Just i)
     
data Func = FCD CodeDefinition
          | FDef FuncDef
          | FData FuncData

funcQD :: QDefinition -> Func
funcQD qd = FCD $ qtoc qd 

funcData :: Name -> String -> DataDesc -> Func
funcData n desc d = FData $ FuncData (toPlainName n) desc d

funcDef :: (Quantity c, MayHaveUnit c) => Name -> String -> [c] -> Space -> 
  Maybe String -> [FuncStmt] -> Func  
funcDef s desc i t returnDesc fs = FDef $ FuncDef (toPlainName s) desc 
  (map quantvar i) (spaceToCodeType t) returnDesc fs 

ctorDef :: Name -> String -> [CodeChunk] -> [Initializer] -> [FuncStmt] -> 
  FuncDef
ctorDef = CtorDef

data FuncData where
  FuncData :: Name -> String -> DataDesc -> FuncData
  
data FuncDef where
  -- Name, description, parameters, return type, return description, statements
  FuncDef :: Name -> String -> [CodeChunk] -> CodeType -> Maybe String -> 
    [FuncStmt] -> FuncDef
  CtorDef :: Name -> String -> [CodeChunk] -> [Initializer] -> [FuncStmt] -> 
    FuncDef

type Initializer = (CodeChunk, Expr)
 
data FuncStmt where
  FAsg :: CodeChunk -> Expr -> FuncStmt
  FAsgIndex :: CodeChunk -> Integer -> Expr -> FuncStmt
  FAsgObjVar :: CodeChunk -> CodeChunk -> Expr -> FuncStmt -- Object, field, value
  FFor :: CodeChunk -> Expr -> [FuncStmt] -> FuncStmt
  FForEach :: CodeChunk -> Expr -> [FuncStmt] -> FuncStmt
  FWhile :: Expr -> [FuncStmt] -> FuncStmt
  FCond :: Expr -> [FuncStmt] -> [FuncStmt] -> FuncStmt
  FRet :: Expr -> FuncStmt
  FThrow :: String -> FuncStmt
  FTry :: [FuncStmt] -> [FuncStmt] -> FuncStmt
  FContinue :: FuncStmt
  FDec :: CodeChunk -> FuncStmt
  FDecDef :: CodeChunk -> Expr -> FuncStmt
  FVal :: Expr -> FuncStmt
  FMulti :: [FuncStmt] -> FuncStmt
  -- slight hack, for now
  FAppend :: Expr -> Expr -> FuncStmt
  
($:=) :: (Quantity c, MayHaveUnit c) => c -> Expr -> FuncStmt
v $:= e = FAsg (quantvar v) e

ffor :: (Quantity c, MayHaveUnit c) => c -> Expr -> [FuncStmt] -> FuncStmt
ffor v = FFor (quantvar  v)

fdec :: (Quantity c, MayHaveUnit c) => c -> FuncStmt
fdec v  = FDec (quantvar v)

getFuncParams :: Func -> [CodeChunk]
getFuncParams (FDef (FuncDef _ _ ps _ _ _)) = ps
getFuncParams (FDef (CtorDef _ _ ps _ _)) = ps
getFuncParams (FData (FuncData _ _ d)) = getInputs d
getFuncParams (FCD _) = []

fstdecl :: ChunkDB -> [FuncStmt] -> [CodeChunk]
fstdecl ctx fsts = nub (concatMap (fstvars ctx) fsts) \\ nub (concatMap (declared ctx) fsts) 
  where
    fstvars :: ChunkDB -> FuncStmt -> [CodeChunk]
    fstvars _  (FDec cch) = [cch]
    fstvars sm (FDecDef cch e) = cch:codevars' e sm
    fstvars sm (FAsg cch e) = cch:codevars' e sm
    fstvars sm (FAsgIndex cch _ e) = cch:codevars' e sm
    fstvars sm (FAsgObjVar cch _ e) = cch:codevars' e sm
    fstvars sm (FFor cch e fs) = nub (cch : codevars' e sm ++ concatMap (fstvars sm) fs)
    fstvars sm (FForEach cch e fs) = nub (cch : codevars' e sm ++ concatMap (fstvars sm) fs)
    fstvars sm (FWhile e fs) = codevars' e sm ++ concatMap (fstvars sm) fs
    fstvars sm (FCond e tfs efs) = codevars' e sm ++ concatMap (fstvars sm) tfs ++ concatMap (fstvars sm) efs
    fstvars sm (FRet e) = codevars' e sm
    fstvars sm (FTry tfs cfs) = concatMap (fstvars sm) tfs ++ concatMap (fstvars sm ) cfs
    fstvars _  (FThrow _) = [] -- is this right?
    fstvars _  FContinue = []
    fstvars sm (FVal v) = codevars' v sm
    fstvars sm (FMulti ss) = concatMap (fstvars sm) ss
    fstvars sm (FAppend a b) = nub (codevars a sm ++ codevars b sm)

    declared :: ChunkDB -> FuncStmt -> [CodeChunk]
    declared _  (FDec cch) = [cch]
    declared _  (FDecDef cch _) = [cch]
    declared _  (FAsg _ _) = []
    declared _  FAsgIndex {} = []
    declared _  FAsgObjVar {} = []
    declared sm (FFor cch _ fs) = cch : concatMap (declared sm) fs
    declared sm (FForEach cch _ fs) = cch : concatMap (declared sm) fs
    declared sm (FWhile _ fs) = concatMap (declared sm) fs
    declared sm (FCond _ tfs efs) = concatMap (declared sm) tfs ++ concatMap (declared sm) efs
    declared _  (FRet _) = []
    declared sm (FTry tfs cfs) = concatMap (declared sm) tfs ++ concatMap (declared sm) cfs
    declared _  (FThrow _) = [] -- is this right?
    declared _  FContinue = []
    declared _  (FVal _) = []
    declared sm (FMulti ss) = concatMap (declared sm) ss
    declared _  (FAppend _ _) = []
       
fname :: Func -> Name       
fname (FCD cd) = codeName cd
fname (FDef (FuncDef n _ _ _ _ _)) = n
fname (FDef (CtorDef n _ _ _ _)) = n
fname (FData (FuncData n _ _)) = n 

prefixFunctions :: [Mod] -> [Mod]
prefixFunctions = map (\(Mod nm desc rs cs fs) -> Mod nm desc rs cs $ map pfunc fs)
  where pfunc (FData (FuncData n desc d)) = FData (FuncData (funcPrefix ++ n) 
          desc d)
        pfunc (FDef (FuncDef n desc a t rd f)) = FDef (FuncDef (funcPrefix ++ n)
          desc a t rd f)
        pfunc f = f