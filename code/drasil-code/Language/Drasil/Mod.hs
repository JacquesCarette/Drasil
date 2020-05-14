{-# LANGUAGE GADTs #-}
module Language.Drasil.Mod (Class(..), Func(..), FuncData(..), FuncDef(..), 
  FuncStmt(..), Initializer, Mod(..), Name, Description, Import, ($:=), 
  classDef, classImplements, ctorDef, ffor, fDecDef, fname, fstdecl, funcData, 
  funcDef, packmod, packmodRequires, prefixFunctions
) where

import Language.Drasil
import Database.Drasil (ChunkDB)

import Language.Drasil.Chunk.Code (CodeVarChunk, codevars, codevars', 
  funcPrefix, quantvar)
import Language.Drasil.Code.DataDesc (DataDesc)
import Language.Drasil.Printers (toPlainName)

import Data.List ((\\), nub)

type Name = String
type Description = String
type Import = String

data Mod = Mod Name Description [Import] [Class] [Func]

packmod :: Name -> Description -> [Class] -> [Func] -> Mod
packmod n d = packmodRequires n d []

packmodRequires :: Name -> Description -> [Import] -> [Class] -> [Func] -> Mod
packmodRequires n = Mod (toPlainName n)

data Class = ClassDef {
  className :: Name, 
  implements :: Maybe Name,
  classDesc :: Description,
  stateVars :: [CodeVarChunk],
  methods :: [Func]}

classDef :: Name -> Description -> [CodeVarChunk] -> [Func] -> Class
classDef n = ClassDef n Nothing

classImplements :: Name -> Name -> Description -> [CodeVarChunk] -> [Func] -> 
  Class
classImplements n i = ClassDef n (Just i)
     
data Func = FDef FuncDef
          | FData FuncData

funcData :: Name -> Description -> DataDesc -> Func
funcData n desc d = FData $ FuncData (toPlainName n) desc d

funcDef :: (Quantity c, MayHaveUnit c) => Name -> Description -> [c] -> 
  Space -> Maybe Description -> [FuncStmt] -> Func  
funcDef s desc i t returnDesc fs = FDef $ FuncDef (toPlainName s) desc 
  (map quantvar i) t returnDesc fs 

ctorDef :: Name -> Description -> [CodeVarChunk] -> [Initializer] -> 
  [FuncStmt] -> Func
ctorDef n desc ps is fs = FDef $ CtorDef n desc ps is fs

data FuncData where
  FuncData :: Name -> Description -> DataDesc -> FuncData
  
data FuncDef where
  -- Name, description, parameters, return type, return description, statements
  FuncDef :: Name -> Description -> [CodeVarChunk] -> Space -> 
    Maybe Description -> [FuncStmt] -> FuncDef
  CtorDef :: Name -> Description -> [CodeVarChunk] -> [Initializer] -> 
    [FuncStmt] -> FuncDef

type Initializer = (CodeVarChunk, Expr)
 
data FuncStmt where
  FAsg :: CodeVarChunk -> Expr -> FuncStmt
  FAsgIndex :: CodeVarChunk -> Integer -> Expr -> FuncStmt
  FAsgObjVar :: CodeVarChunk -> CodeVarChunk -> Expr -> FuncStmt -- Object, field, value
  FFor :: CodeVarChunk -> Expr -> [FuncStmt] -> FuncStmt
  FForEach :: CodeVarChunk -> Expr -> [FuncStmt] -> FuncStmt
  FWhile :: Expr -> [FuncStmt] -> FuncStmt
  FCond :: Expr -> [FuncStmt] -> [FuncStmt] -> FuncStmt
  FRet :: Expr -> FuncStmt
  FThrow :: String -> FuncStmt
  FTry :: [FuncStmt] -> [FuncStmt] -> FuncStmt
  FContinue :: FuncStmt
  FDecDef :: CodeVarChunk -> Expr -> FuncStmt
  FVal :: Expr -> FuncStmt
  FMulti :: [FuncStmt] -> FuncStmt
  -- slight hack, for now
  FAppend :: Expr -> Expr -> FuncStmt
  
($:=) :: (Quantity c, MayHaveUnit c) => c -> Expr -> FuncStmt
v $:= e = FAsg (quantvar v) e

ffor :: (Quantity c, MayHaveUnit c) => c -> Expr -> [FuncStmt] -> FuncStmt
ffor v = FFor (quantvar  v)

fDecDef :: (Quantity c, MayHaveUnit c) => c -> Expr -> FuncStmt
fDecDef v  = FDecDef (quantvar v)

fstdecl :: ChunkDB -> [FuncStmt] -> [CodeVarChunk]
fstdecl ctx fsts = nub (concatMap (fstvars ctx) fsts) \\ nub (concatMap (declared ctx) fsts) 
  where
    fstvars :: ChunkDB -> FuncStmt -> [CodeVarChunk]
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

    declared :: ChunkDB -> FuncStmt -> [CodeVarChunk]
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