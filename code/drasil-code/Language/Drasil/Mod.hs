{-# LANGUAGE GADTs #-}
module Language.Drasil.Mod (Func(..), FuncData(..), FuncDef(..), FuncStmt(..), 
  Mod(..), Name, ($:=), ffor, fdec, fname, fstdecl, funcData, funcDef, funcQD, 
  getFuncParams, packmod, prefixFunctions
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

import Data.List ((\\), delete, nub)

type Name = String

data Mod = Mod Name String [Func]

packmod :: Name -> String -> [Func] -> Mod
packmod n = Mod (toPlainName n)
     
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

data FuncData where
  FuncData :: Name -> String -> DataDesc -> FuncData
  
data FuncDef where
  FuncDef :: Name -> String -> [CodeChunk] -> CodeType -> Maybe String -> 
    [FuncStmt] -> FuncDef
 
data FuncStmt where
  FAsg :: CodeChunk -> Expr -> FuncStmt
  FAsgIndex :: CodeChunk -> Integer -> Expr -> FuncStmt
  FFor :: CodeChunk -> Expr -> [FuncStmt] -> FuncStmt
  FWhile :: Expr -> [FuncStmt] -> FuncStmt
  FCond :: Expr -> [FuncStmt] -> [FuncStmt] -> FuncStmt
  FRet :: Expr -> FuncStmt
  FThrow :: String -> FuncStmt
  FTry :: [FuncStmt] -> [FuncStmt] -> FuncStmt
  FContinue :: FuncStmt
  FDec :: CodeChunk -> FuncStmt
  FProcCall :: Func -> [Expr] -> FuncStmt
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
getFuncParams (FData (FuncData _ _ d)) = getInputs d
getFuncParams (FCD _) = []

fstdecl :: ChunkDB -> [FuncStmt] -> [CodeChunk]
fstdecl ctx fsts = nub (concatMap (fstvars ctx) fsts) \\ nub (concatMap (declared ctx) fsts) 
  where
    fstvars :: ChunkDB -> FuncStmt -> [CodeChunk]
    fstvars _  (FDec cch) = [cch]
    fstvars sm (FAsg cch e) = cch:codevars' e sm
    fstvars sm (FAsgIndex cch _ e) = cch:codevars' e sm
    fstvars sm (FFor cch e fs) = delete cch $ nub (codevars' e sm ++ concatMap (fstvars sm) fs)
    fstvars sm (FWhile e fs) = codevars' e sm ++ concatMap (fstvars sm) fs
    fstvars sm (FCond e tfs efs) = codevars' e sm ++ concatMap (fstvars sm) tfs ++ concatMap (fstvars sm) efs
    fstvars sm (FRet e) = codevars' e sm
    fstvars sm (FTry tfs cfs) = concatMap (fstvars sm) tfs ++ concatMap (fstvars sm ) cfs
    fstvars _  (FThrow _) = [] -- is this right?
    fstvars _  FContinue = []
    fstvars sm (FProcCall _ l) = concatMap (`codevars` sm) l
    fstvars sm (FAppend a b) = nub (codevars a sm ++ codevars b sm)

    declared :: ChunkDB -> FuncStmt -> [CodeChunk]
    declared _  (FDec cch) = [cch]
    declared _  (FAsg _ _) = []
    declared _  FAsgIndex {} = []
    declared sm (FFor _ _ fs) = concatMap (declared sm) fs
    declared sm (FWhile _ fs) = concatMap (declared sm) fs
    declared sm (FCond _ tfs efs) = concatMap (declared sm) tfs ++ concatMap (declared sm) efs
    declared _  (FRet _) = []
    declared sm (FTry tfs cfs) = concatMap (declared sm) tfs ++ concatMap (declared sm) cfs
    declared _  (FThrow _) = [] -- is this right?
    declared _  FContinue = []
    declared _  (FProcCall _ _) = []
    declared _  (FAppend _ _) = []
       
fname :: Func -> Name       
fname (FCD cd) = codeName cd
fname (FDef (FuncDef n _ _ _ _ _)) = n
fname (FData (FuncData n _ _)) = n 

prefixFunctions :: [Mod] -> [Mod]
prefixFunctions = map (\(Mod nm desc fs) -> Mod nm desc $ map pfunc fs)
  where pfunc f@(FCD _) = f
        pfunc (FData (FuncData n desc d)) = FData (FuncData (funcPrefix ++ n) 
          desc d)
        pfunc (FDef (FuncDef n desc a t rd f)) = FDef (FuncDef (funcPrefix ++ n)
          desc a t rd f)