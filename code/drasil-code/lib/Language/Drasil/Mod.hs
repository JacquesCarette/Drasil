{-# LANGUAGE GADTs #-}
-- | Defines an AST for defining Modules.
module Language.Drasil.Mod (Class(..), StateVariable(..), Func(..), 
  FuncData(..), FuncDef(..), FuncStmt(..), Initializer, Mod(..), Name, 
  Version, Description, Import, ($:=), pubStateVar, privStateVar, classDef, 
  classImplements, ctorDef, ffor, fforRange, fDecDef, fname, fstdecl, funcData, funcDef, 
  funcDefParams, packmod, packmodRequires
) where

import Language.Drasil (Space, MayHaveUnit, Quantity, CodeExpr, LiteralC(..))
import Database.Drasil (ChunkDB)
import GOOL.Drasil (ScopeTag(..))

import Language.Drasil.Chunk.Code (CodeVarChunk, CodeFuncChunk, codevars, 
  codevars', quantvar)
import Language.Drasil.Chunk.Parameter (ParameterChunk, pcAuto)
import Language.Drasil.Code.DataDesc (DataDesc)

import Utils.Drasil (toPlainName)

import Data.List ((\\), nub)

-- | Type synonym for clarity.
type Name = String
-- | Type synonym for clarity.
type Description = String
-- | Type synonym for clarity.
type Import = String
-- | Type synonym for clarity.
type Version = String

-- | Holds module information.
data Mod = Mod Name Description [Import] [Class] [Func]

-- | Define a 'Mod' with the given 'Name', 'Description', 'Classes', and 'Functions'.
packmod :: Name -> Description -> [Class] -> [Func] -> Mod
packmod n d = packmodRequires n d []

-- | Define a 'Mod' that requires some library imports, with the given Name, 
-- Description, Classes, and Functions.
packmodRequires :: Name -> Description -> [Import] -> [Class] -> [Func] -> Mod
packmodRequires n = Mod (toPlainName n)

-- | Holds information needed to define a class.
data Class = ClassDef {
  className :: Name, 
  implements :: Maybe Name,
  classDesc :: Description,
  stateVars :: [StateVariable],
  methods :: [Func]}

-- | State variables hold attach a 'ScopeTag' to a 'CodeVarChunk'.
data StateVariable = SV {
  svScope :: ScopeTag,
  stVar :: CodeVarChunk}

-- | Define a public state variable based on the given 'CodeVarChunk'.
pubStateVar :: CodeVarChunk -> StateVariable
pubStateVar = SV Pub

-- | Define a private state variable based on the given 'CodeVarChunk'.
privStateVar :: CodeVarChunk -> StateVariable
privStateVar = SV Priv

-- | Define a class with the given 'Name', 'Description', state variables, and 
-- methods.
classDef :: Name -> Description -> [StateVariable] -> [Func] -> Class
classDef n = ClassDef n Nothing

-- | Define a class that implements an interface. 1st 'Name' is class name, 2nd is
-- interface name. 
classImplements :: Name -> Name -> Description -> [StateVariable] -> [Func] -> 
  Class
classImplements n i = ClassDef n (Just i)

-- | Holds a function definition or function data.
data Func = FDef FuncDef
          | FData FuncData

-- | Define a function that reads data from a file, according to the given
-- 'DataDesc'.
funcData :: Name -> Description -> DataDesc -> Func
funcData n desc d = FData $ FuncData (toPlainName n) desc d

-- | Define a function by providing the 'FuncStmt's for its body. Other 
-- parameters are function name, description, list of parameters, space of the 
-- returned value, and description of the returned value.
funcDef :: (Quantity c, MayHaveUnit c) => Name -> Description -> [c] -> 
  Space -> Maybe Description -> [FuncStmt] -> Func
funcDef s desc i t returnDesc fs = FDef $ FuncDef (toPlainName s) desc 
  (map (pcAuto . quantvar) i) t returnDesc fs 

-- | Like 'funcDef' but uses 'ParameterChunk's to represent the parameters.
funcDefParams :: Name -> Description -> [ParameterChunk] -> Space -> 
  Maybe Description -> [FuncStmt] -> Func
funcDefParams s desc ps t returnDesc fs = FDef $ FuncDef (toPlainName s) desc 
  ps t returnDesc fs

-- | Define a constructor, with the given name, description, parameters, 
-- initializers (variable-value pairs), and 'FuncStmt's for the body. 
ctorDef :: Name -> Description -> [ParameterChunk] -> [Initializer] -> 
  [FuncStmt] -> Func
ctorDef n desc ps is fs = FDef $ CtorDef n desc ps is fs

-- | Function data. Holds a name, description, and pieces of data with its own description.
data FuncData where
  FuncData :: Name -> Description -> DataDesc -> FuncData

-- | Defines a function.
data FuncDef where
  -- | Parameters are: Name, description, parameters, return type, return description, statements.
  FuncDef :: Name -> Description -> [ParameterChunk] -> Space -> 
    Maybe Description -> [FuncStmt] -> FuncDef
  CtorDef :: Name -> Description -> [ParameterChunk] -> [Initializer] -> 
    [FuncStmt] -> FuncDef

-- | Variable-value pair.
type Initializer = (CodeVarChunk, CodeExpr)
 
data FuncStmt where
  FAsg      :: CodeVarChunk -> CodeExpr -> FuncStmt
  FAsgIndex :: CodeVarChunk -> Integer -> CodeExpr -> FuncStmt
  -- | For-loop; Variable, Start, Stop, Step, Body.
  FFor      :: CodeVarChunk -> CodeExpr -> CodeExpr -> CodeExpr
                -> [FuncStmt] -> FuncStmt
  FForEach  :: CodeVarChunk -> CodeExpr -> [FuncStmt] -> FuncStmt
  FWhile    :: CodeExpr -> [FuncStmt] -> FuncStmt
  FCond     :: CodeExpr -> [FuncStmt] -> [FuncStmt] -> FuncStmt
  FRet      :: CodeExpr -> FuncStmt
  FThrow    :: String -> FuncStmt
  FTry      :: [FuncStmt] -> [FuncStmt] -> FuncStmt
  FContinue :: FuncStmt
  FDecDef   :: CodeVarChunk -> CodeExpr -> FuncStmt
  FFuncDef  :: CodeFuncChunk -> [ParameterChunk] -> [FuncStmt] -> FuncStmt
  FVal      :: CodeExpr -> FuncStmt
  FMulti    :: [FuncStmt] -> FuncStmt
  -- slight hack, for now
  FAppend   :: CodeExpr -> CodeExpr -> FuncStmt
  
-- | Define an assignment statement.
($:=) :: (Quantity c, MayHaveUnit c) => c -> CodeExpr -> FuncStmt
v $:= e = FAsg (quantvar v) e

-- | Define a for-loop. 'Quantity' is for the iteration variable, 'CodeExpr' is the 
-- upper bound at that variable (the variable will start with a value of 0).
-- ['FuncStmt'] is for the loop body.
ffor :: (Quantity c, MayHaveUnit c) => c -> CodeExpr -> [FuncStmt] -> FuncStmt
ffor v end = fforRange v (int 0) end (int 1)

-- | Define a for-loop. 'Quantity' is for the iteration variable, and 3 'CodeExpr's
-- for the start, stop, step numbers.
-- ['FuncStmt'] is for the loop body.
fforRange :: (Quantity c, MayHaveUnit c) => c -> CodeExpr -> CodeExpr 
  -> CodeExpr -> [FuncStmt] -> FuncStmt
fforRange v = FFor (quantvar v)

-- | Define a declare-define statement.
fDecDef :: (Quantity c, MayHaveUnit c) => c -> CodeExpr -> FuncStmt
fDecDef v  = FDecDef (quantvar v)

-- | Returns the list of 'CodeVarChunk's that are used in the list of 'FuncStmt's 
-- but are not declared in any of the 'FuncStmt's.
fstdecl :: ChunkDB -> [FuncStmt] -> [CodeVarChunk]
fstdecl ctx fsts = nub (concatMap (fstvars ctx) fsts) \\ nub (concatMap (declared ctx) fsts) 
  where
    fstvars :: ChunkDB -> FuncStmt -> [CodeVarChunk]
    fstvars sm (FDecDef cch e) = cch:codevars' e sm
    fstvars sm (FFuncDef cch ps sts) = quantvar cch : map quantvar ps 
      ++ concatMap (fstvars sm) sts
    fstvars sm (FAsg cch e) = cch:codevars' e sm
    fstvars sm (FAsgIndex cch _ e) = cch:codevars' e sm
    fstvars sm (FFor cch s e st fs) = nub $ cch : codevars' e sm ++ codevars' s sm
       ++ codevars' st sm ++ concatMap (fstvars sm) fs
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
    declared sm (FFuncDef cch ps sts) = quantvar cch : map quantvar ps 
      ++ concatMap (declared sm) sts
    declared _  (FAsg _ _) = []
    declared _  FAsgIndex {} = []
    declared sm (FFor cch _ _ _ fs) = cch : concatMap (declared sm) fs
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

-- | Gets the name of a function. 
fname :: Func -> Name
fname (FDef (FuncDef n _ _ _ _ _)) = n
fname (FDef (CtorDef n _ _ _ _)) = n
fname (FData (FuncData n _ _)) = n
