{-# LANGUAGE GADTs #-}

module Language.Drasil.CodeSpec where

import Language.Drasil.Chunk.Code
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.Quantity -- for hack
import Language.Drasil.Chunk.SymbolForm -- for hack
import Language.Drasil.NounPhrase
import Language.Drasil.Spec
import Language.Drasil.SystemInformation
import Language.Drasil.Code -- for hack
import Language.Drasil.Defs -- for hack
import Language.Drasil.Expr -- for hack
import Language.Drasil.Space -- for hack

import qualified Data.Map as Map
import Control.Lens ((^.))

import Prelude hiding (const)

data CodeSpec = CodeSpec {
  program :: CodeName,
  inputs :: [CodeChunk],
  outputs :: [CodeChunk],
  relations :: [CodeDefinition],
  cMap :: ConstraintMap,
  fMap :: FunctionMap,
  vMap :: VarMap,
  constMap :: FunctionMap,
  fMods :: [FuncMod],
  const :: [CodeDefinition],
  choices :: Choices,
  modDefs :: [ModDef],  -- medium hack
  mods :: [(String, [FunctionDecl])] -- big hack
}

type FunctionMap = Map.Map String CodeDefinition
type VarMap      = Map.Map String CodeChunk

functionMap :: [CodeDefinition] -> FunctionMap
functionMap cs = Map.fromList (map (\x -> (codeName x, x)) cs)

funcTerm :: String -> FunctionMap -> String
funcTerm cname m = lookF (Map.lookup cname m)
  where lookF :: (Maybe CodeDefinition) -> String
        lookF Nothing = ""
        lookF (Just cd) = getStr (phrase $ cd ^. term)
       

varMap :: [CodeChunk] -> VarMap
varMap cs = Map.fromList (map (\x -> (codeName x, x)) cs)

varTerm :: String -> VarMap -> String
varTerm cname m = lookV (Map.lookup cname m)
  where lookV :: (Maybe CodeChunk) -> String
        lookV Nothing = ""
        lookV (Just cc) = getStr (phrase $ cc ^. term)  
        
varType :: String -> VarMap -> CodeType
varType cname m = lookV (Map.lookup cname m)
  where lookV :: (Maybe CodeChunk) -> CodeType
        lookV Nothing = error "Variable not found"
        lookV (Just cc) = codeType cc
        
getStr :: Sentence -> String
getStr (S s) = s
getStr (P s) = symbToCodeName s
getStr ((:+:) s1 s2) = getStr s1 ++ getStr s2
getStr _ = error "Term is not a string" 

codeSpec :: SystemInformation -> CodeSpec
codeSpec si = codeSpec' si defaultChoices

codeSpec' :: SystemInformation -> Choices -> CodeSpec
codeSpec' (SI {_sys = sys, _quants = q, _definitions = defs, _inputs = ins, _outputs = outs, _constraints = cs, _constants = constants}) ch = CodeSpec {
  program = NICN sys,
  inputs = map codevar ins,
  outputs = map codevar outs,
  relations = map qtoc defs,
  cMap = constraintMap cs,
  fMap = functionMap $ map qtoc defs,
  vMap = varMap (map codevar q),
  constMap = functionMap $ map qtoc constants,
  fMods = [funcMod "Calculations" defs],
  const = map qtoc constants,
  choices = ch,
  modDefs = [],
  mods = modHack
}

codeSpec'' :: SystemInformation -> [FuncMod] -> Choices -> CodeSpec
codeSpec'' si fm ch = 
  let sp = codeSpec' si ch 
  in  sp { fMods = fm }

data FuncMod = FuncMod String [CodeDefinition]

funcMod :: String -> [QDefinition] -> FuncMod
funcMod n qd = FuncMod n $ map qtoc qd

data Choices = Choices {
  lang :: [Lang],
  impType :: ImplementationType,
  logFile :: String,
  logging :: Logging,
  comments :: Comments,
  onSfwrConstraint :: ConstraintBehaviour,
  onPhysConstraint :: ConstraintBehaviour,
  inputStructure :: Structure
}

data Lang = Cpp
          | CSharp
          | Java
          | Python

data ImplementationType = Library
                        | Program

data Logging = LogNone
             | LogFunc
             | LogVar
             | LogAll
             deriving Eq
             
data Comments = CommentNone
              | CommentFunc
              deriving Eq
             
data ConstraintBehaviour = Warning
                         | Exception
                         
data Structure = Loose
               | AsClass
             
defaultChoices :: Choices
defaultChoices = Choices {
  lang = [Python],
  impType = Program,
  logFile = "log.txt",
  logging = LogNone,
  comments = CommentNone,
  onSfwrConstraint = Exception,
  onPhysConstraint = Warning,
  inputStructure = AsClass
}


-- medium hacks ---
data ModDef = ModDef String [FuncDef]

data FuncDef where
  FuncDef :: String -> [CodeChunk] -> CodeType -> [FuncStmt] -> FuncDef

funcDef :: (Quantity c, SymbolForm c) => String -> [c] -> Space -> [FuncStmt] -> FuncDef  
funcDef s i t fs = FuncDef s (map codevar i) (spaceToCodeType t) fs 
 
data FuncStmt where
  FAsg :: CodeChunk -> Expr -> FuncStmt
  FFor :: CodeChunk -> Expr -> [FuncStmt] -> FuncStmt
  FWhile :: Expr -> [FuncStmt] -> FuncStmt
  FCond :: Expr -> [FuncStmt] -> [FuncStmt] -> FuncStmt
  FRet :: Expr -> FuncStmt
  FThrow :: String -> FuncStmt
  FTry :: [FuncStmt] -> [FuncStmt] -> FuncStmt
  FContinue :: FuncStmt
  
fasg :: (Quantity c, SymbolForm c) => c -> Expr -> FuncStmt
fasg v e = FAsg (codevar v) e

ffor :: (Quantity c, SymbolForm c) => c -> Expr -> [FuncStmt] -> FuncStmt
ffor v e fs = FFor (codevar v) e fs

addModDefs :: CodeSpec -> [ModDef] -> CodeSpec
addModDefs cs@(CodeSpec{ modDefs = md }) mdnew = cs { modDefs = md ++ mdnew }

---- major hacks ----
modHack :: [(String, [FunctionDecl])]
modHack = [("ReadTable", [read_z_array_func, read_x_array_func, read_y_array_func])--,
           --("Interpolation", [lin_interp_func, indInSeq_func, matrixCol_func, interpY_func, interpZ_func])
          ] 