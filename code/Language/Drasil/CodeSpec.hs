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
import Language.Drasil.DataDesc

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
  mods :: [Mod]  -- medium hack
  --mods :: [(String, [FunctionDecl])] -- big hack
}

type FunctionMap = Map.Map String CodeDefinition
type VarMap      = Map.Map String CodeChunk

assocToMap :: CodeIdea a => [a] -> Map.Map String a
assocToMap = Map.fromList . map (\x -> (codeName x, x))

funcTerm :: String -> FunctionMap -> String
funcTerm cname m = maybe "" (\cd -> getStr (phrase $ cd ^. term)) (Map.lookup cname m)
       
varTerm :: String -> VarMap -> String
varTerm cname m = maybe "" (\cc -> getStr (phrase $ cc ^. term)) (Map.lookup cname m)
        
varType :: String -> VarMap -> CodeType
varType cname m = maybe (error "Variable not found") codeType (Map.lookup cname m)
        
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
  fMap = assocToMap $ map qtoc defs,
  vMap = assocToMap (map codevar q),
  constMap = assocToMap $ map qtoc constants,
  fMods = [funcMod "Calculations" defs],
  const = map qtoc constants,
  choices = ch,
  mods = []
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

type Name = String

-- medium hacks ---
data Mod = ModDef Name [FuncDef]
         | ModData Name [DataDesc]

data FuncDef where
  FuncDef :: Name -> [CodeChunk] -> CodeType -> [FuncStmt] -> FuncDef

funcDef :: (Quantity c, SymbolForm c) => Name -> [c] -> Space -> [FuncStmt] -> FuncDef  
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
  FVal :: Expr -> FuncStmt
  FDec :: CodeChunk -> CodeType -> FuncStmt
  
fasg :: (Quantity c, SymbolForm c) => c -> Expr -> FuncStmt
fasg v e = FAsg (codevar v) e

ffor :: (Quantity c, SymbolForm c) => c -> Expr -> [FuncStmt] -> FuncStmt
ffor v e fs = FFor (codevar v) e fs

fdec :: (Quantity c, SymbolForm c) => c -> Space -> FuncStmt
fdec v t = FDec (codevar v) (spaceToCodeType t)

addModDefs :: CodeSpec -> [Mod] -> CodeSpec
addModDefs cs@(CodeSpec{ mods = md }) mdnew = cs { mods = md ++ mdnew }

---- major hacks ----
modHack :: [(String, [FunctionDecl])]
modHack = [("ReadTable", [read_z_array_func, read_x_array_func, read_y_array_func])--,
           --("Interpolation", [lin_interp_func, indInSeq_func, matrixCol_func, interpY_func, interpZ_func])
          ] 
