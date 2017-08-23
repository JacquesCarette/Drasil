{-# LANGUAGE GADTs #-}
module Language.Drasil.CodeSpec where

import Language.Drasil.Chunk.Code
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.Quantity -- for hack
import Language.Drasil.Chunk.SymbolForm hiding (symbol) -- for hack
import Language.Drasil.NounPhrase
import Language.Drasil.Symbol
import Language.Drasil.Spec hiding (Mod)
import Language.Drasil.SystemInformation
import Language.Drasil.Expr -- for hack
import Language.Drasil.Space -- for hack
import Language.Drasil.DataDesc
import Language.Drasil.Chunk.ExprRelat
import Language.Drasil.ChunkDB
import Language.Drasil.Expr.Extract (codevars)
import Language.Drasil.Chunk.VarChunk
import Language.Drasil.Misc (symbol)

import qualified Data.Map as Map
import Control.Lens ((^.))
import Data.List (nub, delete, (\\))

import Prelude hiding (const)

data CodeSpec = CodeSpec {
  program :: CodeName,
  inputs :: [CodeChunk],
  extInputs :: [CodeChunk],
  derivedInputs :: [CodeDefinition],
  outputs :: [CodeChunk],
  relations :: [CodeDefinition],
  cMap :: ConstraintMap,
  fMap :: FunctionMap,
  vMap :: VarMap,
  eMap :: ModExportMap,
  constMap :: FunctionMap,
  const :: [CodeDefinition],
  choices :: Choices,
  mods :: [DMod]  -- medium hack
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

codeSpec :: SystemInformation -> [Mod] -> CodeSpec
codeSpec si ms = codeSpec' si defaultChoices ms

codeSpec' :: SystemInformation -> Choices -> [Mod] -> CodeSpec
codeSpec' (SI {_sys = sys, _quants = q, _definitions = defs, _inputs = ins, _outputs = outs, _constraints = cs, _constants = constants}) ch ms = 
  let inputs' = map codevar ins
      const' = map qtoc constants
      defs' = map qtoc defs
      derived = getDerivedInputs defs' inputs' const'
      rels = defs' \\ derived
      mods' = (packmod "Calculations" $ map FCD rels):ms 
      mem   = modExportMap mods' inputs' const'
  in  CodeSpec {
        program = NICN sys,
        inputs = inputs' ++ map codevar derived,
        extInputs = inputs',
        derivedInputs = derived,
        outputs = map codevar outs,
        relations = rels,
        cMap = constraintMap cs,
        fMap = assocToMap $ rels,
        vMap = assocToMap (map codevar q),
        eMap = mem,
        constMap = assocToMap $ const',
        const = const',
        choices = ch,
        mods = map (getModDep mem) mods'
      }

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
             
data Comments = CommentNone
              | CommentFunc
             
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
relToQD :: (ExprRelat c) => SymbolMap -> c -> QDefinition
relToQD sm r = convertRel sm $ r ^. relat

convertRel :: SymbolMap -> Expr -> QDefinition
convertRel sm ((C x) := r) = EC (symbLookup x sm) r
convertRel _ _ = error "Conversion failed"

data Mod = Mod Name [Func]

packmod :: Name -> [Func] -> Mod
packmod n fs = Mod (toCodeName n) fs

data DMod = DMod [Name] Mod
     
data Func = FCD CodeDefinition
          | FDef FuncDef
          | FData FuncData

funcQD :: QDefinition -> Func
funcQD qd = FCD $ qtoc qd

funcData :: Name -> DataDesc -> Func
funcData n dd = FData $ FuncData (toCodeName n) dd 

funcDef :: (Quantity c, SymbolForm c) => Name -> [c] -> Space -> [FuncStmt] -> Func  
funcDef s i t fs = FDef $ FuncDef (toCodeName s) (map codevar i) (spaceToCodeType t) fs 
     
data FuncData where
  FuncData :: Name -> DataDesc -> FuncData
  
data FuncDef where
  FuncDef :: Name -> [CodeChunk] -> CodeType -> [FuncStmt] -> FuncDef
 
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

asVC :: Func -> VarChunk
asVC (FDef (FuncDef n _ _ _)) = makeVC n (nounPhraseSP n) (Atomic n)
asVC (FData (FuncData n _)) = makeVC n (nounPhraseSP n) (Atomic n)
asVC (FCD cd) = vc' cd (symbol cd) (cd ^. typ)

asExpr :: Func -> Expr
asExpr f = C $ asVC f


-- name of variable/function maps to module name
type ModExportMap = Map.Map String String

modExportMap :: [Mod] -> [CodeChunk] -> [CodeDefinition] -> ModExportMap
modExportMap ms ins _ = Map.fromList $ concatMap mpair ms
  where mpair (Mod n fs) = map fname fs `zip` repeat n
                        ++ map codeName ins `zip` repeat "InputParameters"
                     --   ++ map codeName consts `zip` repeat "Constants"
                     -- inlining constants for now
          
getModDep :: ModExportMap -> Mod -> DMod
getModDep mem m@(Mod name funcs) = 
  DMod (delete name $ nub $ concatMap getDep (concatMap fdep funcs)) m
  where getDep n = maybe [] (\x -> [x]) (Map.lookup n mem)        
        fdep (FCD cd) = codeName cd:map codeName (codevars $ codeEquat cd)
        fdep (FDef (FuncDef _ i _ fs)) = map codeName (i ++ concatMap fstdep fs)
        fdep (FData (FuncData _ d)) = map codeName $ getInputs d   
        fstdep (FDec cc _) = [cc]
        fstdep (FAsg cc e) = cc:codevars e
        fstdep (FFor cc e fs) = cc:(codevars e ++ concatMap fstdep fs)
        fstdep (FWhile e fs) = codevars e ++ concatMap fstdep fs
        fstdep (FCond e tfs efs) = codevars e ++ concatMap fstdep tfs ++ concatMap fstdep efs
        fstdep (FRet e) = codevars e
        fstdep (FTry tfs cfs) = concatMap fstdep tfs ++ concatMap fstdep cfs
        fstdep (FVal e) = codevars e
        fstdep _ = []
       
fname :: Func -> Name       
fname (FCD cd) = codeName cd
fname (FDef (FuncDef n _ _ _)) = n
fname (FData (FuncData n _)) = n 


getDerivedInputs :: [CodeDefinition] -> [CodeChunk] -> [CodeDefinition] -> [CodeDefinition]
getDerivedInputs defs ins consts =
  let refSet = ins ++ map codevar consts 
  in  filter (null . (filter (not . (`elem` refSet))) . codevars . codeEquat) defs