{-# LANGUAGE GADTs #-}
module Language.Drasil.CodeSpec where

import Language.Drasil
import Database.Drasil (ChunkDB, SystemInformation(SI), symbResolve,
  _authors, _constants, _constraints, _datadefs, _definitions, _inputs,
  _outputs, _quants, _sys, _sysinfodb, sampleData)
import Language.Drasil.Development (namesRI)
import Theory.Drasil (DataDefinition, qdFromDD)

import Language.Drasil.Chunk.Code (CodeChunk, CodeVarChunk, CodeIdea(codeChunk),
  ConstraintMap, programName, codevarC, codevar, quantvar, codevars, codevars', 
  varResolve, constraintMap)
import Language.Drasil.Chunk.CodeDefinition (CodeDefinition, qtov, qtoc, odeDef,
  auxExprs, codeEquat)
import Language.Drasil.Code.Code (spaceToCodeType)
import Language.Drasil.Code.CodeQuantityDicts (inFileName, inParams, consts)
import Language.Drasil.Code.Lang (Lang(..))
import Language.Drasil.Data.ODEInfo (ODEInfo)
import Language.Drasil.Data.ODELibPckg (ODELibPckg)
import Language.Drasil.Mod (Class(..), Func(..), FuncData(..), FuncDef(..), 
  Mod(..), Name, getFuncParams)

import GOOL.Drasil (CodeType)

import Control.Lens ((^.))
import Data.List (nub, (\\))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

import Prelude hiding (const)

type Input = CodeVarChunk
type Output = CodeVarChunk
type Const = CodeDefinition
type Derived = CodeDefinition
type Def = CodeDefinition

data CodeSystInfo where
  CSI :: (HasName a) => {
  pName :: Name,
  authors :: [a], 
  inputs :: [Input],
  extInputs :: [Input],
  derivedInputs :: [Derived],
  outputs :: [Output],
  execOrder :: [Def],
  cMap :: ConstraintMap,
  constants :: [Const],
  mods :: [Mod],  -- medium hack
  sysinfodb :: ChunkDB,
  smplData :: FilePath
  } -> CodeSystInfo

data CodeSpec where
  CodeSpec :: {
  relations :: [Def],
  fMap :: FunctionMap,
  codequants :: [CodeVarChunk],
  constMap :: FunctionMap,
  csi :: CodeSystInfo
  } -> CodeSpec

type FunctionMap = Map.Map String CodeDefinition

assocToMap :: HasUID a => [a] -> Map.Map UID a
assocToMap = Map.fromList . map (\x -> (x ^. uid, x))

codeSpec :: SystemInformation -> Choices -> [Mod] -> CodeSpec
codeSpec SI {_sys = sys
              , _authors = as
              , _quants = q
              , _definitions = defs'
              , _datadefs = ddefs
              , _inputs = ins
              , _outputs = outs
              , _constraints = cs
              , _constants = cnsts
              , _sysinfodb = db
              , sampleData = sd} chs ms = 
  let n = programName sys
      inputs' = map quantvar ins
      const' = map qtov (filter ((`Map.notMember` conceptMatch chs) . (^. uid)) 
        cnsts)
      derived = getDerivedInputs ddefs defs' inputs' const' db
      rels = map qtoc ((defs' ++ map qdFromDD ddefs) \\ derived) 
        ++ map odeDef (odes chs)
      outs' = map quantvar outs
      allInputs = nub $ inputs' ++ map quantvar derived
      exOrder = getExecOrder rels (allInputs ++ map quantvar cnsts) outs' db
      csi' = CSI {
        pName = n,
        authors = as,
        inputs = allInputs,
        extInputs = inputs',
        derivedInputs = map qtov derived,
        outputs = outs',
        execOrder = exOrder,
        cMap = constraintMap cs,
        constants = const',
        mods = ms,
        sysinfodb = db,
        smplData = sd
      }
  in  CodeSpec {
        relations = rels,
        fMap = assocToMap rels,
        codequants = map quantvar q,
        constMap = assocToMap const',
        csi = csi'
      }

data Choices = Choices {
  lang :: [Lang],
  modularity :: Modularity,
  impType :: ImplementationType,
  logFile :: String,
  logging :: Logging,
  comments :: [Comments],
  doxVerbosity :: Verbosity,
  dates :: Visibility,
  onSfwrConstraint :: ConstraintBehaviour,
  onPhysConstraint :: ConstraintBehaviour,
  inputStructure :: Structure,
  constStructure :: ConstantStructure,
  constRepr :: ConstantRepr,
  conceptMatch :: ConceptMatchMap,
  spaceMatch :: SpaceMatch,
  auxFiles :: [AuxFile],
  odeLib :: [ODELibPckg],
  -- FIXME: ODEInfos should be automatically built from Instance models when 
  -- needed, but we can't do that yet so I'm passing it through Choices instead.
  -- This choice should really just be for an ODEMethod
  odes :: [ODEInfo]
}

data Modularity = Modular InputModule | Unmodular

data ImplementationType = Library
                        | Program

data Logging = LogNone
             | LogFunc
             | LogVar
             | LogAll
             
data Comments = CommentFunc
              | CommentClass
              | CommentMod deriving Eq

data Verbosity = Verbose | Quiet
             
data ConstraintBehaviour = Warning
                         | Exception
                         
data Structure = Unbundled
               | Bundled

data ConstantStructure = Inline | WithInputs | Store Structure

data ConstantRepr = Var | Const

data InputModule = Combined
                 | Separated

type ConceptMatchMap = Map.Map UID [CodeConcept]
type MatchedConceptMap = Map.Map UID CodeConcept
type SpaceMatch = Space -> [CodeType]
type MatchedSpaces = Space -> CodeType

data CodeConcept = Pi

matchConcepts :: (HasUID c) => [c] -> [[CodeConcept]] -> ConceptMatchMap
matchConcepts cncs cdcs = Map.fromList (zip (map (^. uid) cncs) cdcs)

matchSpace :: Space -> [CodeType] -> SpaceMatch -> SpaceMatch
matchSpace _ [] _ = error "Must match each Space to at least one CodeType"
matchSpace s ts sm = \sp -> if sp == s then ts else sm sp

matchSpaces :: [Space] -> [[CodeType]] -> SpaceMatch -> SpaceMatch
matchSpaces (s:ss) (ct:cts) sm = matchSpaces ss cts $ matchSpace s ct sm
matchSpaces [] [] sm = sm
matchSpaces _ _ _ = error "Lists passed to matchSpaces must have equal size"

data AuxFile = SampleInput deriving Eq
             
data Visibility = Show
                | Hide

inputModule :: Choices -> InputModule
inputModule c = inputModule' $ modularity c
  where inputModule' Unmodular = Combined
        inputModule' (Modular im) = im

defaultChoices :: Choices
defaultChoices = Choices {
  lang = [Python],
  modularity = Modular Combined,
  impType = Program,
  logFile = "log.txt",
  logging = LogNone,
  comments = [],
  doxVerbosity = Verbose,
  dates = Hide,
  onSfwrConstraint = Exception,
  onPhysConstraint = Warning,
  inputStructure = Bundled,
  constStructure = Inline,
  constRepr = Const,
  conceptMatch = matchConcepts ([] :: [QDefinition]) [],
  spaceMatch = spaceToCodeType, 
  auxFiles = [SampleInput],
  odeLib = [],
  odes = []
}

-- medium hacks ---
relToQD :: ExprRelat c => ChunkDB -> c -> QDefinition
relToQD sm r = convertRel sm (r ^. relat)

convertRel :: ChunkDB -> Expr -> QDefinition
convertRel sm (BinaryOp Eq (C x) r) = ec (symbResolve sm x) r
convertRel _ _ = error "Conversion failed"

asVC :: Func -> QuantityDict
asVC (FDef (FuncDef n _ _ _ _ _)) = implVar n (nounPhraseSP n) (Variable n) Real
asVC (FDef (CtorDef n _ _ _ _)) = implVar n (nounPhraseSP n) (Variable n) Real
asVC (FData (FuncData n _ _)) = implVar n (nounPhraseSP n) (Variable n) Real
asVC (FCD _) = error "Can't make QuantityDict from FCD function" -- codeVC cd (codeSymb cd) (cd ^. typ)

funcUID :: Func -> UID
funcUID f = asVC f ^. uid

-- FIXME: hack. Use for implementation-stage functions that need to be displayed in the SRS.
funcUID' :: Func -> UID
funcUID' f = asVC' f ^. uid

-- FIXME: Part of above hack
asVC' :: Func -> QuantityDict
asVC' (FDef (FuncDef n _ _ _ _ _)) = vc n (nounPhraseSP n) (Variable n) Real
asVC' (FDef (CtorDef n _ _ _ _)) = vc n (nounPhraseSP n) (Variable n) Real
asVC' (FData (FuncData n _ _)) = vc n (nounPhraseSP n) (Variable n) Real
asVC' (FCD _) = error "Can't make QuantityDict from FCD function" -- vc'' cd (codeSymb cd) (cd ^. typ)

getAdditionalVars :: Choices -> [Mod] -> [CodeVarChunk]
getAdditionalVars chs ms = map codevar (inFileName 
  : inParamsVar (inputStructure chs) 
  ++ constsVar (constStructure chs))
  ++ concatMap funcParams ms
  where inParamsVar Bundled = [inParams]
        inParamsVar Unbundled = []
        constsVar (Store Bundled) = [consts]
        constsVar _ = []
        funcParams (Mod _ _ _ cs fs) = concatMap getFuncParams (fs ++ 
          concatMap methods cs)

getDerivedInputs :: [DataDefinition] -> [QDefinition] -> [Input] -> [Const] ->
  ChunkDB -> [QDefinition]
getDerivedInputs ddefs defs' ins cnsts sm =
  let refSet = ins ++ map codevarC cnsts
  in  if null ddefs then filter ((`subsetOf` refSet) . flip codevars sm . (^.equat)) defs'
      else filter ((`subsetOf` refSet) . flip codevars sm . (^.defnExpr)) (map qdFromDD ddefs)

type Known = CodeVarChunk
type Need  = CodeVarChunk

getExecOrder :: [Def] -> [Known] -> [Need] -> ChunkDB -> [Def]
getExecOrder d k' n' sm  = getExecOrder' [] d k' (n' \\ k')
  where getExecOrder' ord _ _ []   = ord
        getExecOrder' ord defs' k n = 
          let new  = filter (\def -> (`subsetOf` k) (concatMap (`codevars'` sm)
                (codeEquat def : def ^. auxExprs) \\ [codevarC def])) defs'
              cnew = map codevarC new
              kNew = k ++ cnew
              nNew = n \\ cnew
          in  if null new 
              then error ("Cannot find path from inputs to outputs: " ++
                        show (map (^. uid) n)
                        ++ " given Defs as " ++ show (map (^. uid) defs')
                        ++ " and Knowns as " ++ show (map (^. uid) k))
              else getExecOrder' (ord ++ new) (defs' \\ new) kNew nNew

subsetOf :: (Eq a) => [a] -> [a] -> Bool
xs `subsetOf` ys = all (`elem` ys) xs

-- | Get a list of Constraints for a list of CodeChunks
getConstraints :: (HasUID c) => ConstraintMap -> [c] -> [Constraint]
getConstraints cm cs = concat $ mapMaybe (\c -> Map.lookup (c ^. uid) cm) cs

-- | Get a list of CodeChunks from a constraint
constraintvars :: Constraint -> ChunkDB -> [CodeChunk]
constraintvars (Range _ ri) m = map (codeChunk . varResolve m) $ nub $ 
  namesRI ri
constraintvars _ _ = []
