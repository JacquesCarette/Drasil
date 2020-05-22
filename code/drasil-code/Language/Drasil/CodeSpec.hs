{-# LANGUAGE GADTs #-}
module Language.Drasil.CodeSpec where

import Language.Drasil
import Database.Drasil (ChunkDB, SystemInformation(SI), symbResolve,
  _authors, _constants, _constraints, _datadefs, _definitions, _inputs,
  _outputs, _sys, _sysinfodb)
import Language.Drasil.Development (namesRI)
import Theory.Drasil (DataDefinition, qdFromDD)

import Language.Drasil.Chunk.Code (CodeChunk, CodeVarChunk, CodeIdea(codeChunk),
  ConstraintMap, programName, quantvar, codevars, codevars', 
  varResolve, constraintMap)
import Language.Drasil.Chunk.CodeDefinition (CodeDefinition, qtov, qtoc, odeDef,
  auxExprs, codeEquat)
import Language.Drasil.Choices (Choices(..))
import Language.Drasil.Mod (Func(..), FuncData(..), FuncDef(..), Mod(..), Name)

import Control.Lens ((^.))
import Data.List (intercalate, nub, (\\))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

import Prelude hiding (const)

type Input = CodeVarChunk
type Output = CodeVarChunk
type Const = CodeDefinition
type Derived = CodeDefinition
type Def = CodeDefinition

data CodeSpec where
  CodeSpec :: (HasName a) => {
  pName :: Name,
  authors :: [a], 
  inputs :: [Input],
  extInputs :: [Input],
  derivedInputs :: [Derived],
  outputs :: [Output],
  execOrder :: [Def],
  cMap :: ConstraintMap,
  constants :: [Const],
  constMap :: ConstantMap,
  mods :: [Mod],  -- medium hack
  sysinfodb :: ChunkDB
  } -> CodeSpec

type ConstantMap = Map.Map String CodeDefinition

assocToMap :: HasUID a => [a] -> Map.Map UID a
assocToMap = Map.fromList . map (\x -> (x ^. uid, x))

codeSpec :: SystemInformation -> Choices -> [Mod] -> CodeSpec
codeSpec SI {_sys = sys
              , _authors = as
              , _definitions = defs'
              , _datadefs = ddefs
              , _inputs = ins
              , _outputs = outs
              , _constraints = cs
              , _constants = cnsts
              , _sysinfodb = db} chs ms = 
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
  in  CodeSpec {
        pName = n,
        authors = as,
        inputs = allInputs,
        extInputs = inputs',
        derivedInputs = map qtov derived,
        outputs = outs',
        execOrder = exOrder,
        cMap = constraintMap cs,
        constants = const',
        constMap = assocToMap const',
        mods = ms,
        sysinfodb = db
      }

-- medium hacks ---
relToQD :: ExprRelat c => ChunkDB -> c -> QDefinition
relToQD sm r = convertRel sm (r ^. relat)

convertRel :: ChunkDB -> Expr -> QDefinition
convertRel sm (BinaryOp Eq (C x) r) = ec (symbResolve sm x) r
convertRel _ _ = error "Conversion failed"

asVC :: Func -> QuantityDict
asVC (FDef (FuncDef n _ _ _ _ _)) = implVar n (nounPhraseSP n) Real (Variable n)
asVC (FDef (CtorDef n _ _ _ _)) = implVar n (nounPhraseSP n) Real (Variable n)
asVC (FData (FuncData n _ _)) = implVar n (nounPhraseSP n) Real (Variable n)

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
        

-- Determines the derived inputs, which can be immediately calculated from the 
-- knowns (inputs and constants). If there are DDs, the derived inputs will 
-- come from those. If there are none, then the QDefinitions are used instead.
getDerivedInputs :: [DataDefinition] -> [QDefinition] -> [Input] -> [Const] ->
  ChunkDB -> [QDefinition]
getDerivedInputs ddefs defs' ins cnsts sm =
  let refSet = ins ++ map quantvar cnsts
  in  if null ddefs then filter ((`subsetOf` refSet) . flip codevars sm . (^.equat)) defs'
      else filter ((`subsetOf` refSet) . flip codevars sm . (^.defnExpr)) (map qdFromDD ddefs)

type Known = CodeVarChunk
type Need  = CodeVarChunk

getExecOrder :: [Def] -> [Known] -> [Need] -> ChunkDB -> [Def]
getExecOrder d k' n' sm  = getExecOrder' [] d k' (n' \\ k')
  where getExecOrder' ord _ _ []   = ord
        getExecOrder' ord defs' k n = 
          let new  = filter (\def -> (`subsetOf` k) (concatMap (`codevars'` sm)
                (codeEquat def : def ^. auxExprs) \\ [quantvar def])) defs'
              cnew = map quantvar new
              kNew = k ++ cnew
              nNew = n \\ cnew
          in  if null new 
              then error ("The following outputs cannot be computed: " ++
                       intercalate ", " (map (^. uid) n) ++ "\n"
                     ++ "Unused definitions are: " 
                       ++ intercalate ", " (map (^. uid) defs') ++ "\n"
                     ++ "Known values are: " 
                       ++ intercalate ", " (map (^. uid) k))
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
