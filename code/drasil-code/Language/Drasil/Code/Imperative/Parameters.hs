module Language.Drasil.Code.Imperative.Parameters(getInConstructorParams,
  getInputFormatIns, getInputFormatOuts, getDerivedIns, getDerivedOuts,
  getConstraintParams, getCalcParams, getOutputParams
) where

import Language.Drasil 
import Language.Drasil.Code.Imperative.State (State(..))
import Language.Drasil.Chunk.Code (CodeChunk, CodeIdea(codeChunk), codevar)
import Language.Drasil.Chunk.CodeDefinition (CodeDefinition, codeEquat)
import Language.Drasil.Code.CodeQuantityDicts (inFileName, inParams, consts)
import Language.Drasil.CodeSpec (CodeSpec(..), CodeSystInfo(..), Structure(..), 
  ConstantStructure(..), ConstantRepr(..), codevars, codevars', 
  constraintvarsandfuncs, getConstraints)

import Data.List (nub, (\\))
import Data.Map (member, notMember)
import Control.Monad.Reader (Reader, ask)
import Control.Lens ((^.))

getInConstructorParams :: Reader State [CodeChunk]
getInConstructorParams = getInputFormatIns

getInputFormatIns :: Reader State [CodeChunk]
getInputFormatIns = do
  g <- ask
  let getIns :: Structure -> [CodeChunk]
      getIns Unbundled = []
      getIns Bundled = [codevar inParams]
  getParams $ codevar inFileName : getIns (inStruct g)

getInputFormatOuts :: Reader State [CodeChunk]
getInputFormatOuts = do
  g <- ask
  getParams $ extInputs $ csi $ codeSpec g

getDerivedIns :: Reader State [CodeChunk]
getDerivedIns = do
  g <- ask
  let s = csi $ codeSpec g
      dvals = derivedInputs s
      reqdVals = concatMap (flip codevars (sysinfodb s) . codeEquat) dvals
  getParams reqdVals

getDerivedOuts :: Reader State [CodeChunk]
getDerivedOuts = do
  g <- ask
  getParams $ map codeChunk $ derivedInputs $ csi $ codeSpec g

getConstraintParams :: Reader State [CodeChunk]
getConstraintParams = do 
  g <- ask
  let cm = cMap $ csi $ codeSpec g
      mem = eMap $ codeSpec g
      db = sysinfodb $ csi $ codeSpec g
      varsList = filter (\i -> member (i ^. uid) cm) (inputs $ csi $ codeSpec g)
      reqdVals = nub $ varsList ++ concatMap (\v -> constraintvarsandfuncs v db 
        mem) (getConstraints cm varsList)
  getParams reqdVals

getCalcParams :: CodeDefinition -> Reader State [CodeChunk]
getCalcParams c = do
  g <- ask
  getParams $ codevars' (codeEquat c) $ sysinfodb $ csi $ codeSpec g

getOutputParams :: Reader State [CodeChunk]
getOutputParams = do
  g <- ask
  getParams $ outputs $ csi $ codeSpec g

getParams :: (CodeIdea c) => [c] -> Reader State [CodeChunk]
getParams cs' = do
  g <- ask
  let cs = map codeChunk cs'
      ins = inputs $ csi $ codeSpec g
      cnsnts = map codeChunk $ constants $ csi $ codeSpec g
      inpVars = filter (`elem` ins) cs
      conVars = filter (`elem` cnsnts) cs
      csSubIns = filter ((`notMember` concMatches g) . (^. uid)) 
        (cs \\ (ins ++ cnsnts))
      inVs = getInputVars (inStruct g) Var inpVars
  conVs <- getConstVars (conStruct g) (conRepr g) conVars
  return $ nub $ inVs ++ conVs ++ csSubIns

getInputVars :: Structure -> ConstantRepr -> [CodeChunk] -> [CodeChunk]
getInputVars _ _ [] = []
getInputVars Unbundled _ cs = cs
getInputVars Bundled Var _ = [codevar inParams]
getInputVars Bundled Const _ = []

getConstVars :: ConstantStructure -> ConstantRepr -> [CodeChunk] -> 
  Reader State [CodeChunk]
getConstVars _ _ [] = return []
getConstVars (Store Unbundled) _ cs = return cs
getConstVars (Store Bundled) Var _ = return [codevar consts]
getConstVars (Store Bundled) Const _ = return []
getConstVars WithInputs cr cs = do
  g <- ask
  return $ getInputVars (inStruct g) cr cs
getConstVars Inline _ _ = return []
