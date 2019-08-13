module Language.Drasil.Code.Imperative.Parameters(
  getInputFormatIns, getInputFormatOuts, getDerivedIns, getDerivedOuts,
  getConstraintParams, getCalcParams, getOutputParams
) where

import Language.Drasil 
import Language.Drasil.Code.Imperative.State (State(..))
import Language.Drasil.Chunk.Code (CodeChunk, CodeIdea(codeChunk), codevar)
import Language.Drasil.Chunk.CodeDefinition (CodeDefinition, codeEquat)
import Language.Drasil.Code.CodeQuantityDicts (inFileName, inParams)
import Language.Drasil.CodeSpec (CodeSpec(..), CodeSystInfo(..), Structure(..), 
  codevars, codevars', constraintvarsandfuncs, getConstraints)

import Data.List (nub, (\\))
import Data.Map (member)
import Control.Monad.Reader (Reader, ask)
import Control.Lens ((^.))

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
  let getOuts :: Structure -> [CodeChunk]
      getOuts Unbundled = extInputs $ csi $ codeSpec g
      getOuts Bundled = []
  getParams $ getOuts (inStruct g)

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
  let getOuts :: Structure -> [CodeChunk]
      getOuts Unbundled = map codeChunk $ derivedInputs $ csi $ codeSpec g
      getOuts Bundled = []
  getParams $ getOuts (inStruct g)

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
      consts = map codeChunk $ constants $ csi $ codeSpec g
      inpVars = filter (`elem` ins) cs
      conVars = filter (`elem` consts) cs
      csSubIns = cs \\ (ins ++ consts)
      inVs = getInputVars (inStruct g) inpVars
      conVs = getConstVars conVars
  return $ inVs ++ conVs ++ csSubIns

getInputVars :: Structure -> [CodeChunk] -> [CodeChunk]
getInputVars _ [] = []
getInputVars Unbundled cs = cs
getInputVars Bundled _ = [codevar inParams]

-- Right now, we always inline constants. In the future, this will be captured by a choice and this function should be updated to read that choice
getConstVars :: [CodeChunk] -> [CodeChunk]
getConstVars _ = []
