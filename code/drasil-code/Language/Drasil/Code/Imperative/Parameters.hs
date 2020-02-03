module Language.Drasil.Code.Imperative.Parameters(getInConstructorParams,
  getInputFormatIns, getInputFormatOuts, getDerivedIns, getDerivedOuts,
  getConstraintParams, getCalcParams, getOutputParams
) where

import Language.Drasil 
import Language.Drasil.Code.Imperative.DrasilState (DrasilState(..))
import Language.Drasil.Chunk.Code (CodeChunk, CodeIdea(codeChunk), codevar)
import Language.Drasil.Chunk.CodeDefinition (CodeDefinition, codeEquat)
import Language.Drasil.Code.CodeQuantityDicts (inFileName, inParams, consts)
import Language.Drasil.CodeSpec (CodeSpec(..), CodeSystInfo(..), Structure(..), 
  InputModule(..), ConstantStructure(..), ConstantRepr(..), codevars, codevars',
  constraintvarsandfuncs, getConstraints)

import Data.List (nub, (\\))
import Data.Map (member, notMember)
import Control.Monad.Reader (Reader, ask)
import Control.Lens ((^.))

data ParamType = In | Out deriving Eq

isIn :: ParamType -> Bool
isIn = (In ==)

getInConstructorParams :: Reader DrasilState [CodeChunk]
getInConstructorParams = do
  g <- ask
  let getCParams False = []
      getCParams True = [codevar inFileName]
  getParams In $ getCParams $ member "InputParameters" (eMap $ codeSpec g) && 
    member "get_input" (defMap $ codeSpec g)

getInputFormatIns :: Reader DrasilState [CodeChunk]
getInputFormatIns = do
  g <- ask
  let getIns :: Structure -> InputModule -> [CodeChunk]
      getIns Bundled Separated = [codevar inParams]
      getIns _ _ = []
  getParams In $ codevar inFileName : getIns (inStruct g) (inMod g)

getInputFormatOuts :: Reader DrasilState [CodeChunk]
getInputFormatOuts = do
  g <- ask
  getParams Out $ extInputs $ csi $ codeSpec g

getDerivedIns :: Reader DrasilState [CodeChunk]
getDerivedIns = do
  g <- ask
  let s = csi $ codeSpec g
      dvals = derivedInputs s
      reqdVals = concatMap (flip codevars (sysinfodb s) . codeEquat) dvals
  getParams In reqdVals

getDerivedOuts :: Reader DrasilState [CodeChunk]
getDerivedOuts = do
  g <- ask
  getParams Out $ map codeChunk $ derivedInputs $ csi $ codeSpec g

getConstraintParams :: Reader DrasilState [CodeChunk]
getConstraintParams = do 
  g <- ask
  let cm = cMap $ csi $ codeSpec g
      mem = eMap $ codeSpec g
      db = sysinfodb $ csi $ codeSpec g
      varsList = filter (\i -> member (i ^. uid) cm) (inputs $ csi $ codeSpec g)
      reqdVals = nub $ varsList ++ concatMap (\v -> constraintvarsandfuncs v db 
        mem) (getConstraints cm varsList)
  getParams In reqdVals

getCalcParams :: CodeDefinition -> Reader DrasilState [CodeChunk]
getCalcParams c = do
  g <- ask
  getParams In $ codevars' (codeEquat c) $ sysinfodb $ csi $ codeSpec g

getOutputParams :: Reader DrasilState [CodeChunk]
getOutputParams = do
  g <- ask
  getParams In $ outputs $ csi $ codeSpec g

getParams :: (CodeIdea c) => ParamType -> [c] -> Reader DrasilState [CodeChunk]
getParams pt cs' = do
  g <- ask
  let cs = map codeChunk cs'
      ins = inputs $ csi $ codeSpec g
      cnsnts = map codeChunk $ constants $ csi $ codeSpec g
      inpVars = filter (`elem` ins) cs
      conVars = filter (`elem` cnsnts) cs
      csSubIns = filter ((`notMember` concMatches g) . (^. uid)) 
        (cs \\ (ins ++ cnsnts))
  inVs <- getInputVars pt (inStruct g) Var inpVars
  conVs <- getConstVars pt (conStruct g) (conRepr g) conVars
  return $ nub $ inVs ++ conVs ++ csSubIns

getInputVars :: ParamType -> Structure -> ConstantRepr -> [CodeChunk] -> 
  Reader DrasilState [CodeChunk]
getInputVars _ _ _ [] = return []
getInputVars _ Unbundled _ cs = return cs
getInputVars pt Bundled Var _ = do
  g <- ask
  let mname = "InputParameters"
  return [codevar inParams | not (currentModule g == mname && member mname 
    (eMap $ codeSpec g)) && isIn pt]
getInputVars _ Bundled Const _ = return []

getConstVars :: ParamType -> ConstantStructure -> ConstantRepr -> [CodeChunk] 
  -> Reader DrasilState [CodeChunk]
getConstVars _ _ _ [] = return []
getConstVars _ (Store Unbundled) _ cs = return cs
getConstVars pt (Store Bundled) Var _ = return [codevar consts | isIn pt]
getConstVars _ (Store Bundled) Const _ = return []
getConstVars pt WithInputs cr cs = do
  g <- ask
  getInputVars pt (inStruct g) cr cs
getConstVars _ Inline _ _ = return []
