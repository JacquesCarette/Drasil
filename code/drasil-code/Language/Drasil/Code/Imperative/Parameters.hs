module Language.Drasil.Code.Imperative.Parameters(getInConstructorParams,
  getInputFormatIns, getInputFormatOuts, getDerivedIns, getDerivedOuts,
  getConstraintParams, getCalcParams, getOutputParams
) where

import Language.Drasil 
import Language.Drasil.Code.Imperative.DrasilState (DrasilState(..), inMod)
import Language.Drasil.Chunk.Code (CodeVarChunk, CodeIdea(codeChunk), 
  quantvar, codevars, codevars')
import Language.Drasil.Chunk.CodeDefinition (CodeDefinition, codeEquat)
import Language.Drasil.Code.CodeQuantityDicts (inFileName, inParams, consts)
import Language.Drasil.CodeSpec (CodeSpec(..), CodeSystInfo(..), Structure(..), 
  InputModule(..), ConstantStructure(..), ConstantRepr(..),
  constraintvarsandfuncs, getConstraints)

import Data.List (nub, (\\))
import Data.Map (member, notMember)
import Control.Monad.Reader (Reader, ask)
import Control.Lens ((^.))

data ParamType = In | Out deriving Eq

isIn :: ParamType -> Bool
isIn = (In ==)

getInConstructorParams :: Reader DrasilState [CodeVarChunk]
getInConstructorParams = do
  g <- ask
  ifPs <- getInputFormatIns
  dvPs <- getDerivedIns
  icPs <- getConstraintParams
  ps <- getParams In $ ifPs ++ dvPs ++ icPs
  return $ ps \\ (quantvar inParams : inputs (csi $ codeSpec g))

getInputFormatIns :: Reader DrasilState [CodeVarChunk]
getInputFormatIns = do
  g <- ask
  let getIns :: Structure -> InputModule -> [CodeVarChunk]
      getIns Bundled Separated = [quantvar inParams]
      getIns _ _ = []
  getParams In $ quantvar inFileName : getIns (inStruct g) (inMod g)

getInputFormatOuts :: Reader DrasilState [CodeVarChunk]
getInputFormatOuts = do
  g <- ask
  getParams Out $ extInputs $ csi $ codeSpec g

getDerivedIns :: Reader DrasilState [CodeVarChunk]
getDerivedIns = do
  g <- ask
  let s = csi $ codeSpec g
      dvals = derivedInputs s
      reqdVals = concatMap (flip codevars (sysinfodb s) . codeEquat) dvals
  getParams In reqdVals

getDerivedOuts :: Reader DrasilState [CodeVarChunk]
getDerivedOuts = do
  g <- ask
  getParams Out $ map codeChunk $ derivedInputs $ csi $ codeSpec g

getConstraintParams :: Reader DrasilState [CodeVarChunk]
getConstraintParams = do 
  g <- ask
  let cm = cMap $ csi $ codeSpec g
      mem = eMap $ codeSpec g
      db = sysinfodb $ csi $ codeSpec g
      varsList = filter (\i -> member (i ^. uid) cm) (inputs $ csi $ codeSpec g)
      reqdVals = nub $ varsList ++ map quantvar (concatMap (\v -> 
        constraintvarsandfuncs v db mem) (getConstraints cm varsList))
  getParams In reqdVals

getCalcParams :: CodeDefinition -> Reader DrasilState [CodeVarChunk]
getCalcParams c = do
  g <- ask
  getParams In $ codevars' (codeEquat c) $ sysinfodb $ csi $ codeSpec g

getOutputParams :: Reader DrasilState [CodeVarChunk]
getOutputParams = do
  g <- ask
  getParams In $ outputs $ csi $ codeSpec g

getParams :: (Quantity c, MayHaveUnit c) => ParamType -> [c] -> 
  Reader DrasilState [CodeVarChunk]
getParams pt cs' = do
  g <- ask
  let cs = map quantvar cs'
      ins = inputs $ csi $ codeSpec g
      cnsnts = map quantvar $ constants $ csi $ codeSpec g
      inpVars = filter (`elem` ins) cs
      conVars = filter (`elem` cnsnts) cs
      csSubIns = filter ((`notMember` concMatches g) . (^. uid)) 
        (cs \\ (ins ++ cnsnts))
  inVs <- getInputVars pt (inStruct g) Var inpVars
  conVs <- getConstVars pt (conStruct g) (conRepr g) conVars
  return $ nub $ inVs ++ conVs ++ csSubIns

getInputVars :: ParamType -> Structure -> ConstantRepr -> [CodeVarChunk] -> 
  Reader DrasilState [CodeVarChunk]
getInputVars _ _ _ [] = return []
getInputVars _ Unbundled _ cs = return cs
getInputVars pt Bundled Var _ = do
  g <- ask
  let cname = "InputParameters"
  return [quantvar inParams | currentClass g /= cname && isIn pt]
getInputVars _ Bundled Const _ = return []

getConstVars :: ParamType -> ConstantStructure -> ConstantRepr -> 
  [CodeVarChunk] -> Reader DrasilState [CodeVarChunk]
getConstVars _ _ _ [] = return []
getConstVars _ (Store Unbundled) _ cs = return cs
getConstVars pt (Store Bundled) Var _ = return [quantvar consts | isIn pt]
getConstVars _ (Store Bundled) Const _ = return []
getConstVars pt WithInputs cr cs = do
  g <- ask
  getInputVars pt (inStruct g) cr cs
getConstVars _ Inline _ _ = return []
