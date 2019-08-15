module Language.Drasil.Code.Imperative.Descriptions (
  modDesc, inputParametersDesc, inputFormatDesc, derivedValuesDesc, 
  inputConstraintsDesc, outputFormatDesc, inputClassDesc, inFmtFuncDesc,
  inConsFuncDesc, dvFuncDesc, woFuncDesc
) where

import Utils.Drasil (stringList)

import Language.Drasil
import Language.Drasil.Code.Imperative.State (State(..))
import Language.Drasil.CodeSpec (CodeSpec(..), CodeSystInfo(..), 
  InputModule(..), Structure(..))

import qualified Data.Map as Map (lookup, elems)
import Control.Monad.Reader (Reader, ask)

modDesc :: Reader State [String] -> Reader State String
modDesc = fmap ((++) "Provides " . stringList)

inputParametersDesc :: Reader State [String]
inputParametersDesc = do
  g <- ask
  ifDesc <- inputFormatDesc
  dvDesc <- derivedValuesDesc
  icDesc <- inputConstraintsDesc
  let im = inMod g
      st = inStruct g
      ipDesc Separated = inDesc st
      ipDesc Combined = inDesc st ++ [ifDesc, dvDesc, icDesc]
      inDesc Bundled = ["the structure for holding input values"]
      inDesc Unbundled = [""]
  return $ ipDesc im

inputFormatDesc :: Reader State String
inputFormatDesc = do
  g <- ask
  let ifDesc Nothing = ""
      ifDesc _ = "the function for reading inputs"
  return $ ifDesc $ Map.lookup "get_input" (eMap $ codeSpec g)

derivedValuesDesc :: Reader State String
derivedValuesDesc = do
  g <- ask
  let dvDesc Nothing = ""
      dvDesc _ = "the function for calculating derived values"
  return $ dvDesc $ Map.lookup "derived_values" (eMap $ codeSpec g)

inputConstraintsDesc :: Reader State String
inputConstraintsDesc = do
  g <- ask
  pAndS <- physAndSfwrCons
  let icDesc Nothing = ""
      icDesc _ = "the function for checking the " ++ pAndS ++ 
        " on the input"
  return $ icDesc $ Map.lookup "input_constraints" (eMap $ codeSpec g)

outputFormatDesc :: Reader State String
outputFormatDesc = do
  g <- ask
  let ofDesc Nothing = ""
      ofDesc _ = "the function for writing outputs"
  return $ ofDesc $ Map.lookup "write_output" (eMap $ codeSpec g)

inputClassDesc :: Reader State String
inputClassDesc = do
  g <- ask
  let inClassD [] = ""
      inClassD _ = "Structure for holding the " ++ stringList [
        inPs $ extInputs $ csi $ codeSpec g,
        dVs $ Map.lookup "derived_values" (eMap $ codeSpec g)]
      inPs [] = ""
      inPs _ = "input values"
      dVs Nothing = ""
      dVs _ = "derived values"
  return $ inClassD $ inputs $ csi $ codeSpec g

inFmtFuncDesc :: Reader State String
inFmtFuncDesc = do
  g <- ask
  let ifDesc Nothing = ""
      ifDesc _ = "Reads input from a file with the given file name"
  return $ ifDesc $ Map.lookup "get_input" (eMap $ codeSpec g)

inConsFuncDesc :: Reader State String
inConsFuncDesc = do
  g <- ask
  pAndS <- physAndSfwrCons
  let icDesc Nothing = ""
      icDesc _ = "Verifies that input values satisfy the " ++ pAndS
  return $ icDesc $ Map.lookup "input_constraints" (eMap $ codeSpec g)

dvFuncDesc :: Reader State String
dvFuncDesc = do
  g <- ask
  let dvDesc Nothing = ""
      dvDesc _ = "Calculates values that can be immediately derived from the" ++
        " inputs"
  return $ dvDesc $ Map.lookup "derived_values" (eMap $ codeSpec g)

woFuncDesc :: Reader State String
woFuncDesc = do
  g <- ask
  let woDesc Nothing = ""
      woDesc _ = "Writes the output values to output.txt"
  return $ woDesc $ Map.lookup "write_output" (eMap $ codeSpec g)

physAndSfwrCons :: Reader State String
physAndSfwrCons = do
  g <- ask
  let cns = concat $ Map.elems (cMap $ csi $ codeSpec g)
  return $ stringList [
    if null (map isPhysC cns) then "" else "physical constraints",
    if null (map isSfwrC cns) then "" else "software constraints"]