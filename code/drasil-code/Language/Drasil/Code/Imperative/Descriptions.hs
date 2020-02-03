module Language.Drasil.Code.Imperative.Descriptions (
  modDesc, inputParametersDesc, inputConstructorDesc, inputFormatDesc, 
  derivedValuesDesc, inputConstraintsDesc, constModDesc, outputFormatDesc, 
  inputClassDesc, constClassDesc, inFmtFuncDesc, inConsFuncDesc, dvFuncDesc, 
  woFuncDesc
) where

import Utils.Drasil (stringList)

import Language.Drasil
import Language.Drasil.Code.Imperative.DrasilState (DrasilState(..), inMod)
import Language.Drasil.Chunk.Code (CodeIdea(codeName))
import Language.Drasil.CodeSpec (CodeSpec(..), CodeSystInfo(..), 
  InputModule(..), Structure(..))

import Data.Map (member)
import qualified Data.Map as Map (filter, lookup, elems)
import Control.Monad.Reader (Reader, ask)

modDesc :: Reader DrasilState [String] -> Reader DrasilState String
modDesc = fmap ((++) "Provides " . stringList)

inputParametersDesc :: Reader DrasilState [String]
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

inputConstructorDesc :: Reader DrasilState String
inputConstructorDesc = do
  g <- ask
  pAndS <- physAndSfwrCons
  let ifDesc False = ""
      ifDesc True = "reading inputs"
      idDesc False = ""
      idDesc True = "calculating derived values"
      icDesc False = ""
      icDesc True = "checking " ++ pAndS ++ " on the input"
      dm = defMap $ codeSpec g
  return $ "Initializes input object by " ++ stringList [ 
    ifDesc (member "get_input" dm),
    idDesc (member "derived_values" dm),
    icDesc (member "input_constraints" dm)]

inputFormatDesc :: Reader DrasilState String
inputFormatDesc = do
  g <- ask
  let ifDesc Nothing = ""
      ifDesc _ = "the function for reading inputs"
  return $ ifDesc $ Map.lookup "get_input" (eMap $ codeSpec g)

derivedValuesDesc :: Reader DrasilState String
derivedValuesDesc = do
  g <- ask
  let dvDesc Nothing = ""
      dvDesc _ = "the function for calculating derived values"
  return $ dvDesc $ Map.lookup "derived_values" (eMap $ codeSpec g)

inputConstraintsDesc :: Reader DrasilState String
inputConstraintsDesc = do
  g <- ask
  pAndS <- physAndSfwrCons
  let icDesc Nothing = ""
      icDesc _ = "the function for checking the " ++ pAndS ++ 
        " on the input"
  return $ icDesc $ Map.lookup "input_constraints" (eMap $ codeSpec g)

constModDesc :: Reader DrasilState String
constModDesc = do
  g <- ask
  let cDesc [] = ""
      cDesc _ = "the structure for holding constant values"
  return $ cDesc $ filter (flip member (eMap $ codeSpec g) . codeName) 
    (constants $ csi $ codeSpec g)

outputFormatDesc :: Reader DrasilState String
outputFormatDesc = do
  g <- ask
  let ofDesc Nothing = ""
      ofDesc _ = "the function for writing outputs"
  return $ ofDesc $ Map.lookup "write_output" (eMap $ codeSpec g)

inputClassDesc :: Reader DrasilState String
inputClassDesc = do
  g <- ask
  let cname = "InputParameters"
      inClassD [] = ""
      inClassD _ = "Structure for holding the " ++ stringList [
        inPs $ extInputs $ csi $ codeSpec g,
        dVs $ Map.lookup "derived_values" (defMap $ codeSpec g),
        cVs $ filter (flip member (Map.filter (cname ==) 
          (eMap $ codeSpec g)) . codeName) (constants $ csi $ codeSpec g)]
      inPs [] = ""
      inPs _ = "input values"
      dVs Nothing = ""
      dVs _ = "derived values"
      cVs [] = ""
      cVs _ = "constant values"
  return $ inClassD $ inputs $ csi $ codeSpec g

constClassDesc :: Reader DrasilState String
constClassDesc = do
  g <- ask
  let ccDesc [] = ""
      ccDesc _ = "Structure for holding the constant values"
  return $ ccDesc $ filter (flip member (eMap $ codeSpec g) . codeName) 
    (constants $ csi $ codeSpec g)

inFmtFuncDesc :: Reader DrasilState String
inFmtFuncDesc = do
  g <- ask
  let ifDesc Nothing = ""
      ifDesc _ = "Reads input from a file with the given file name"
  return $ ifDesc $ Map.lookup "get_input" (defMap $ codeSpec g)

inConsFuncDesc :: Reader DrasilState String
inConsFuncDesc = do
  g <- ask
  pAndS <- physAndSfwrCons
  let icDesc Nothing = ""
      icDesc _ = "Verifies that input values satisfy the " ++ pAndS
  return $ icDesc $ Map.lookup "input_constraints" (defMap $ codeSpec g)

dvFuncDesc :: Reader DrasilState String
dvFuncDesc = do
  g <- ask
  let dvDesc Nothing = ""
      dvDesc _ = "Calculates values that can be immediately derived from the" ++
        " inputs"
  return $ dvDesc $ Map.lookup "derived_values" (defMap $ codeSpec g)

woFuncDesc :: Reader DrasilState String
woFuncDesc = do
  g <- ask
  let woDesc Nothing = ""
      woDesc _ = "Writes the output values to output.txt"
  return $ woDesc $ Map.lookup "write_output" (defMap $ codeSpec g)

physAndSfwrCons :: Reader DrasilState String
physAndSfwrCons = do
  g <- ask
  let cns = concat $ Map.elems (cMap $ csi $ codeSpec g)
  return $ stringList [
    if null (map isPhysC cns) then "" else "physical constraints",
    if null (map isSfwrC cns) then "" else "software constraints"]