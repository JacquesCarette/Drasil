module Language.Drasil.Code.Imperative.Descriptions (
  modDesc, unmodularDesc, inputParametersDesc, inputConstructorDesc, 
  inputFormatDesc, derivedValuesDesc, inputConstraintsDesc, constModDesc, 
  outputFormatDesc, inputClassDesc, constClassDesc, inFmtFuncDesc, 
  inConsFuncDesc, dvFuncDesc, calcModDesc, woFuncDesc
) where

import Utils.Drasil (stringList)

import Language.Drasil
import Language.Drasil.Code.Imperative.DrasilState (DrasilState(..), inMod)
import Language.Drasil.Chunk.Code (CodeIdea(codeName), quantvar)
import Language.Drasil.CodeSpec (CodeSpec(..), CodeSystInfo(..), 
  ImplementationType(..), InputModule(..), Structure(..))
import Language.Drasil.Mod (Description)

import Data.Map (member)
import qualified Data.Map as Map (filter, lookup, null)
import Data.Maybe (mapMaybe)
import Control.Lens ((^.))
import Control.Monad.Reader (Reader, ask)

modDesc :: Reader DrasilState [Description] -> Reader DrasilState Description
modDesc = fmap ((++) "Provides " . stringList)

unmodularDesc :: Reader DrasilState Description
unmodularDesc = do
  g <- ask
  let n = pName $ csi $ codeSpec g
      getDesc Library = "library"
      getDesc Program = "program"
  return $ "Contains the entire " ++ n ++ " " ++ getDesc (implType g)

inputParametersDesc :: Reader DrasilState [Description]
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

inputConstructorDesc :: Reader DrasilState Description
inputConstructorDesc = do
  g <- ask
  pAndS <- physAndSfwrCons
  let ifDesc False = ""
      ifDesc True = "reading inputs"
      idDesc False = ""
      idDesc True = "calculating derived values"
      icDesc False = ""
      icDesc True = "checking " ++ pAndS ++ " on the input"
      dl = defList $ codeSpec g
  return $ "Initializes input object by " ++ stringList [ 
    ifDesc ("get_input" `elem` dl),
    idDesc ("derived_values" `elem` dl),
    icDesc ("input_constraints" `elem` dl)]

inputFormatDesc :: Reader DrasilState Description
inputFormatDesc = do
  g <- ask
  let ifDesc False = ""
      ifDesc _ = "the function for reading inputs"
  return $ ifDesc $ "get_input" `elem` defList (codeSpec g)

derivedValuesDesc :: Reader DrasilState Description
derivedValuesDesc = do
  g <- ask
  let dvDesc False = ""
      dvDesc _ = "the function for calculating derived values"
  return $ dvDesc $ "derived_values" `elem` defList (codeSpec g)

inputConstraintsDesc :: Reader DrasilState Description
inputConstraintsDesc = do
  g <- ask
  pAndS <- physAndSfwrCons
  let icDesc False = ""
      icDesc _ = "the function for checking the " ++ pAndS ++ 
        " on the input"
  return $ icDesc $ "input_constraints" `elem` defList (codeSpec g)

constModDesc :: Reader DrasilState Description
constModDesc = do
  g <- ask
  let cname = "Constants"
      cDesc [] = ""
      cDesc _ = "the structure for holding constant values"
  return $ cDesc $ filter (flip member (Map.filter (cname ==) 
    (clsMap $ codeSpec g)) . codeName) (constants $ csi $ codeSpec g)

outputFormatDesc :: Reader DrasilState Description
outputFormatDesc = do
  g <- ask
  let ofDesc False = ""
      ofDesc _ = "the function for writing outputs"
  return $ ofDesc $ "write_output" `elem` defList (codeSpec g)

inputClassDesc :: Reader DrasilState Description
inputClassDesc = do
  g <- ask
  let cname = "InputParameters"
      ipMap = Map.filter (cname ==) (clsMap $ codeSpec g)
      inIPMap = filter ((`member` ipMap) . codeName)
      inClassD True = ""
      inClassD _ = "Structure for holding the " ++ stringList [
        inPs $ inIPMap $ extInputs $ csi $ codeSpec g,
        dVs $ inIPMap $ map quantvar $ derivedInputs $ csi $ codeSpec g,
        cVs $ inIPMap $ map quantvar $ constants $ csi $ codeSpec g]
      inPs [] = ""
      inPs _ = "input values"
      dVs [] = ""
      dVs _ = "derived values"
      cVs [] = ""
      cVs _ = "constant values"
  return $ inClassD $ Map.null ipMap

constClassDesc :: Reader DrasilState Description
constClassDesc = do
  g <- ask
  let cname = "Constants"
      ccDesc [] = ""
      ccDesc _ = "Structure for holding the constant values"
  return $ ccDesc $ filter (flip member (Map.filter (cname ==) 
    (clsMap $ codeSpec g)) . codeName) (constants $ csi $ codeSpec g)

inFmtFuncDesc :: Reader DrasilState Description
inFmtFuncDesc = do
  g <- ask
  let ifDesc False = ""
      ifDesc _ = "Reads input from a file with the given file name"
  return $ ifDesc $ "get_input" `elem` defList (codeSpec g)

inConsFuncDesc :: Reader DrasilState Description
inConsFuncDesc = do
  g <- ask
  pAndS <- physAndSfwrCons
  let icDesc False = ""
      icDesc _ = "Verifies that input values satisfy the " ++ pAndS
  return $ icDesc $ "input_constraints" `elem` defList (codeSpec g)

dvFuncDesc :: Reader DrasilState Description
dvFuncDesc = do
  g <- ask
  let dvDesc False = ""
      dvDesc _ = "Calculates values that can be immediately derived from the" ++
        " inputs"
  return $ dvDesc $ "derived_values" `elem` defList (codeSpec g)

calcModDesc :: Description
calcModDesc = "Provides functions for calculating the outputs"

woFuncDesc :: Reader DrasilState Description
woFuncDesc = do
  g <- ask
  let woDesc False = ""
      woDesc _ = "Writes the output values to output.txt"
  return $ woDesc $ "write_output" `elem` defList (codeSpec g)

physAndSfwrCons :: Reader DrasilState Description
physAndSfwrCons = do
  g <- ask
  let cns = concat $ mapMaybe ((`Map.lookup` (cMap $ csi $ codeSpec g)) . 
        (^. uid)) (inputs $ csi $ codeSpec g)
  return $ stringList [
    if not (any isPhysC cns) then "" else "physical constraints",
    if not (any isSfwrC cns) then "" else "software constraints"]