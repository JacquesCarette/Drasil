-- | Defines description generators for common SCS functions, classes, and
-- modules.
module Language.Drasil.Code.Imperative.Descriptions (
  modDesc, unmodularDesc, inputParametersDesc, inputConstructorDesc,
  inputFormatDesc, derivedValuesDesc, inputConstraintsDesc, constModDesc,
  outputFormatDesc, inputClassDesc, constClassDesc, inFmtFuncDesc,
  inConsFuncDesc, dvFuncDesc, calcModDesc, woFuncDesc
) where

import Utils.Drasil (stringList)

import Language.Drasil
import Language.Drasil.Chunk.CodeBase
import Language.Drasil.Code.Imperative.DrasilState (GenState, DrasilState(..), 
  genICName)
import Language.Drasil.Choices (ImplementationType(..), Structure(..), 
  InternalConcept(..))
import Language.Drasil.CodeSpec (HasOldCodeSpec(..))
import Language.Drasil.Mod (Description)
import Language.Drasil.Printers (SingleLine(OneLine), sentenceDoc, PrintingInformation)

import Data.Map (member)
import qualified Data.Map as Map (filter, lookup, null)
import Data.Maybe (mapMaybe)
import Control.Lens ((^.))
import Control.Monad.State (get)
import System.Drasil hiding (systemdb)

-- | Returns a module description based on a list of descriptions of what is
-- contained in the module.
modDesc :: GenState [Description] -> GenState Description
modDesc = fmap ((++) "Provides " . stringList)

-- | Returns description of what is contained in the module that is generated
-- when the user chooses an Unmodular design. Module is described as either a
-- program or library, depending on the user's choice of implementation type.
unmodularDesc :: PrintingInformation -> GenState Description
unmodularDesc sm = do
  g <- get
  let spec = codeSpec g
      implTypeStr Program = "program"
      implTypeStr Library = "library"
  return $ show $ sentenceDoc sm (spec ^. systemdbO) Implementation OneLine $ capSent $
    foldlSent ([S "a", S (implTypeStr (implType g)), S "to"] ++ codeSpec g ^. purpose)

-- | Returns description of what is contained in the Input Parameters module.
-- If user chooses the 'Bundled' input parameter, this module will include the structure for holding the
-- input values. Does not include the structure if they choose 'Unbundled'.
-- This module includes the input-related functions.
inputParametersDesc :: GenState [Description]
inputParametersDesc = do
  g <- get
  ifDesc <- inputFormatDesc
  dvDesc <- derivedValuesDesc
  icDesc <- inputConstraintsDesc
  let st = inStruct g
      ipDesc = inDesc st ++ [ifDesc, dvDesc, icDesc]
      inDesc Bundled = ["the structure for holding input values"]
      inDesc Unbundled = [""]
  return ipDesc

-- | Returns a description of the input constructor, checking whether each
-- possible method that may be called by the constructor is defined, and
-- including it in the description if so.
inputConstructorDesc :: GenState Description
inputConstructorDesc = do
  g <- get
  pAndS <- physAndSfwrCons
  giName <- genICName GetInput
  dvName <- genICName DerivedValuesFn
  icName <- genICName InputConstraintsFn
  let ifDesc False = ""
      ifDesc True = "reading inputs"
      idDesc False = ""
      idDesc True = "calculating derived values"
      icDesc False = ""
      icDesc True = "checking " ++ pAndS ++ " on the input"
      ds = defSet g
  return $ "Initializes input object by " ++ stringList [
    ifDesc (giName `elem` ds),
    idDesc (dvName `elem` ds),
    icDesc (icName `elem` ds)]

-- | Returns a description of what is contained in the Input Format module,
-- if it exists.
inputFormatDesc :: GenState Description
inputFormatDesc = do
  g <- get
  giName <- genICName GetInput
  let ifDesc False = ""
      ifDesc _ = "the function for reading inputs"
  return $ ifDesc $ giName `elem` defSet g

-- | Returns a description of what is contained in the Derived Values module,
-- if it exists.
derivedValuesDesc :: GenState Description
derivedValuesDesc = do
  g <- get
  dvName <- genICName DerivedValuesFn
  let dvDesc False = ""
      dvDesc _ = "the function for calculating derived values"
  return $ dvDesc $ dvName `elem` defSet g

-- | Returns a description of what is contained in the Input Constraints
-- module, if it exists.
inputConstraintsDesc :: GenState Description
inputConstraintsDesc = do
  g <- get
  icName <- genICName InputConstraintsFn
  pAndS <- physAndSfwrCons
  let icDesc False = ""
      icDesc _ = "the function for checking the " ++ pAndS ++
        " on the input"
  return $ icDesc $ icName `elem` defSet g

-- | Returns a description of what is contained in the Constants module,
-- if it exists.
constModDesc :: GenState Description
constModDesc = do
  g <- get
  cname <- genICName Constants
  let cDesc [] = ""
      cDesc _ = "the structure for holding constant values"
  return $ cDesc $ filter (flip member (Map.filter (cname ==)
    (clsMap g)) . codeName) (codeSpec g ^. constantsO)

-- | Returns a description of what is contained in the Output Format module,
-- if it exists.
outputFormatDesc :: GenState Description
outputFormatDesc = do
  g <- get
  woName <- genICName WriteOutput
  let ofDesc False = ""
      ofDesc _ = "the function for writing outputs"
  return $ ofDesc $ woName `elem` defSet g

-- | Returns a description for the generated function that stores inputs,
-- if it exists. Checks whether explicit inputs, derived inputs, and constants
-- are defined in the InputParameters class and includes each in the
-- description if so.
inputClassDesc :: GenState Description
inputClassDesc = do
  g <- get
  cname <- genICName InputParameters
  let ipMap = Map.filter (cname ==) (clsMap g)
      inIPMap = filter ((`member` ipMap) . codeName)
      inClassD True = ""
      inClassD _ = "Structure for holding the " ++ stringList [
        inPs $ inIPMap $ codeSpec g ^. extInputsO,
        dVs $ inIPMap $ map quantvar $ codeSpec g ^. derivedInputsO,
        cVs $ inIPMap $ map quantvar $ codeSpec g ^. constantsO]
      inPs [] = ""
      inPs _ = "input values"
      dVs [] = ""
      dVs _ = "derived values"
      cVs [] = ""
      cVs _ = "constant values"
  return $ inClassD $ Map.null ipMap

-- | Returns a description for the generated class that stores constants,
-- if it exists. If no constants are defined in the Constants class, then it
-- does not exist and an empty description is returned.
constClassDesc :: GenState Description
constClassDesc = do
  g <- get
  cname <- genICName Constants
  let ccDesc [] = ""
      ccDesc _ = "Structure for holding the constant values"
  return $ ccDesc $ filter (flip member (Map.filter (cname ==)
    (clsMap g)) . codeName) (codeSpec g ^. constantsO)

-- | Returns a description for the generated function that reads input from a
-- file, if it exists.
inFmtFuncDesc :: GenState Description
inFmtFuncDesc = do
  g <- get
  giName <- genICName GetInput
  let ifDesc False = ""
      ifDesc _ = "Reads input from a file with the given file name"
  return $ ifDesc $ giName `elem` defSet g

-- | Returns a description for the generated function that checks input
-- constraints, if it exists.
inConsFuncDesc :: GenState Description
inConsFuncDesc = do
  g <- get
  icName <- genICName InputConstraintsFn
  pAndS <- physAndSfwrCons
  let icDesc False = ""
      icDesc _ = "Verifies that input values satisfy the " ++ pAndS
  return $ icDesc $ icName `elem` defSet g

-- | Returns a description for the generated function that calculates derived
-- inputs, if it exists.
dvFuncDesc :: GenState Description
dvFuncDesc = do
  g <- get
  dvName <- genICName DerivedValuesFn
  let dvDesc False = ""
      dvDesc _ = "Calculates values that can be immediately derived from the"
        ++ " inputs"
  return $ dvDesc $ dvName `elem` defSet g

-- | Description of the generated Calculations module.
calcModDesc :: Description
calcModDesc = "Provides functions for calculating the outputs"

-- | Returns description for generated output-printing function, if it exists.
woFuncDesc :: GenState Description
woFuncDesc = do
  g <- get
  woName <- genICName WriteOutput
  let woDesc False = ""
      woDesc _ = "Writes the output values to output.txt"
  return $ woDesc $ woName `elem` defSet g

-- | Returns the phrase "physical constraints" if there are any physical
-- constraints on the input and "software constraints" if there are any
-- software constraints on the input. If there are both,
-- "physical constraints and software constraints" is returned.
physAndSfwrCons :: GenState Description
physAndSfwrCons = do
  g <- get
  let cns = concat $ mapMaybe ((`Map.lookup` (codeSpec g ^. cMapO)) . (^. uid))
        (codeSpec g ^. inputsO)
  return $ stringList [
    if not (any isPhysC cns) then "" else "physical constraints",
    if not (any isSfwrC cns) then "" else "software constraints"]
