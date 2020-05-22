-- | Defines description generators for common SCS functions, classes, and 
-- modules
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
import Language.Drasil.Choices (ImplementationType(..), InputModule(..), 
  Structure(..))
import Language.Drasil.CodeSpec (CodeSpec(..))
import Language.Drasil.Mod (Description)

import Data.Map (member)
import qualified Data.Map as Map (filter, lookup, null)
import Data.Maybe (mapMaybe)
import Control.Lens ((^.))
import Control.Monad.Reader (Reader, ask)

-- | Returns a module description based on a list of descriptions of what is
-- contained in the module.
modDesc :: Reader DrasilState [Description] -> Reader DrasilState Description
modDesc = fmap ((++) "Provides " . stringList)

-- | Returns description of what is contained in the module that is generated
-- when the user chooses an Unmodular design. Module is described as either
-- program or library, depending on the user's choice of implementation type.
unmodularDesc :: Reader DrasilState Description
unmodularDesc = do
  g <- ask
  let n = pName $ codeSpec g
      getDesc Library = "library"
      getDesc Program = "program"
  return $ "Contains the entire " ++ n ++ " " ++ getDesc (implType g)

-- | Returns description of what is contained in the Input Parameters module.
-- If user chose Bundled, this module includes the structure for holding the 
-- input values, but not if they chose Unbundled.
-- If the user chose Combined, this module includes the input-related functions,
-- but not if they chose Separated.
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

-- | Returns description of the input constructor, checking whether each 
-- possible method that may be called by the constructor is defined, and 
-- including it in the description if so
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
      dl = defList g
  return $ "Initializes input object by " ++ stringList [ 
    ifDesc ("get_input" `elem` dl),
    idDesc ("derived_values" `elem` dl),
    icDesc ("input_constraints" `elem` dl)]

-- | Returns description of what is contained in the Input Format module,
-- if it exists.
inputFormatDesc :: Reader DrasilState Description
inputFormatDesc = do
  g <- ask
  let ifDesc False = ""
      ifDesc _ = "the function for reading inputs"
  return $ ifDesc $ "get_input" `elem` defList g

-- | Returns description of what is contained in the Derived Values module,
-- if it exists.
derivedValuesDesc :: Reader DrasilState Description
derivedValuesDesc = do
  g <- ask
  let dvDesc False = ""
      dvDesc _ = "the function for calculating derived values"
  return $ dvDesc $ "derived_values" `elem` defList g

-- | Returns description of what is contained in the Input Constraints module,
-- if it exists.
inputConstraintsDesc :: Reader DrasilState Description
inputConstraintsDesc = do
  g <- ask
  pAndS <- physAndSfwrCons
  let icDesc False = ""
      icDesc _ = "the function for checking the " ++ pAndS ++ 
        " on the input"
  return $ icDesc $ "input_constraints" `elem` defList g

-- | Returns description of what is contained in the Constants module,
-- if it exists.
constModDesc :: Reader DrasilState Description
constModDesc = do
  g <- ask
  let cname = "Constants"
      cDesc [] = ""
      cDesc _ = "the structure for holding constant values"
  return $ cDesc $ filter (flip member (Map.filter (cname ==) 
    (clsMap g)) . codeName) (constants $ codeSpec g)

-- | Returns description of what is contained in the Output Format module, 
-- if it exists.
outputFormatDesc :: Reader DrasilState Description
outputFormatDesc = do
  g <- ask
  let ofDesc False = ""
      ofDesc _ = "the function for writing outputs"
  return $ ofDesc $ "write_output" `elem` defList g

-- | Returns description for generated function that stores inputs,
-- if it exists. Checks whether explicit inputs, derived inputs, and constants 
-- are defined in the InputParameters class and includes each in the 
-- description if so.
inputClassDesc :: Reader DrasilState Description
inputClassDesc = do
  g <- ask
  let cname = "InputParameters"
      ipMap = Map.filter (cname ==) (clsMap g)
      inIPMap = filter ((`member` ipMap) . codeName)
      inClassD True = ""
      inClassD _ = "Structure for holding the " ++ stringList [
        inPs $ inIPMap $ extInputs $ codeSpec g,
        dVs $ inIPMap $ map quantvar $ derivedInputs $ codeSpec g,
        cVs $ inIPMap $ map quantvar $ constants $ codeSpec g]
      inPs [] = ""
      inPs _ = "input values"
      dVs [] = ""
      dVs _ = "derived values"
      cVs [] = ""
      cVs _ = "constant values"
  return $ inClassD $ Map.null ipMap

-- | Returns description for generated class that stores constants,
-- if it exists. If no constants are defined in the Constants class, then it 
-- does not exist and an empty description is returned.
constClassDesc :: Reader DrasilState Description
constClassDesc = do
  g <- ask
  let cname = "Constants"
      ccDesc [] = ""
      ccDesc _ = "Structure for holding the constant values"
  return $ ccDesc $ filter (flip member (Map.filter (cname ==) 
    (clsMap g)) . codeName) (constants $ codeSpec g)

-- | Returns description for generated function that reads input from a file,
-- if it exists.
inFmtFuncDesc :: Reader DrasilState Description
inFmtFuncDesc = do
  g <- ask
  let ifDesc False = ""
      ifDesc _ = "Reads input from a file with the given file name"
  return $ ifDesc $ "get_input" `elem` defList g

-- | Returns description for generated function that checks input constraints,
-- if it exists.
inConsFuncDesc :: Reader DrasilState Description
inConsFuncDesc = do
  g <- ask
  pAndS <- physAndSfwrCons
  let icDesc False = ""
      icDesc _ = "Verifies that input values satisfy the " ++ pAndS
  return $ icDesc $ "input_constraints" `elem` defList g

-- | Returns description for generated function that calculates derived inputs,
-- if it exists.
dvFuncDesc :: Reader DrasilState Description
dvFuncDesc = do
  g <- ask
  let dvDesc False = ""
      dvDesc _ = "Calculates values that can be immediately derived from the" ++
        " inputs"
  return $ dvDesc $ "derived_values" `elem` defList g

-- | Description of the generated Calculations module
calcModDesc :: Description
calcModDesc = "Provides functions for calculating the outputs"

-- | Returns description for generated output-printing function, if it exists
woFuncDesc :: Reader DrasilState Description
woFuncDesc = do
  g <- ask
  let woDesc False = ""
      woDesc _ = "Writes the output values to output.txt"
  return $ woDesc $ "write_output" `elem` defList g

-- | Returns the phrase "physical constraints" if there are any physical 
-- constraints on the input and "software constraints" if there are any 
-- software constraints on the input. If there are both, 
-- "physical constraints and software constraints" is returned.
physAndSfwrCons :: Reader DrasilState Description
physAndSfwrCons = do
  g <- ask
  let cns = concat $ mapMaybe ((`Map.lookup` (cMap $ codeSpec g)) . (^. uid)) 
        (inputs $ codeSpec g)
  return $ stringList [
    if not (any isPhysC cns) then "" else "physical constraints",
    if not (any isSfwrC cns) then "" else "software constraints"]