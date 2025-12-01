module Language.Drasil.Code.Imperative.Parameters(getInConstructorParams,
  getInputFormatIns, getInputFormatOuts, getDerivedIns, getDerivedOuts,
  getConstraintParams, getCalcParams, getOutputParams
) where

import Control.Lens ((^.))
import Control.Monad.State (get)
import Data.List (nub, (\\), delete)
import Data.Map (member, notMember)
import qualified Data.Map as Map (lookup)

import Drasil.Database (HasUID(..))
import Language.Drasil hiding (isIn, Var)
import Language.Drasil.Chunk.CodeDefinition (CodeDefinition, auxExprs)
import Language.Drasil.Chunk.CodeBase
import Language.Drasil.Choices (Structure(..), ConstantStructure(..),
  ConstantRepr(..), InternalConcept(..))
import Language.Drasil.Code.CodeQuantityDicts (inFileName, inParams, consts)
import Language.Drasil.Code.Imperative.DrasilState (GenState, DrasilState(..),
  genICName)
import Language.Drasil.CodeSpec (HasOldCodeSpec(..), constraintvars, getConstraints)
import Language.Drasil.Mod (Name)

-- | Parameters may be inputs or outputs.
data ParamType = In | Out deriving Eq

-- | Useful to see if a parameter is for 'In'put or output.
isIn :: ParamType -> Bool
isIn = (In ==)

-- | Since the input constructor calls the three input-related methods, the
-- parameters to the constructor are the parameters to the three methods,
-- except excluding any of variables that are state variables in the class,
-- since they are already in scope.
-- If InputParameters is not in the definition list, then the default
-- constructor is used, which takes no parameters.
getInConstructorParams :: GenState [CodeVarChunk]
getInConstructorParams = do
  g <- get
  ifPs <- getInputFormatIns
  dvPs <- getDerivedIns
  icPs <- getConstraintParams
  ipName <- genICName InputParameters
  let getCParams False = []
      getCParams True = ifPs ++ dvPs ++ icPs
  ps <- getParams ipName In $ getCParams (ipName `elem` defSet g)
  return $ filter ((Just ipName /=) . flip Map.lookup (clsMap g) . codeName) ps

-- | The inputs to the function for reading inputs are the input file name.
getInputFormatIns :: GenState [CodeVarChunk]
getInputFormatIns = do
  giName <- genICName GetInput
  getParams giName In [quantvar inFileName]

-- | The outputs from the function for reading inputs are the inputs.
getInputFormatOuts :: GenState [CodeVarChunk]
getInputFormatOuts = do
  g <- get
  giName <- genICName GetInput
  getParams giName Out $ codeSpec g ^. extInputsO

-- | The inputs to the function for calculating derived inputs are any variables
-- used in the equations for the derived inputs.
getDerivedIns :: GenState [CodeVarChunk]
getDerivedIns = do
  g <- get
  let s = codeSpec g
      dvals = s ^. derivedInputsO
      reqdVals = concatMap (flip codevars (s ^. systemdbO) . (^. codeExpr)) dvals
  dvName <- genICName DerivedValuesFn
  getParams dvName In reqdVals

-- | The outputs from the function for calculating derived inputs are the derived inputs.
getDerivedOuts :: GenState [CodeVarChunk]
getDerivedOuts = do
  g <- get
  dvName <- genICName DerivedValuesFn
  getParams dvName Out $ map codeChunk $ codeSpec g ^. derivedInputsO

-- | The parameters to the function for checking constraints on the inputs are
-- any inputs with constraints, and any variables used in the expressions of
-- the constraints.
getConstraintParams :: GenState [CodeVarChunk]
getConstraintParams = do
  g <- get
  let s = codeSpec g
      cm = s ^. cMapO
      db = s ^. systemdbO
      varsList = filter (\i -> member (i ^. uid) cm) (s ^. inputsO)
      reqdVals = nub $ varsList ++ map quantvar (concatMap (`constraintvars` db)
        (getConstraints cm varsList))
  icName <- genICName InputConstraintsFn
  getParams icName In reqdVals

-- | The parameters to a calculation function are any variables used in the
-- expression representing the calculation.
getCalcParams :: CodeDefinition -> GenState [CodeVarChunk]
getCalcParams c = do
  g <- get
  getParams (codeName c) In $ delete (quantvar c) $ concatMap (`codevars'`
    (codeSpec g ^. systemdbO)) (c ^. codeExpr : c ^. auxExprs)

-- | The parameters to the function for printing outputs are the outputs.
getOutputParams :: GenState [CodeVarChunk]
getOutputParams = do
  g <- get
  woName <- genICName WriteOutput
  getParams woName In $ codeSpec g ^. outputsO

-- | Passes parameters that are inputs to 'getInputVars' for further processing.
-- Passes parameters that are constants to 'getConstVars' for further processing.
-- Other parameters are put into the returned parameter list as long as they
-- are not matched to a code concept.
getParams :: (Quantity c, MayHaveUnit c, Concept c) => Name -> ParamType -> [c] ->
  GenState [CodeVarChunk]
getParams n pt cs' = do
  g <- get
  let s = codeSpec g
      cs = map quantvar cs'
      ins = s ^. inputsO
      cnsnts = map quantvar $ s ^. constantsO
      inpVars = filter (`elem` ins) cs
      conVars = filter (`elem` cnsnts) cs
      csSubIns = filter ((`notMember` concMatches g) . (^. uid))
        (cs \\ (ins ++ cnsnts))
  inVs <- getInputVars n pt (inStruct g) Var inpVars
  conVs <- getConstVars n pt (conStruct g) (conRepr g) conVars
  return $ nub $ inVs ++ conVs ++ csSubIns

-- | If the passed list of input variables is empty, then return empty list.
-- If the user has chosen 'Unbundled' inputs, then the input variables are
-- returned as-is.
-- If the user has chosen 'Bundled' inputs, and the parameters are inputs to the
-- function (as opposed to outputs), then the 'inParams' object is returned
-- instead of the individual input variables, unless the function being
-- parameterized is itself defined in the InputParameters class, in which case
-- the inputs are already in scope and thus no parameter is required.
-- If the 'ParamType' is 'Out', the 'inParams' object is not an output parameter
-- because it undergoes state transitions, so is not actually an output.
-- The final case only happens when getInputVars is called by 'getConstVars'
-- because the user has chosen 'WithInputs' as their constant structure. If they
-- have chosen 'Bundled' inputs and a constant const representation, then the
-- constant variables are static and can be accessed through the class, without
-- an object, so no parameters are required.
getInputVars :: Name -> ParamType -> Structure -> ConstantRepr ->
  [CodeVarChunk] -> GenState [CodeVarChunk]
getInputVars _ _ _ _ [] = return []
getInputVars _ _ Unbundled _ cs = return cs
getInputVars n pt Bundled Var _ = do
  g <- get
  cname <- genICName InputParameters
  return [quantvar inParams | Map.lookup n (clsMap g) /= Just cname && isIn pt]
getInputVars _ _ Bundled Const _ = return []

-- | If the passed list of constant variables is empty, then return empty list.
-- If the user has chosen 'Unbundled' constants, then the constant variables are
-- returned as-is.
-- If the user has chosen 'Bundled' constants and 'Var' representation, and the
-- parameters are inputs to the function (as opposed to outputs), then the
-- 'consts' object is returned instead of the individual constant variables.
-- If the 'ParamType' is 'Out', the 'consts' object is not an output parameter
-- because it undergoes state transitions, so is not actually an output.
-- The final case only happens when 'getInputVars' is called by 'getConstVars'
-- because the user has chosen 'WithInputs' as their constant structure. If they
-- have chosen 'Bundled' inputs and a constant const representation, then the
-- constant variables are static and can be accessed through the class, without
-- an object, so no parameters are required.
getConstVars :: Name -> ParamType -> ConstantStructure -> ConstantRepr ->
  [CodeVarChunk] -> GenState [CodeVarChunk]
getConstVars _ _ _ _ [] = return []
getConstVars _ _ (Store Unbundled) _ cs = return cs
getConstVars _ pt (Store Bundled) Var _ = return [quantvar consts | isIn pt]
getConstVars _ _ (Store Bundled) Const _ = return []
getConstVars n pt WithInputs cr cs = do
  g <- get
  getInputVars n pt (inStruct g) cr cs
getConstVars _ _ Inline _ _ = return []
