module Language.Drasil.Code.Imperative.FunctionCalls (
  genAllInputCalls, genAllInputCallsProc, genInputCall, genInputCallProc,
  genDerivedCall, genDerivedCallProc, genConstraintCall, genConstraintCallProc,
  genCalcCall, genCalcCallProc, genOutputCall, genOutputCallProc
) where

import Data.List ((\\), intersect)
import qualified Data.Map as Map (lookup)
import Data.Maybe (catMaybes)
import Control.Applicative ((<|>))
import Control.Monad.State (get)

import Drasil.Code.CodeVar (CodeIdea(codeName), CodeVarChunk, quantvar)
import Language.Drasil.Code.Imperative.GenerateGOOL (fApp, fAppProc, fAppInOut,
  fAppInOutProc)
import Language.Drasil.Code.Imperative.Helpers (convScope)
import Language.Drasil.Code.Imperative.Import (codeType, mkVal, mkValProc,
  mkVar, mkVarProc)
import Language.Drasil.Code.Imperative.Parameters (getCalcParams,
  getConstraintParams, getDerivedIns, getDerivedOuts, getInputFormatIns,
  getInputFormatOuts, getOutputParams)
import Language.Drasil.Code.Imperative.DrasilState (GenState, DrasilState(..),
  genICName)
import Language.Drasil.Chunk.CodeDefinition (CodeDefinition)
import Language.Drasil.Mod (Name)
import Language.Drasil.Choices (InternalConcept(..))

import Drasil.GOOL (SValue, OOStatement, MS, VS, TypeSym(..), VariableValue(..),
  DeclStatement(..), ValueStatement(..), convType, convTypeOO, TypeData,
  FuncAppStatement, TypeElim, VariableElim, Argument, Set, ValueExpression,
  Comparison, BooleanExpression, MathConstant, List)
import Drasil.GProc (NativeVector, Reference, NumericExpression)

-- | Generates calls to all of the input-related functions. First is the call to
-- the function for reading inputs, then the function for calculating derived
-- inputs, then the function for checking input constraints.
genAllInputCalls
  :: (OOStatement r stmt, ValueStatement r stmt, TypeElim r, VariableElim r)
  => GenState [MS (r stmt)]
genAllInputCalls = do
  gi <- genInputCall
  dv <- genDerivedCall
  ic <- genConstraintCall
  return $ catMaybes [gi, dv, ic]

-- | Generates a call to the function for reading inputs from a file.
genInputCall
  :: (OOStatement r stmt, VariableElim r)
  => GenState (Maybe (MS (r stmt)))
genInputCall = do
  giName <- genICName GetInput
  genInOutCall giName getInputFormatIns getInputFormatOuts

-- | Generates a call to the function for calculating derived inputs.
genDerivedCall
  :: (OOStatement r stmt, VariableElim r)
  => GenState (Maybe (MS (r stmt)))
genDerivedCall = do
  dvName <- genICName DerivedValuesFn
  genInOutCall dvName getDerivedIns getDerivedOuts

-- | Generates a call to the function for checking constraints on the input.
genConstraintCall
  :: (OOStatement r stmt, ValueStatement r stmt, TypeElim r, VariableElim r)
  => GenState (Maybe (MS (r stmt)))
genConstraintCall = do
  icName <- genICName InputConstraintsFn
  val <- genFuncCall icName void getConstraintParams
  return $ fmap valStmt val

-- | Generates a call to a calculation function, given the 'CodeDefinition' for the
-- value being calculated.
genCalcCall
  :: (OOStatement r stmt, TypeElim r, VariableElim r)
  => CodeDefinition -> GenState (Maybe (MS (r stmt)))
genCalcCall c = do
  g <- get
  let scp = convScope $ currentScope g
  t <- codeType c
  val <- genFuncCall (codeName c) (convTypeOO t) (getCalcParams c)
  v <- mkVar (quantvar c)
  return $ fmap (varDecDef v scp) val

-- | Generates a call to the function for printing outputs.
genOutputCall
  :: (OOStatement r stmt, ValueStatement r stmt, TypeElim r, VariableElim r)
  => GenState (Maybe (MS (r stmt)))
genOutputCall = do
  woName <- genICName WriteOutput
  val <- genFuncCall woName void getOutputParams
  return $ fmap valStmt val

-- | Generates a function call given the name, return type, and arguments to
-- the function.
genFuncCall
  :: (OOStatement r stmt, TypeElim r, VariableElim r)
  => Name
  -> VS (r TypeData)
  -> GenState [CodeVarChunk]
  -> GenState (Maybe (SValue r))
genFuncCall n t funcPs = do
  mm <- genCall n
  let genFuncCall' Nothing = return Nothing
      genFuncCall' (Just m) = do
        cs <- funcPs
        pvals <- mapM mkVal cs
        val <- fApp m n t pvals []
        return $ Just val
  genFuncCall' mm

-- | Generates a function call given the name, inputs, and outputs for the
-- function.
genInOutCall
  :: (OOStatement r stmt, VariableElim r)
  => Name
  -> GenState [CodeVarChunk]
  -> GenState [CodeVarChunk]
  -> GenState (Maybe (MS (r stmt)))
genInOutCall n inFunc outFunc = do
  mm <- genCall n
  let genInOutCall' Nothing = return Nothing
      genInOutCall' (Just m) = do
        ins' <- inFunc
        outs' <- outFunc
        ins <- mapM mkVar (ins' \\ outs')
        outs <- mapM mkVar (outs' \\ ins')
        both <- mapM mkVar (ins' `intersect` outs')
        stmt <- fAppInOut m n (map valueOf ins) outs both
        return $ Just stmt
  genInOutCall' mm

-- | Gets the name of the module containing the function being called.
-- If the function is not in either the module export map or class definition map,
--   return 'Nothing'.
-- If the function is not in module export map but is in the class definition map,
-- that means it is a private function, so return 'Nothing' unless it is in the
-- current class.
genCall :: Name -> GenState (Maybe Name)
genCall n = do
  g <- get
  let currc = currentClass g
      genCallExported Nothing = genCallInClass (Map.lookup n $ clsMap g)
      genCallExported m = return m
      genCallInClass Nothing = return Nothing
      genCallInClass (Just c) = if c == currc then return $ Map.lookup c (eMap
        g) <|> error (c ++ " class missing from export map")
        else return Nothing
  genCallExported $ Map.lookup n (eMap g)

-- Procedural Versions --

-- | Generates calls to all of the input-related functions. First is the call to
-- the function for reading inputs, then the function for calculating derived
-- inputs, then the function for checking input constraints.
genAllInputCallsProc
  ::
    ( MathConstant r
    , VariableValue r
    , BooleanExpression r
    , Comparison r
    , NumericExpression r
    , ValueExpression r
    , NativeVector r
    , FuncAppStatement r stmt
    , Argument r
    , List r stmt
    , Reference r
    , Set r
    , ValueStatement r stmt
    , TypeElim r
    )
  => GenState [MS (r stmt)]
genAllInputCallsProc = do
  gi <- genInputCallProc
  dv <- genDerivedCallProc
  ic <- genConstraintCallProc
  return $ catMaybes [gi, dv, ic]

-- | Generates a call to the function for reading inputs from a file.
genInputCallProc
  :: (FuncAppStatement r stmt, VariableValue r)
  => GenState (Maybe (MS (r stmt)))
genInputCallProc = do
  giName <- genICName GetInput
  genInOutCallProc giName getInputFormatIns getInputFormatOuts

-- | Generates a call to the function for calculating derived inputs.
genDerivedCallProc
  :: (FuncAppStatement r stmt, VariableValue r) => GenState (Maybe (MS (r stmt)))
genDerivedCallProc = do
  dvName <- genICName DerivedValuesFn
  genInOutCallProc dvName getDerivedIns getDerivedOuts

-- | Generates a call to the function for checking constraints on the input.
genConstraintCallProc
  ::
    ( MathConstant r
    , VariableValue r
    , BooleanExpression r
    , Comparison r
    , NumericExpression r
    , ValueExpression r
    , NativeVector r
    , Argument r
    , List r stmt
    , Reference r
    , Set r
    , ValueStatement r stmt
    , TypeElim r
    )
  => GenState (Maybe (MS (r stmt)))
genConstraintCallProc = do
  icName <- genICName InputConstraintsFn
  val <- genFuncCallProc icName void getConstraintParams
  return $ fmap valStmt val

-- | Generates a call to a calculation function, given the 'CodeDefinition' for the
-- value being calculated.
genCalcCallProc
  ::
    ( MathConstant r
    , VariableValue r
    , BooleanExpression r
    , Comparison r
    , NumericExpression r
    , ValueExpression r
    , DeclStatement r stmt
    , Argument r
    , List r stmt
    , NativeVector r
    , Reference r
    , Set r
    , TypeElim r
    )
  => CodeDefinition -> GenState (Maybe (MS (r stmt)))
genCalcCallProc c = do
  g <- get
  let scp = convScope $ currentScope g
  t <- codeType c
  val <- genFuncCallProc (codeName c) (convType t) (getCalcParams c)
  v <- mkVarProc (quantvar c)
  return $ fmap ((`varDecDef` scp) v) val

-- | Generates a call to the function for printing outputs.
genOutputCallProc
  ::
    ( MathConstant r
    , VariableValue r
    , BooleanExpression r
    , Comparison r
    , NumericExpression r
    , ValueExpression r
    , Argument r
    , List r stmt
    , NativeVector r
    , Reference r
    , Set r
    , ValueStatement r stmt
    , TypeElim r
    )
  => GenState (Maybe (MS (r stmt)))
genOutputCallProc = do
  woName <- genICName WriteOutput
  val <- genFuncCallProc woName void getOutputParams
  return $ fmap valStmt val

-- | Generates a function call given the name, return type, and arguments to
-- the function.
genFuncCallProc
  ::
    ( MathConstant r
    , VariableValue r
    , BooleanExpression r
    , Comparison r
    , NumericExpression r
    , ValueExpression r
    , Argument r
    , List r stmt
    , NativeVector r
    , Reference r
    , Set r
    , TypeElim r
    )
  => Name
  -> VS (r TypeData)
  -> GenState [CodeVarChunk]
  -> GenState (Maybe (SValue r))
genFuncCallProc n t funcPs = do
  mm <- genCall n
  let genFuncCall' Nothing = return Nothing
      genFuncCall' (Just m) = do
        cs <- funcPs
        pvals <- mapM mkValProc cs
        val <- fAppProc m n t pvals []
        return $ Just val
  genFuncCall' mm

-- | Generates a function call given the name, inputs, and outputs for the
-- function.
genInOutCallProc
  :: (FuncAppStatement r stmt, VariableValue r)
  => Name
  -> GenState [CodeVarChunk]
  -> GenState [CodeVarChunk]
  -> GenState (Maybe (MS (r stmt)))
genInOutCallProc n inFunc outFunc = do
  mm <- genCall n
  let genInOutCall' Nothing = return Nothing
      genInOutCall' (Just m) = do
        ins' <- inFunc
        outs' <- outFunc
        ins <- mapM mkVarProc (ins' \\ outs')
        outs <- mapM mkVarProc (outs' \\ ins')
        both <- mapM mkVarProc (ins' `intersect` outs')
        stmt <- fAppInOutProc m n (map valueOf ins) outs both
        return $ Just stmt
  genInOutCall' mm
