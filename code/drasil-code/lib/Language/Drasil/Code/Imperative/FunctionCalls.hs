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

import Drasil.GOOL (SValue, MS, VS, TypeSym(..), OOTypeSym, ScopeSym, ValueSym,
  OOVariableSym, VariableValue(..), ValueStatement(valStmt), DeclStatement(..),
  convType, convTypeOO, FuncAppStatement, TypeElim, VariableSym,
  VariableElim, Argument, Set, ValueExpression, Comparison, BooleanExpression,
  MathConstant, List, SelfSym, OOFuncAppStatement, InternalValueExp, Literal,
  OOValueExpression)
import Drasil.GProc (NativeVector, Reference, NumericExpression)

-- | Generates calls to all of the input-related functions. First is the call to
-- the function for reading inputs, then the function for calculating derived
-- inputs, then the function for checking input constraints.
genAllInputCalls
  ::
    ( ValueSym r typ
    , Argument r
    , Literal r typ
    , MathConstant r
    , TypeSym r typ
    , OOTypeSym r typ
    , VariableSym r typ
    , OOVariableSym r typ
    , VariableValue r
    , BooleanExpression r
    , Comparison r
    , NumericExpression r
    , ValueExpression r typ
    , SelfSym r
    , InternalValueExp r typ
    , OOValueExpression r typ
    , List r
    , Reference r
    , Set r
    , ValueStatement r stmt
    , FuncAppStatement r stmt
    , OOFuncAppStatement r stmt
    , TypeElim r typ
    , VariableElim r typ
    )
  => GenState [MS (r stmt)]
genAllInputCalls = do
  gi <- genInputCall
  dv <- genDerivedCall
  ic <- genConstraintCall
  pure $ catMaybes [gi, dv, ic]

-- | Generates a call to the function for reading inputs from a file.
genInputCall
  ::
    ( TypeSym r typ
    , OOTypeSym r typ
    , VariableSym r typ
    , OOVariableSym r typ
    , VariableValue r
    , SelfSym r
    , FuncAppStatement r stmt
    , OOFuncAppStatement r stmt
    , VariableElim r typ
    )
  => GenState (Maybe (MS (r stmt)))
genInputCall = do
  giName <- genICName GetInput
  genInOutCall giName getInputFormatIns getInputFormatOuts

-- | Generates a call to the function for calculating derived inputs.
genDerivedCall
  ::
    ( TypeSym r typ
    , OOTypeSym r typ
    , VariableSym r typ
    , OOVariableSym r typ
    , VariableValue r
    , SelfSym r
    , FuncAppStatement r stmt
    , OOFuncAppStatement r stmt
    , VariableElim r typ
    )
  => GenState (Maybe (MS (r stmt)))
genDerivedCall = do
  dvName <- genICName DerivedValuesFn
  genInOutCall dvName getDerivedIns getDerivedOuts

-- | Generates a call to the function for checking constraints on the input.
genConstraintCall
  ::
    ( ValueSym r typ
    , Argument r
    , Literal r typ
    , MathConstant r
    , TypeSym r typ
    , OOTypeSym r typ
    , VariableSym r typ
    , OOVariableSym r typ
    , VariableValue r
    , BooleanExpression r
    , Comparison r
    , NumericExpression r
    , ValueExpression r typ
    , SelfSym r
    , InternalValueExp r typ
    , OOValueExpression r typ
    , List r
    , Reference r
    , Set r
    , ValueStatement r stmt
    , TypeElim r typ
    , VariableElim r typ
    )
  => GenState (Maybe (MS (r stmt)))
genConstraintCall = do
  icName <- genICName InputConstraintsFn
  val <- genFuncCall icName void getConstraintParams
  pure $ fmap valStmt val

-- | Generates a call to a calculation function, given the 'CodeDefinition' for the
-- value being calculated.
genCalcCall
  ::
    ( ValueSym r typ
    , Argument r
    , Literal r typ
    , MathConstant r
    , TypeSym r typ
    , OOTypeSym r typ
    , VariableSym r typ
    , OOVariableSym r typ
    , ScopeSym r
    , VariableValue r
    , BooleanExpression r
    , Comparison r
    , NumericExpression r
    , ValueExpression r typ
    , SelfSym r
    , InternalValueExp r typ
    , OOValueExpression r typ
    , List r
    , Reference r
    , Set r
    , DeclStatement r stmt bod
    , TypeElim r typ
    , VariableElim r typ
    )
  => CodeDefinition -> GenState (Maybe (MS (r stmt)))
genCalcCall c = do
  g <- get
  let scp = convScope $ currentScope g
  t <- codeType c
  val <- genFuncCall (codeName c) (convTypeOO t) (getCalcParams c)
  v <- mkVar (quantvar c)
  pure $ fmap (varDecDef v scp) val

-- | Generates a call to the function for printing outputs.
genOutputCall
  ::
    ( ValueSym r typ
    , Argument r
    , Literal r typ
    , MathConstant r
    , TypeSym r typ
    , OOTypeSym r typ
    , VariableSym r typ
    , OOVariableSym r typ
    , VariableValue r
    , BooleanExpression r
    , Comparison r
    , NumericExpression r
    , ValueExpression r typ
    , SelfSym r
    , InternalValueExp r typ
    , OOValueExpression r typ
    , List r
    , Reference r
    , Set r
    , ValueStatement r stmt
    , TypeElim r typ
    , VariableElim r typ
    )
  => GenState (Maybe (MS (r stmt)))
genOutputCall = do
  woName <- genICName WriteOutput
  val <- genFuncCall woName void getOutputParams
  pure $ fmap valStmt val

-- | Generates a function call given the name, return type, and arguments to
-- the function.
genFuncCall
  ::
    ( ValueSym r typ
    , Argument r
    , Literal r typ
    , MathConstant r
    , TypeSym r typ
    , OOTypeSym r typ
    , VariableSym r typ
    , OOVariableSym r typ
    , VariableValue r
    , BooleanExpression r
    , Comparison r
    , NumericExpression r
    , ValueExpression r typ
    , SelfSym r
    , InternalValueExp r typ
    , OOValueExpression r typ
    , List r
    , Reference r
    , Set r
    , TypeElim r typ
    , VariableElim r typ
    )
  => Name
  -> VS (r typ)
  -> GenState [CodeVarChunk]
  -> GenState (Maybe (SValue r))
genFuncCall n t funcPs = do
  mm <- genCall n
  let genFuncCall' Nothing = pure Nothing
      genFuncCall' (Just m) = do
        cs <- funcPs
        pvals <- mapM mkVal cs
        val <- fApp m n t pvals []
        pure $ Just val
  genFuncCall' mm

-- | Generates a function call given the name, inputs, and outputs for the
-- function.
genInOutCall
  ::
    ( TypeSym r typ
    , OOTypeSym r typ
    , VariableSym r typ
    , OOVariableSym r typ
    , VariableValue r
    , SelfSym r
    , FuncAppStatement r stmt
    , OOFuncAppStatement r stmt
    , VariableElim r typ
    )
  => Name
  -> GenState [CodeVarChunk]
  -> GenState [CodeVarChunk]
  -> GenState (Maybe (MS (r stmt)))
genInOutCall n inFunc outFunc = do
  mm <- genCall n
  let genInOutCall' Nothing = pure Nothing
      genInOutCall' (Just m) = do
        ins' <- inFunc
        outs' <- outFunc
        ins <- mapM mkVar (ins' \\ outs')
        outs <- mapM mkVar (outs' \\ ins')
        both <- mapM mkVar (ins' `intersect` outs')
        stmt <- fAppInOut m n (map valueOf ins) outs both
        pure $ Just stmt
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
      genCallExported m = pure m
      genCallInClass Nothing = pure Nothing
      genCallInClass (Just c) = if c == currc then pure $ Map.lookup c (eMap
        g) <|> error (c ++ " class missing from export map")
        else pure Nothing
  genCallExported $ Map.lookup n (eMap g)

-- Procedural Versions --

-- | Generates calls to all of the input-related functions. First is the call to
-- the function for reading inputs, then the function for calculating derived
-- inputs, then the function for checking input constraints.
genAllInputCallsProc
  ::
    ( TypeSym r typ
    , ValueSym r typ
    , Literal r typ
    , MathConstant r
    , VariableSym r typ
    , VariableValue r
    , BooleanExpression r
    , Comparison r
    , NumericExpression r
    , ValueExpression r typ
    , NativeVector r typ
    , FuncAppStatement r stmt
    , Argument r
    , List r
    , Reference r
    , Set r
    , ValueStatement r stmt
    , TypeElim r typ
    )
  => GenState [MS (r stmt)]
genAllInputCallsProc = do
  gi <- genInputCallProc
  dv <- genDerivedCallProc
  ic <- genConstraintCallProc
  pure $ catMaybes [gi, dv, ic]

-- | Generates a call to the function for reading inputs from a file.
genInputCallProc
  :: (FuncAppStatement r stmt, TypeSym r typ, VariableSym r typ, VariableValue r)
  => GenState (Maybe (MS (r stmt)))
genInputCallProc = do
  giName <- genICName GetInput
  genInOutCallProc giName getInputFormatIns getInputFormatOuts

-- | Generates a call to the function for calculating derived inputs.
genDerivedCallProc
  :: (FuncAppStatement r stmt, TypeSym r typ, VariableSym r typ, VariableValue r)
  => GenState (Maybe (MS (r stmt)))
genDerivedCallProc = do
  dvName <- genICName DerivedValuesFn
  genInOutCallProc dvName getDerivedIns getDerivedOuts

-- | Generates a call to the function for checking constraints on the input.
genConstraintCallProc
  ::
    ( TypeSym r typ
    , ValueSym r typ
    , Literal r typ
    , MathConstant r
    , VariableSym r typ
    , VariableValue r
    , BooleanExpression r
    , Comparison r
    , NumericExpression r
    , ValueExpression r typ
    , NativeVector r typ
    , Argument r
    , List r
    , Reference r
    , Set r
    , ValueStatement r stmt
    , TypeElim r typ
    )
  => GenState (Maybe (MS (r stmt)))
genConstraintCallProc = do
  icName <- genICName InputConstraintsFn
  val <- genFuncCallProc icName void getConstraintParams
  pure $ fmap valStmt val

-- | Generates a call to a calculation function, given the 'CodeDefinition' for the
-- value being calculated.
genCalcCallProc
  ::
    ( TypeSym r typ
    , ValueSym r typ
    , Literal r typ
    , MathConstant r
    , VariableValue r
    , BooleanExpression r
    , Comparison r
    , NumericExpression r
    , VariableSym r typ
    , ScopeSym r
    , ValueExpression r typ
    , DeclStatement r stmt bod
    , Argument r
    , List r
    , NativeVector r typ
    , Reference r
    , Set r
    , TypeElim r typ
    )
  => CodeDefinition -> GenState (Maybe (MS (r stmt)))
genCalcCallProc c = do
  g <- get
  let scp = convScope $ currentScope g
  t <- codeType c
  val <- genFuncCallProc (codeName c) (convType t) (getCalcParams c)
  v <- mkVarProc (quantvar c)
  pure $ fmap ((`varDecDef` scp) v) val

-- | Generates a call to the function for printing outputs.
genOutputCallProc
  ::
    ( TypeSym r typ
    , ValueSym r typ
    , Literal r typ
    , MathConstant r
    , VariableSym r typ
    , VariableValue r
    , BooleanExpression r
    , Comparison r
    , NumericExpression r
    , ValueExpression r typ
    , Argument r
    , List r
    , NativeVector r typ
    , Reference r
    , Set r
    , ValueStatement r stmt
    , TypeElim r typ
    )
  => GenState (Maybe (MS (r stmt)))
genOutputCallProc = do
  woName <- genICName WriteOutput
  val <- genFuncCallProc woName void getOutputParams
  pure $ fmap valStmt val

-- | Generates a function call given the name, return type, and arguments to
-- the function.
genFuncCallProc
  ::
    ( TypeSym r typ
    , ValueSym r typ
    , Literal r typ
    , MathConstant r
    , VariableSym r typ
    , VariableValue r
    , BooleanExpression r
    , Comparison r
    , NumericExpression r
    , ValueExpression r typ
    , Argument r
    , List r
    , NativeVector r typ
    , Reference r
    , Set r
    , TypeElim r typ
    )
  => Name
  -> VS (r typ)
  -> GenState [CodeVarChunk]
  -> GenState (Maybe (SValue r))
genFuncCallProc n t funcPs = do
  mm <- genCall n
  let genFuncCall' Nothing = pure Nothing
      genFuncCall' (Just m) = do
        cs <- funcPs
        pvals <- mapM mkValProc cs
        val <- fAppProc m n t pvals []
        pure $ Just val
  genFuncCall' mm

-- | Generates a function call given the name, inputs, and outputs for the
-- function.
genInOutCallProc
  :: (FuncAppStatement r stmt, TypeSym r typ, VariableSym r typ, VariableValue r)
  => Name
  -> GenState [CodeVarChunk]
  -> GenState [CodeVarChunk]
  -> GenState (Maybe (MS (r stmt)))
genInOutCallProc n inFunc outFunc = do
  mm <- genCall n
  let genInOutCall' Nothing = pure Nothing
      genInOutCall' (Just m) = do
        ins' <- inFunc
        outs' <- outFunc
        ins <- mapM mkVarProc (ins' \\ outs')
        outs <- mapM mkVarProc (outs' \\ ins')
        both <- mapM mkVarProc (ins' `intersect` outs')
        stmt <- fAppInOutProc m n (map valueOf ins) outs both
        pure $ Just stmt
  genInOutCall' mm
