{-# LANGUAGE PostfixOperators #-}

-- | Language-polymorphic functions that are defined by GOOL code
module GOOL.Drasil.LanguageRenderer.Macros (
  ifExists, decrement1, increment, increment1, runStrategy, 
  listSlice, makeSetterVal, stringListVals, stringListLists, forRange, notifyObservers,
  notifyObservers'
) where

import GOOL.Drasil.CodeType (CodeType(..))
import GOOL.Drasil.ClassInterface (Label, MSBody, MSBlock, VSType, SVariable, 
  SValue, VSFunction, MSStatement, bodyStatements, oneLiner, TypeElim(getType),
  VariableElim(variableType), listOf, ValueSym(valueType), 
  NumericExpression((#+), (#-), (#*), (#/)), Comparison(..),
  BooleanExpression((?&&), (?||)), at, ($.), StatementSym(multi),
  AssignStatement((&+=), (&-=), (&++)), (&=), observerListName)
import qualified GOOL.Drasil.ClassInterface as S (BlockSym(block), 
  TypeSym(int, listInnerType), VariableSym(var), Literal(litInt), 
  VariableValue(valueOf), ValueExpression(notNull), 
  List(listSize, listAppend, listAccess, intToIndex), StatementSym(valStmt), 
  AssignStatement(assign), DeclStatement(varDecDef, listDec), 
  ControlStatement(ifCond, for, forRange), ValueExpression(inlineIf))
import GOOL.Drasil.RendererClasses (RenderSym, RenderValue(cast), 
  ValueElim(valueInt))
import qualified GOOL.Drasil.RendererClasses as S (
  RenderStatement(stmt, emptyStmt))
import qualified GOOL.Drasil.RendererClasses as RC (BodyElim(..),
  StatementElim(statement))
import GOOL.Drasil.Helpers (toCode, onStateValue, on2StateValues)
import GOOL.Drasil.State (MS, lensMStoVS, genVarName, genLoopIndex)

import Data.Maybe (fromMaybe)
import Data.Functor ((<&>))
import Control.Lens.Zoom (zoom)
import Text.PrettyPrint.HughesPJ (Doc, vcat)

ifExists :: (RenderSym r) => SValue r -> MSBody r -> MSBody r -> MSStatement r
ifExists v ifBody = S.ifCond [(S.notNull v, ifBody)]

decrement1 :: (RenderSym r) => SVariable r -> MSStatement r
decrement1 v = v &-= S.litInt 1

increment :: (RenderSym r) => SVariable r -> SValue r -> MSStatement r
increment vr vl = vr &= S.valueOf vr #+ vl

increment1 :: (RenderSym r) => SVariable r -> MSStatement r
increment1 vr = vr &+= S.litInt 1

strat :: (RenderSym r, Monad r) => MSStatement r -> MSBody r -> MS (r Doc)
strat = on2StateValues (\result b -> toCode $ vcat [RC.body b, 
  RC.statement result])

runStrategy :: (RenderSym r, Monad r) => Label -> [(Label, MSBody r)] -> 
  Maybe (SValue r) -> Maybe (SVariable r) -> MS (r Doc)
runStrategy l strats rv av = maybe
  (strError l "RunStrategy called on non-existent strategy") 
  (strat (S.stmt resultState)) (lookup l strats)
  where resultState = maybe S.emptyStmt asgState av
        asgState v = maybe (strError l 
          "Attempt to assign null return to a Value") (v &=) rv
        strError n s = error $ "Strategy '" ++ n ++ "': " ++ s ++ "."

listSlice :: (RenderSym r) => Maybe (SValue r) -> Maybe (SValue r) -> 
  Maybe (SValue r) -> SVariable r -> SValue r -> MSBlock r
listSlice beg end step vnew vold = do
  
  l_temp <- genVarName [] "temp"
  l_i <- genLoopIndex
  let var_temp = S.var l_temp (onStateValue variableType vnew)
      v_temp = S.valueOf var_temp
      var_i = S.var l_i S.int
      v_i = S.valueOf var_i

  let step' = fromMaybe (S.litInt 1) step
  stepV <- zoom lensMStoVS step'
  let mbStepV = valueInt stepV
      (setBeg, begVal) = makeSetterVal "begIdx" step' mbStepV beg (S.litInt 0)    (S.listSize vold #- S.litInt 1)
      (setEnd, endVal) = makeSetterVal "endIdx" step' mbStepV end (S.listSize vold) (S.litInt (-1))

  mbBegV <- case beg of
        Nothing -> pure Nothing
        (Just b) -> zoom lensMStoVS b <&> valueInt
  mbEndV <- case end of
        Nothing -> pure Nothing
        (Just e) -> zoom lensMStoVS e <&> valueInt
  -- Get the condition for the for-loop
  let cond = case mbStepV of
              -- If step is a litInt, do a one-sided check
              (Just s) -> if s >= 0 then v_i ?< endVal else v_i ?> endVal
              Nothing -> case (mbBegV, mbEndV) of
                -- If both bounds are litInt's, do a two-sided check.
                -- Also, make sure step is in same direction as check.
                (Just b, Just e) -> if e >= b 
                    then begVal ?<= v_i ?&& v_i ?< endVal ?&& step' ?> S.litInt 0
                    else endVal ?< v_i ?&& v_i ?<= begVal ?&& step' ?< S.litInt 0
                -- If bounds are not litInt's, do both two-sided checks
                _ ->  begVal ?<= v_i ?&& v_i ?< endVal ?&& step' ?> S.litInt 0 ?|| 
                      endVal ?< v_i ?&& v_i ?<= begVal ?&& step' ?< S.litInt 0

  S.block [
    S.listDec 0 var_temp,
    setBeg, setEnd,
    S.for (S.varDecDef var_i begVal) cond (maybe (var_i &++) (var_i &+=) step)
      (oneLiner $ S.valStmt $ S.listAppend v_temp (S.listAccess vold v_i)),
    vnew &= v_temp]

-- Java, C#, C++, and Swift --
-- | Gets the expression and code for setting bounds in a list slice
--   Input: 
--   - String: Variable name for bound (to be created if necessary),
--   - SValue: step value
--   - Maybe Integer: literal value of step, if exists
--   - Maybe SValue: given value of bound
--   - SValue: value of bound if bound not given and step is positive
--   - SValue: value of bound if bound not given and step is negative
--   Output: (MSStatement, SValue): (setter, value) of bound
makeSetterVal :: RenderSym r => Label -> SValue r -> Maybe Integer -> Maybe (SValue r) -> SValue r -> SValue r -> (MSStatement r, SValue r)
makeSetterVal _     _    _      (Just v) _  _  = (S.emptyStmt, v)
makeSetterVal _     _   (Just s) _       lb rb = (S.emptyStmt, if s > 0 then lb else rb)
makeSetterVal vName step _       _       lb rb = 
  let theVar = S.var vName S.int
      theSetter = S.varDecDef theVar $ S.inlineIf (step ?> S.litInt 0) lb rb
  in (theSetter, S.intToIndex $ S.valueOf theVar)
      
stringListVals :: (RenderSym r) => [SVariable r] -> SValue r -> MSStatement r
stringListVals vars sl = zoom lensMStoVS sl >>= (\slst -> multi $ checkList 
  (getType $ valueType slst))
  where checkList (List String) = assignVals vars 0
        checkList _ = error 
          "Value passed to stringListVals must be a list of strings"
        assignVals [] _ = []
        assignVals (v:vs) n = S.assign v (cast (onStateValue variableType v) 
          (S.listAccess sl (S.litInt n))) : assignVals vs (n+1)

stringListLists :: (RenderSym r) => [SVariable r] -> SValue r -> MSStatement r
stringListLists lsts sl = do
  slst <- zoom lensMStoVS sl
  l_i <- genLoopIndex
  let
    checkList (List String) = mapM (zoom lensMStoVS) lsts >>= listVals .
      map (getType . variableType)
    checkList _ = error
      "Value passed to stringListLists must be a list of strings"
    listVals [] = loop
    listVals (List _:vs) = listVals vs
    listVals _ = error
      "All values passed to stringListLists must have list types"
    loop = S.forRange var_i (S.litInt 0) (S.listSize sl #/ numLists)
      (S.litInt 1) (bodyStatements $ appendLists (map S.valueOf lsts) 0)
    appendLists [] _ = []
    appendLists (v:vs) n = S.valStmt (S.listAppend v (cast
      (S.listInnerType $ onStateValue valueType v)
      (S.listAccess sl ((v_i #* numLists) #+ S.litInt n))))
      : appendLists vs (n+1)
    numLists = S.litInt (toInteger $ length lsts)
    var_i = S.var l_i S.int
    v_i = S.valueOf var_i
  checkList (getType $ valueType slst)

forRange :: (RenderSym r) => SVariable r -> SValue r -> SValue r -> SValue r -> 
  MSBody r -> MSStatement r
forRange i initv finalv stepv = S.for (S.varDecDef i initv) (S.valueOf i ?< 
  finalv) (i &+= stepv)

observerIndex :: (RenderSym r) => SVariable r
observerIndex = S.var "observerIndex" S.int

observerIdxVal :: (RenderSym r) => SValue r
observerIdxVal = S.valueOf observerIndex

obsList :: (RenderSym r) => VSType r -> SValue r
obsList t = S.valueOf $ observerListName `listOf` t

notify :: (RenderSym r) => VSType r -> VSFunction r -> MSBody r
notify t f = oneLiner $ S.valStmt $ at (obsList t) observerIdxVal $. f

notifyObservers :: (RenderSym r) => VSFunction r -> VSType r -> MSStatement r
notifyObservers f t = S.for initv (observerIdxVal ?< S.listSize (obsList t)) 
  (observerIndex &++) (notify t f)
  where initv = S.varDecDef observerIndex $ S.litInt 0

notifyObservers' :: (RenderSym r) => VSFunction r -> VSType r -> MSStatement r
notifyObservers' f t = S.forRange observerIndex initv (S.listSize $ obsList t) 
    (S.litInt 1) (notify t f)
    where initv = S.litInt 0
