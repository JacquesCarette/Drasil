{-# LANGUAGE PostfixOperators #-}

-- | Language-polymorphic functions that are defined by GOOL code
module Drasil.Shared.LanguageRenderer.Macros (
  ifExists, decrement1, increment, increment1, runStrategy,
  listSlice, makeSetterVal, stringListVals, stringListLists, forRange, notifyObservers,
  notifyObservers'
) where

import Drasil.Shared.CodeType (CodeType(..))
import Drasil.Shared.InterfaceCommon (Label, MSBody, MSBlock, VSFunction, VSType,
  SVariable, SValue, MSStatement, bodyStatements, oneLiner, TypeElim(getType),
  VariableElim(..), listOf, ValueSym(valueType),
  NumericExpression((#+), (#-), (#*), (#/)), Comparison(..),
  BooleanExpression((?&&), (?||)), at, StatementSym(multi),
  AssignStatement((&+=), (&-=), (&++)), (&=), convScope)
import qualified Drasil.Shared.InterfaceCommon as IC (BlockSym(block),
  TypeSym(int, listInnerType), VariableSym(var), ScopeSym(..), Literal(litInt),
  VariableValue(valueOf), ValueExpression(notNull), List(listSize, listAppend,
  listAccess, intToIndex), StatementSym(valStmt, emptyStmt), AssignStatement(assign),
  DeclStatement(varDecDef, listDec),  ControlStatement(ifCond, for, forRange),
  ValueExpression(inlineIf))
import Drasil.GOOL.InterfaceGOOL (($.), observerListName)
import Drasil.Shared.RendererClassesCommon (CommonRenderSym, RenderValue(cast),
  ValueElim(valueInt))
import qualified Drasil.Shared.RendererClassesCommon as S (
  RenderStatement(stmt))
import qualified Drasil.Shared.RendererClassesCommon as RC (BodyElim(..),
  StatementElim(statement))
import Drasil.GOOL.RendererClassesOO (OORenderSym)
import Drasil.Shared.Helpers (toCode, onStateValue, on2StateValues)
import Drasil.Shared.State (MS, lensMStoVS, genVarName, genLoopIndex,
  genVarNameIf, getVarScope)

import Data.Maybe (fromMaybe, isNothing)
import Data.Functor ((<&>))
import Control.Lens.Zoom (zoom)
import Text.PrettyPrint.HughesPJ (Doc, vcat)

ifExists :: (CommonRenderSym r) => SValue r -> MSBody r -> MSBody r -> MSStatement r
ifExists v ifBody = IC.ifCond [(IC.notNull v, ifBody)]

decrement1 :: (CommonRenderSym r) => SVariable r -> MSStatement r
decrement1 v = v &-= IC.litInt 1

increment :: (CommonRenderSym r) => SVariable r -> SValue r -> MSStatement r
increment vr vl = vr &= IC.valueOf vr #+ vl

increment1 :: (CommonRenderSym r) => SVariable r -> MSStatement r
increment1 vr = vr &+= IC.litInt 1

strat :: (CommonRenderSym r, Monad r) => MSStatement r -> MSBody r -> MS (r Doc)
strat = on2StateValues (\result b -> toCode $ vcat [RC.body b,
  RC.statement result])

runStrategy :: (CommonRenderSym r, Monad r) => Label -> [(Label, MSBody r)] ->
  Maybe (SValue r) -> Maybe (SVariable r) -> MS (r Doc)
runStrategy l strats rv av = maybe
  (strError l "RunStrategy called on non-existent strategy")
  (strat (S.stmt resultState)) (lookup l strats)
  where resultState = maybe IC.emptyStmt asgState av
        asgState v = maybe (strError l
          "Attempt to assign null return to a Value") (v &=) rv
        strError n s = error $ "Strategy '" ++ n ++ "': " ++ s ++ "."

listSlice :: (CommonRenderSym r) => Maybe (SValue r) -> Maybe (SValue r) ->
  Maybe (SValue r) -> SVariable r -> SValue r -> MSBlock r
listSlice beg end step vnew vold = do

  l_temp <- genVarName [] "temp"
  l_i <- genLoopIndex
  vn <- zoom lensMStoVS vnew
  scpData <- getVarScope $ variableName vn
  let scp = convScope scpData
      var_temp = IC.var l_temp (onStateValue variableType vnew)
      v_temp = IC.valueOf var_temp
      var_i = IC.var l_i IC.int
      v_i = IC.valueOf var_i

  let step' = fromMaybe (IC.litInt 1) step
  stepV <- zoom lensMStoVS step'
  let mbStepV = valueInt stepV

  -- Generate fresh variable names if required
  begName <- genVarNameIf (isNothing beg && isNothing mbStepV) "begIdx"
  endName <- genVarNameIf (isNothing end && isNothing mbStepV) "endIdx"

  let (setBeg, begVal) = makeSetterVal begName step' mbStepV beg (IC.litInt 0)    (IC.listSize vold #- IC.litInt 1) scp
      (setEnd, endVal) = makeSetterVal endName step' mbStepV end (IC.listSize vold) (IC.litInt (-1)) scp

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
                    then begVal ?<= v_i ?&& v_i ?< endVal ?&& step' ?> IC.litInt 0
                    else endVal ?< v_i ?&& v_i ?<= begVal ?&& step' ?< IC.litInt 0
                -- If bounds are not litInt's, do both two-sided checks
                _ ->  begVal ?<= v_i ?&& v_i ?< endVal ?&& step' ?> IC.litInt 0 ?||
                      endVal ?< v_i ?&& v_i ?<= begVal ?&& step' ?< IC.litInt 0

  IC.block [
    IC.listDec 0 var_temp scp,
    setBeg, setEnd,
    IC.for (IC.varDecDef var_i IC.local begVal) cond
      (maybe (var_i &++) (var_i &+=) step)
      (oneLiner $ IC.valStmt $ IC.listAppend v_temp (IC.listAccess vold v_i)),
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
makeSetterVal :: (CommonRenderSym r) => Label -> SValue r -> Maybe Integer ->
  Maybe (SValue r) -> SValue r -> SValue r -> r (IC.Scope r) ->
  (MSStatement r, SValue r)
makeSetterVal _     _    _      (Just v) _  _  _   = (IC.emptyStmt, v)
makeSetterVal _     _   (Just s) _       lb rb _   = (IC.emptyStmt, if s > 0 then lb else rb)
makeSetterVal vName step _       _       lb rb  scp =
  let theVar = IC.var vName IC.int
      theSetter = IC.varDecDef theVar scp $ IC.inlineIf (step ?> IC.litInt 0) lb rb
  in (theSetter, IC.intToIndex $ IC.valueOf theVar)

stringListVals :: (CommonRenderSym r) => [SVariable r] -> SValue r -> MSStatement r
stringListVals vars sl = zoom lensMStoVS sl >>= (\slst -> multi $ checkList
  (getType $ valueType slst))
  where checkList (List String) = assignVals vars 0
        checkList _ = error
          "Value passed to stringListVals must be a list of strings"
        assignVals [] _ = []
        assignVals (v:vs) n = IC.assign v (cast (onStateValue variableType v)
          (IC.listAccess sl (IC.litInt n))) : assignVals vs (n+1)

stringListLists :: (CommonRenderSym r) => [SVariable r] -> SValue r -> MSStatement r
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
    loop = IC.forRange var_i (IC.litInt 0) (IC.listSize sl #/ numLists)
      (IC.litInt 1) (bodyStatements $ appendLists (map IC.valueOf lsts) 0)
    appendLists [] _ = []
    appendLists (v:vs) n = IC.valStmt (IC.listAppend v (cast
      (IC.listInnerType $ onStateValue valueType v)
      (IC.listAccess sl ((v_i #* numLists) #+ IC.litInt n))))
      : appendLists vs (n+1)
    numLists = IC.litInt (toInteger $ length lsts)
    var_i = IC.var l_i IC.int
    v_i = IC.valueOf var_i
  checkList (getType $ valueType slst)

forRange :: (CommonRenderSym r) => SVariable r -> SValue r -> SValue r -> SValue r ->
  MSBody r -> MSStatement r
forRange i initv finalv stepv = IC.for (IC.varDecDef i IC.local initv)
  (IC.valueOf i ?< finalv) (i &+= stepv)

observerIndex :: (CommonRenderSym r) => SVariable r
observerIndex = IC.var "observerIndex" IC.int

observerIdxVal :: (CommonRenderSym r) => SValue r
observerIdxVal = IC.valueOf observerIndex

obsList :: (CommonRenderSym r) => VSType r -> SValue r
obsList t = IC.valueOf $ listOf observerListName t

notify :: (OORenderSym r) => VSType r -> VSFunction r -> MSBody r
notify t f = oneLiner $ IC.valStmt $ at (obsList t) observerIdxVal $. f

notifyObservers :: (OORenderSym r) => VSFunction r -> VSType r -> MSStatement r
notifyObservers f t = IC.for initv (observerIdxVal ?< IC.listSize (obsList t))
  (observerIndex &++) (notify t f)
  where initv = IC.varDecDef observerIndex IC.local $ IC.litInt 0

notifyObservers' :: (OORenderSym r) => VSFunction r -> VSType r -> MSStatement r
notifyObservers' f t = IC.forRange observerIndex initv (IC.listSize $ obsList t )
    (IC.litInt 1) (notify t f)
    where initv = IC.litInt 0
