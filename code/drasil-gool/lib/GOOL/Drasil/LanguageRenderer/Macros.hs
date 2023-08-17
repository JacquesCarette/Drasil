{-# LANGUAGE PostfixOperators #-}

-- | Language-polymorphic functions that are defined by GOOL code
module GOOL.Drasil.LanguageRenderer.Macros (
  ifExists, decrement1, increment, increment1, runStrategy, 
  listSlice, stringListVals, stringListLists, forRange, notifyObservers,
  notifyObservers', checkState
) where

import GOOL.Drasil.CodeType (CodeType(..))
import GOOL.Drasil.ClassInterface (Label, MSBody, MSBlock, VSType, SVariable, 
  SValue, VSFunction, MSStatement, bodyStatements, oneLiner, TypeElim(getType),
  VariableElim(variableType), listOf, ValueSym(valueType), 
  NumericExpression((#+), (#*), (#/)), Comparison(..), at, ($.),
  StatementSym(multi), AssignStatement((&+=), (&-=), (&++)), (&=), 
  observerListName)
import qualified GOOL.Drasil.ClassInterface as S (BlockSym(block), 
  TypeSym(int, string, listInnerType), VariableSym(var), Literal(litInt), 
  VariableValue(valueOf), ValueExpression(notNull), 
  List(listSize, listAppend, listAccess), StatementSym(valStmt), 
  AssignStatement(assign), DeclStatement(varDecDef, listDec), 
  ControlStatement(ifCond, switch, for, forRange))
import GOOL.Drasil.RendererClasses (RenderSym, RenderValue(cast))
import qualified GOOL.Drasil.RendererClasses as S (
  RenderStatement(stmt, emptyStmt))
import qualified GOOL.Drasil.RendererClasses as RC (BodyElim(..),
  StatementElim(statement))
import GOOL.Drasil.Helpers (toCode, onStateValue, on2StateValues)
import GOOL.Drasil.State (MS, lensMStoVS, genVarName, genLoopIndex)

import Data.Maybe (fromMaybe)
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
listSlice b e s vnew vold = do
  l_temp <- genVarName [] "temp"
  l_i <- genLoopIndex
  let
    var_temp = S.var l_temp (onStateValue variableType vnew)
    v_temp = S.valueOf var_temp
    var_i = S.var l_i S.int
    v_i = S.valueOf var_i
  S.block [
    S.listDec 0 var_temp,
    S.for (S.varDecDef var_i (fromMaybe (S.litInt 0) b))
      (v_i ?< fromMaybe (S.listSize vold) e) (maybe (var_i &++) (var_i &+=) s)
      (oneLiner $ S.valStmt $ S.listAppend v_temp (S.listAccess vold v_i)),
    vnew &= v_temp]
      
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
        
checkState :: (RenderSym r) => Label -> [(SValue r, MSBody r)] -> MSBody r -> 
  MSStatement r
checkState l = S.switch (S.valueOf $ S.var l S.string)
