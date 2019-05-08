module Drasil.GamePhysics.DesignDec.Vector where

import Language.Drasil

vR1, vR2, vNMass, vTMass, vBounce, vJnAcc, vJtAcc, vJBias,
  vBias, vA, vB, vId, vNormal, vCount, vArr, vElast, vSurfaceVal, vAs, vAb, vBodyA, vBodyB, vThreadA, vThreadB, vContacts, vHandler, vHandlerA,
  vHandlerB, vSwapper, vStamp, vState, vPoints, vArb :: VarChunk

vR1         = makeVC "R1"         (nounPhrase "R1")         (sub (lR) (Atomic "1"))
vR2         = makeVC "R2"         (nounPhrase "R2")         (sub (lR) (Atomic "2"))
vNMass      = makeVC "nMass"      (nounPhrase "nMass")      (Atomic "nMass")
vTMass      = makeVC "tMass"      (nounPhrase "tMass")      (Atomic "tMass")
vBounce     = makeVC "bounce"     (nounPhrase "bounce")     (Atomic "bounce")
vJnAcc      = makeVC "jnAcc"      (nounPhrase "jnAcc")      (Atomic "jnAcc")
vJtAcc      = makeVC "jtAcc"      (nounPhrase "jtAcc")      (Atomic "jtAcc")
vJBias      = makeVC "jBias"      (nounPhrase "jBias")      (Atomic "jBias")
vA          = makeVC "a"          (nounPhrase "a")          (Atomic "a")
vB          = makeVC "b"          (nounPhrase "b")          (Atomic "b")
vId         = makeVC "id"         (nounPhrase "id")         (Atomic "id")
vNormal     = makeVC "normal"     (nounPhrase "normal")     (Atomic "normal")
vCount      = makeVC "count"      (nounPhrase "count")      (Atomic "count")
vHandler    = makeVC "handler"    (nounPhrase "hanlder")    (Atomic "hanlder")
vHandlerA   = makeVC "handlerA"   (nounPhrase "handlerA")   (Atomic "handlerA")
vHandlerB   = makeVC "handlerB"   (nounPhrase "handlerB")   (Atomic "handlerB")
vArr        = makeVC "arr"        (nounPhrase "arr")        (Atomic "arr")
vElast      = makeVC "elast"      (nounPhrase "elast")      (Atomic "elast")
vSurfaceVal = makeVC "surfaceVal" (nounPhrase "surfaceVal") (Atomic "surfaceVal")
vAs         = makeVC "as"         (nounPhrase "as")         (Atomic "as")
vAb         = makeVC "ab"         (nounPhrase "ab")         (Atomic "ab")
vBodyA      = makeVC "bodyA"      (nounPhrase "bodyA")      (Atomic "bodyA")
vBodyB      = makeVC "bodyB"      (nounPhrase "bodyB")      (Atomic "bodyB")
vThreadA    = makeVC "threadA"    (nounPhrase "threadA")    (Atomic "threadA")
vThreadB    = makeVC "threadB"    (nounPhrase "threadB")    (Atomic "threadB")
vContacts   = makeVC "contacts"   (nounPhrase "contacts")   (Atomic "contacts")
vSwapper    = makeVC "swapper"    (nounPhrase "swapper")    (Atomic "swapper")
vStamp      = makeVC "stamp"      (nounPhrase "stamp")      (Atomic "stamp")
vState      = makeVC "state"      (nounPhrase "state")      (Atomic "state")
vPoints     = makeVC "points"     (nounPhrase "points")     (Atomic "points")
vArb        = makeVC "arb"        (nounPhrase "arb")        (Atomic "arb")

--arbiterGetCount, arbiterGetNormal, arbiterIsFirstContact :: funcDef

arbiterGetCount = funcDef "arbiterGetCount" [arb] Rational
  [
    FRet (FCall (asExpr getCount) [arb])
  ]

arbiterGetNormal = funcDef "arbiterGetNormal" [arb] Rational
  [
    FRet (FCall (asExpr getNormal) [arb])
  ]