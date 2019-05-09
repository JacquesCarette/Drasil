module Drasil.GamePhysics.DesignDec.Vector where

import Language.Drasil

v_R1, v_R2, v_nMass, v_tMass, v_bounce, v_jnAcc, v_jtAcc, v_jBias,
  v_bias, v_a, v_b, v_id, v_normal, v_count, v_arr, v_elast,
  v_surfaceVal, v_as, v_ab, v_bodyA, v_bodyB, v_threadA, 
  v_threadB, v_contacts, v_handler, v_handlerA,
  v_handlerB, v_swapper, v_stamp, v_state, v_points, v_arb :: VarChunk

v_R1         = makeVC "R1"         (nounPhrase "R1")         (sub (lR) (Atomic "1"))
v_R2         = makeVC "R2"         (nounPhrase "R2")         (sub (lR) (Atomic "2"))
v_nMass      = makeVC "nMass"      (nounPhrase "nMass")      (Atomic "nMass")
v_tMass      = makeVC "tMass"      (nounPhrase "tMass")      (Atomic "tMass")
v_bounce     = makeVC "bounce"     (nounPhrase "bounce")     (Atomic "bounce")
v_jnAcc      = makeVC "jnAcc"      (nounPhrase "jnAcc")      (Atomic "jnAcc")
v_jtAcc      = makeVC "jtAcc"      (nounPhrase "jtAcc")      (Atomic "jtAcc")
v_jBias      = makeVC "jBias"      (nounPhrase "jBias")      (Atomic "jBias")
v_a          = makeVC "a"          (nounPhrase "a")          (Atomic "a")
v_b          = makeVC "b"          (nounPhrase "b")          (Atomic "b")
v_id         = makeVC "id"         (nounPhrase "id")         (Atomic "id")
v_normal     = makeVC "normal"     (nounPhrase "normal")     (Atomic "normal")
v_count      = makeVC "count"      (nounPhrase "count")      (Atomic "count")
v_handler    = makeVC "handler"    (nounPhrase "handler")    (Atomic "handler")
v_handlerA   = makeVC "handlerA"   (nounPhrase "handlerA")   (Atomic "handlerA")
v_handlerB   = makeVC "handlerB"   (nounPhrase "handlerB")   (Atomic "handlerB")
v_arr        = makeVC "arr"        (nounPhrase "arr")        (Atomic "arr")
v_elast      = makeVC "elast"      (nounPhrase "elast")      (Atomic "elast")
v_surfaceVal = makeVC "surfaceVal" (nounPhrase "surfaceVal") (Atomic "surfaceVal")
v_as         = makeVC "as"         (nounPhrase "as")         (Atomic "as")
v_ab         = makeVC "ab"         (nounPhrase "ab")         (Atomic "ab")
v_bodyA      = makeVC "bodyA"      (nounPhrase "bodyA")      (Atomic "bodyA")
v_bodyB      = makeVC "bodyB"      (nounPhrase "bodyB")      (Atomic "bodyB")
v_threadA    = makeVC "threadA"    (nounPhrase "threadA")    (Atomic "threadA")
v_threadB    = makeVC "threadB"    (nounPhrase "threadB")    (Atomic "threadB")
v_contacts   = makeVC "contacts"   (nounPhrase "contacts")   (Atomic "contacts")
v_swapper    = makeVC "swapper"    (nounPhrase "swapper")    (Atomic "swapper")
v_stamp      = makeVC "stamp"      (nounPhrase "stamp")      (Atomic "stamp")
v_state      = makeVC "state"      (nounPhrase "state")      (Atomic "state")
v_points     = makeVC "points"     (nounPhrase "points")     (Atomic "points")
v_arb        = makeVC "arb"        (nounPhrase "arb")        (Atomic "arb")

--arbiterGetCount, arbiterGetNormal, arbiterIsFirstContact :: funcDef

arbiterGetCount = funcDef "arbiterGetCount" [arb] Rational
  [
    FRet (FCall (asExpr getCount) [arb])
  ]

arbiterGetNormal = funcDef "arbiterGetNormal" [arb] Rational
  [
    FRet (FCall (asExpr getNormal) [arb])
  ]