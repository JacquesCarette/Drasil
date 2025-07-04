module Drasil.GamePhysics.DesignDec.Vector where

import Language.Drasil

vV1, vV2, vX, vY, vV, vS, vLength, vDist, vDblMin, vV1X,
  vV1Y, vV2X, vV2Y, vRad :: VarChunk

vV1      = makeVC "v1"      (nounPhrase "v1")      (sub lV (Atomic "1"))
vV2      = makeVC "v2"      (nounPhrase "v2")      (sub lV (Atomic "2"))
vX       = makeVC "x"       (nounPhrase "x")       lX
vY       = makeVC "y"       (nounPhrase "y")       lY
vV       = makeVC "v"       (nounPhrase "v")       lV
vS       = makeVC "s"       (nounPhrase "s")       lS
vLength  = makeVC "length"  (nounPhrase "length")  (Atomic "length")
vDist    = makeVC "dist"    (nounPhrase "dist")    (Atomic "dist")
vDblMin  = makeVC "DblMin"  (nounPhrase "DblMin")  (Atomic "DblMin")
vV1X     = makeVC "v1X"     (nounPhrase "v1X")     (Atomic "v1X")
vV1Y     = makeVC "v1Y"     (nounPhrase "v1Y")     (Atomic "v1Y")
vV2X     = makeVC "v2X"     (nounPhrase "v2X")     (Atomic "v2X")
vV2Y     = makeVC "v2Y"     (nounPhrase "v2Y")     (Atomic "v2Y")
vRad     = makeVC "vRad"    (nounPhrase "rad" )    (Atomic "rad") 

vectEqual, vectAdd, vectSub, vectMult, vectNeg, vectDot, vectCross, vectPerp,
  vectRPerp, vectProject, vectForAngle, vectToAngle, vectRotate, vectUnrotate,
  vectLengthSq, vectLength, vectNormalize, vectClamp, vectLerp, vectDistSq, 
  vectDist, vectNear, getX, getY, vect :: FuncDef

vectEqual = funcDef "vectEqual" [vV1, vV2] Boolean
  [
    FRet (FCond (FCall (funcUID getX) [vV1] := FCall (funcUID getX) [vV2]) :&&
                (FCall (funcUID getY) [vV1] := FCall (funcUID getY) [vV2])
         )
  ]

vectAdd = funcDef "vectAdd" [vV1, vV2] (vectNDS dim Real)
  [
    FRet (FCall (funcUID vect) 
      [
        FCall (funcUID getX) [vV1] + FCall (funcUID getX) [vV2],
        FCall (funcUID getY) [vV1] + FCall (funcUID getY) [vV2]
      ]
    )
  ]
  
vectSub = funcDef "vectSub" [vV1, vV2] (vectNDS dim Real)
  [
    FRet (FCall (funcUID vect)
      [
        FCall (funcUID getX) [vV1] - FCall (funcUID getX) [vV2],
        FCall (funcUID getY) [vV1] - FCall (funcUID getY) [vV2]
      ]
    )
  ]
  
vectMult = funcDef "vectMult" [vV1, vV2] (vectNDS dim Real)
  [
    FRet (FCall (funcUID vect)
      [
        FCall (funcUID getX) [vV1] * FCall (funcUID getX) [vV2],
        FCall (funcUID getY) [vV1] * FCall (funcUID getY) [vV2]
      ]
    )
  ]

vectNeg = funcDef "vectNeg" [v_v] (vectNDS dim Real)
  [
    FRet (FCall (funcUID vect)
      $ map Neg [
        FCall (funcUID getX) [v_v],
        FCall (funcUID getY) [v_v]
      ]
    )
  ]

vectDot = funcDef "vectDot" [vV1, vV2] Real
  [
    FRet (
      FCall (funcUID getX) [vV1] *
      FCall (funcUID getX) [vV2] +
      FCall (funcUID getY) [vV1] *
      FCall (funcUID getY) [vV2]
    )
  ]

vectCross = funcDef "vectCross" [vV1, vV2] Real
  [
    FRet (
      (FCall (funcUID getX) [vV1] *
      FCall (funcUID getY) [vV2]) -
      (FCall (funcUID getY) [vV1] *
      FCall (funcUID getX) [vV2]) 
    )
  ]
  
vectPerp = funcDef "vectPerp" [v_v] (vectNDS dim Real)
  [
    FRet (FCall (funcUID vect) 
      [
        Neg (FCall (funcUID getY) [v_v]),
        FCall (funcUID getX) [v_v]
      ]
    )
  ]

vectRPerp = funcDef "vectRPerp" [v_v] (vectNDS dim Real)
  [
    FRet (FCall (funcUID vect)
      [
        FCall (funcUID getY) [v_v],
        Neg (FCall (funcUID getX) [v_v])
      ]
    )
  ]
   
vectProject = funcDef "vectProject" [vV1, vV2] (vectNDS dim Real)   
  [ 
    FRet (FCall (funcUID vectMult) 
      [
        vV2,
        FCall (funcUID vectDot) [vV1, vV2] / FCall (funcUID vectDot) [vV2, vV2]
      ]
    )
  ]

vectForAngle = funcDef "vectForAngle" [rad] (vectNDS dim Real)
  [
    FRet (FCall (funcUID vect)
      [
        Cos rad,
        Sin rad
      ]
    )
  ]

vectToAngle = funcDef "vectToAngle" [v_v] Real
  [
    FRet (FCall (funcUID atan2)
      [
        FCall (funcUID getY) [v_v],
        FCall (funcUID getX) [v_v]
      ]
    )
  ]

vectRotate = funcDef "vectRotate" [vV1, vV2] (vectNDS dim Real)
  [
    FRet (FCall (funcUID vect)
      [
        FCall (funcUID getX) [vV1] * FCall (funcUID getX) [vV2] -
        FCall (funcUID getY) [vV1] * FCall (funcUID getY) [vV2],
        FCall (funcUID getX) [vV1] * FCall (funcUID getY) [vV2] +
        FCall (funcUID getY) [vV1] * FCall (funcUID getX) [vV2]
      ]
    )
  ]
  
vectUnrotate = funcDef "vectUnrotate" [vV1, vV2] (vectNDS dim Real)
  [
    FRet (FCall (funcUID vect)
      [
        FCall (funcUID getX) [vV1] * FCall (funcUID getX) [vV2] +
        FCall (funcUID getY) [vV1] * FCall (funcUID getY) [vV2],
        FCall (funcUID getY) [vV1] * FCall (funcUID getX) [vV2] -
        FCall (funcUID getX) [vV1] * FCall (funcUID getY) [vV2]
      ]
    )
  ]
  
vectLengthSq = funcDef "vectLengthSq" [v_v] Real
  [
    FRet (FCall (funcUID vectDot) [v_v, v_v])
  ]
  
vectLength = funcDef "vectLength" [v_v] Real
  [
    FRet (Sqrt (FCall (funcUID vectLengthSq) [v_v]))
  ]
  
vectClamp = funcDef "vectClamp" [v_v, length] (vectNDS dim Real)
  [
    (FCond (FCall (funcUID vectLength) [v_v]) :< length) 
      [FRet v_v]
      [FRet (FCall (funcUID vectMult) 
        [FCall (funcUID vectNormalize) [v_v], length])
      ]
  ]

vectLerp = funcDef "vectLerp" [vV1, vV2, t] (vectNDS dim Real)
  [
    FRet (FCall (funcUID vectAdd)
      [
        FCall (funcUID vectMult) [vV1, 1.0 - t],
        FCall (funcUID vectMult) [vV2, t]
      ])
  ]

vectNormalize = funcDef "vectNormalize" [v_v] (vectNDS dim Real)
  [
    FRet (FCall (funcUID vectMult) 
      [v_v, 1.0 / FCall (funcUID vectLength) [v_v] + DBL_MIN]
    )
  ]

vectDistSq = funcDef "vectDistSq" [vV1, vV2] Real
  [
    FRet (FCall (funcUID vectLengthSq) [FCall (funcUID vectSub) [vV1, vV2]])
  ]

vectDist = funcDef "vectDist" [vV1, vV2] Real
  [
    FRet (Sqrt (FCall (funcUID vectDistSq) [vV1, vV2]))
  ]


vectNear = funcDef "vectNear" [vV1, vV2, dist] Boolean
  [
    FRet (FCall (funcUID vectDist) [vV1, vV2] :< dist) 
  ]

DBL_MIN = fasg v_DBL_MIN 2.2250738585072014e-308

funcUID :: FuncDef -> Expr
funcUID (FuncDef n _ _ _) = C $ makeVC n (nounPhraseSP n) (Atomic n)
