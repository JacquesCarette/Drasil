module Drasil.GamePhysics.DesignDec.Vector where

import Language.Drasil

vV1, vV2, vX, vY, vV, vS, vLength, vDist, vDblMin, vV1X,
  vV1Y, vV2X, vV2Y, vRad :: VarChunk

vV1      = makeVC "v1"      (nounPhrase "v1")      (sub (lV) (Atomic "1"))
vV2      = makeVC "v2"      (nounPhrase "v2")      (sub (lV) (Atomic "2"))
vX       = makeVC "x"       (nounPhrase "x")       (lX)
vY       = makeVC "y"       (nounPhrase "y")       (lY)
vV       = makeVC "v"       (nounPhrase "v")       (lV)
vS       = makeVC "s"       (nounPhrase "s")       (lS)
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
    FRet (FCond ((FCall (asExpr getX) [vV1]) := (FCall (asExpr getX) [vV2])) :&&
                ((FCall (asExpr getY) [vV1]) := (FCall (asExpr getY) [vV2]))
         )
  ]

vectAdd = funcDef "vectAdd" [vV1, vV2] (Vect Rational)
  [
    FRet (FCall (asExpr vect) 
      [
        ((FCall (asExpr getX) [vV1]) + (FCall (asExpr getX) [vV2])),
        ((FCall (asExpr getY) [vV1]) + (FCall (asExpr getY) [vV2]))
      ]
    )
  ]
  
vectSub = funcDef "vectSub" [vV1, vV2] (Vect Rational)
  [
    FRet (FCall (asExpr vect)
      [
        ((FCall (asExpr getX) [vV1]) - (FCall (asExpr getX) [vV2])),
        ((FCall (asExpr getY) [vV1]) - (FCall (asExpr getY) [vV2]))
      ]
    )
  ]
  
vectMult = funcDef "vectMult" [vV1, vV2] (Vect Rational)
  [
    FRet (FCall (asExpr vect)
      [
        ((FCall (asExpr getX) [vV1]) * (FCall (asExpr getX) [vV2])),
        ((FCall (asExpr getY) [vV1]) * (FCall (asExpr getY) [vV2]))
      ]
    )
  ]

vectNeg = funcDef "vectNeg" [v_v] (Vect Rational)
  [
    FRet (FCall (asExpr vect)
      $ map Neg [
        (FCall (asExpr getX) [v_v]),
        (FCall (asExpr getY) [v_v])
      ]
    )
  ]

vectDot = funcDef "vectDot" [vV1, vV2] Rational
  [
    FRet (
      (FCall (asExpr getX) [vV1]) *
      (FCall (asExpr getX) [vV2]) +
      (FCall (asExpr getY) [vV1]) *
      (FCall (asExpr getY) [vV2])
    )
  ]

vectCross = funcDef "vectCross" [vV1, vV2] Rational
  [
    FRet (
      ((FCall (asExpr getX) [vV1]) *
      (FCall (asExpr getY) [vV2])) -
      ((FCall (asExpr getY) [vV1]) *
      (FCall (asExpr getX) [vV2])) 
    )
  ]
  
vectPerp = funcDef "vectPerp" [v_v] (Vect Rational)
  [
    FRet (FCall (asExpr vect) 
      [
        (Neg (FCall (asExpr getY) [v_v])),
        (FCall (asExpr getX) [v_v])
      ]
    )
  ]

vectRPerp = funcDef "vectRPerp" [v_v] (Vect Rational)
  [
    FRet (FCall (asExpr vect)
      [
        (FCall (asExpr getY) [v_v]),
        (Neg (FCall (asExpr getX) [v_v]))
      ]
    )
  ]
   
vectProject = funcDef "vectProject" [vV1, vV2] (Vect Rational)   
  [ 
    FRet (FCall (asExpr vectMult) 
      [
        vV2,
        ((FCall (asExpr vectDot) [vV1, vV2]) / (FCall (asExpr vectDot) [vV2, vV2]))
      ]
    )
  ]

vectForAngle = funcDef "vectForAngle" [rad] (Vect Rational)
  [
    FRet (FCall (asExpr vect)
      [
        Cos rad,
        Sin rad
      ]
    )
  ]

vectToAngle = funcDef "vectToAngle" [v_v] Radians
  [
    FRet (FCall (asExpr atan2) 
      [
        FCall (asExpr getY) [v_v],
        FCall (asExpr getX) [v_v]
      ]
    )
  ]

vectRotate = funcDef "vectRotate" [vV1, vV2] (Vect Rational)
  [
    FRet (FCall (asExpr vect)
      [
        ((FCall (asExpr getX) [vV1]) * (FCall (asExpr getX) [vV2])) -
        ((FCall (asExpr getY) [vV1]) * (FCall (asExpr getY) [vV2])),
        ((FCall (asExpr getX) [vV1]) * (FCall (asExpr getY) [vV2])) +
        ((FCall (asExpr getY) [vV1]) * (FCall (asExpr getX) [vV2]))
      ]
    )
  ]
  
vectUnrotate = funcDef "vectUnrotate" [vV1, vV2] (Vect Rational)
  [
    FRet (FCall (asExpr vect)
      [
        ((FCall (asExpr getX) [vV1]) * (FCall (asExpr getX) [vV2])) +
        ((FCall (asExpr getY) [vV1]) * (FCall (asExpr getY) [vV2])),
        ((FCall (asExpr getY) [vV1]) * (FCall (asExpr getX) [vV2])) -
        ((FCall (asExpr getX) [vV1]) * (FCall (asExpr getY) [vV2]))
      ]
    )
  ]
  
vectLengthSq = funcDef "vectLengthSq" [v_v] Rational
  [
    FRet (FCall (asExpr vectDot) [v_v, v_v])
  ]
  
vectLength = funcDef "vectLength" [v_v] Rational
  [
    FRet (Sqrt (FCall (asExpr vectLengthSq) [v_v]))
  ]
  
vectClamp = funcDef "vectClamp" [v_v, length] (Vect Rational)
  [
    (FCond (FCall (asExpr vectLength) [v_v]) :< length) 
      [FRet v_v]
      [FRet (FCall (asExpr vectMult) 
        [FCall (asExpr vectNormalize) [v_v], length])
      ]
  ]

vectLerp = funcDef "vectLerp" [vV1, vV2, t] (Vect Rational)
  [
    FRet (FCall (asExpr vectAdd)
      [
        (FCall (asExpr vectMult) [(vV1), (1.0 - t)]),
        (FCall (asExpr vectMult) [(vV2), t])
      ])
  ]

vectNormalize = funcDef "vectNormalize" [v_v] (Vect Rational)
  [
    FRet (FCall (asExpr vectMult) 
      [v_v, (1.0 / (FCall (asExpr vectLength) [v_v]) + DBL_MIN)]
    )
  ]

vectDistSq = funcDef "vectDistSq" [vV1, vV2] Rational
  [
    FRet (FCall (asExpr vectLengthSq) [FCall (asExpr vectSub) [vV1, vV2]])
  ]

vectDist = funcDef "vectDist" [vV1, vV2] Rational
  [
    FRet (Sqrt (FCall (asExpr vectDistSq) [vV1, vV2]))
  ]


vectNear = funcDef "vectNear" [vV1, vV2, dist] Boolean
  [
    FRet ((FCall (asExpr vectDist) [vV1, vV2]) :< dist) 
  ]

DBL_MIN = fasg v_DBL_MIN (2.2250738585072014e-308) 

asExpr :: FuncDef -> Expr
asExpr (FuncDef n _ _ _) = C $ makeVC n (nounPhraseSP n) (Atomic n)
