module Drasil.GamePhysics.DesignDec.Vector where

import Language.Drasil

v_v1, v_v2, v_x, v_y, v_v, v_s, v_length, v_dist, v_DBL_MIN, v_v1_x,
  v_v1_y, v_v2_x, v_v2_y, v_rad :: VarChunk

v_v1      = makeVC "v1"      (nounPhrase "v1")      (sub (lV) (Atomic "1"))
v_v2      = makeVC "v2"      (nounPhrase "v2")      (sub (lV) (Atomic "2"))
v_x       = makeVC "x"       (nounPhrase "x")       (lX)
v_y       = makeVC "y"       (nounPhrase "y")       (lY)
v_v       = makeVC "v"       (nounPhrase "v")       (lV)
v_s       = makeVC "s"       (nounPhrase "s")       (lS)
v_length  = makeVC "length"  (nounPhrase "length")  (Atomic "length")
v_dist    = makeVC "dist"    (nounPhrase "dist")    (Atomic "dist")
v_DBL_MIN = makeVC "DBL_MIN" (nounPhrase "DBL_MIN") (Atomic "DBL_MIN")
v_v1_x    = makeVC "v1_x"    (nounPhrase "v1_x")    (Atomic "v1_x")
v_v1_y    = makeVC "v1_y"    (nounPhrase "v1_y")    (Atomic "v1_y")
v_v2_x    = makeVC "v2_x"    (nounPhrase "v2_x")    (Atomic "v2_x")
v_v2_y    = makeVC "v2_y"    (nounPhrase "v2_y")    (Atomic "v2_y")
v_rad     = makeVC "v_rad"   (nounPhrase "rad" )    (Atomic "rad") 

vectEqual, vectAdd, vectSub, vectMult, vectNeg, vectDot, vectCross, vectPerp,
  vectRPerp, vectProject, vectForAngle, vectToAngle, vectRotate, vectUnrotate,
  vectLengthSq, vectLength, vectNormalize, vectClamp, vectLerp, vectDistSq, 
  vectDist, vectNear, getX, getY, vect :: FuncDef

vectEqual = funcDef "vectEqual" [v_v1, v_v2] Boolean
  [
    FRet (FCond ((FCall (asExpr getX) [v_v1]) := (FCall (asExpr getX) [v_v2])) :&&
                ((FCall (asExpr getY) [v_v1]) := (FCall (asExpr getY) [v_v2]))
         )
  ]

vectAdd = funcDef "vectAdd" [v_v1, v_v2] (Vect Rational)
  [
    FRet (FCall (asExpr vect) 
      [
        ((FCall (asExpr getX) [v_v1]) + (FCall (asExpr getX) [v_v2])),
        ((FCall (asExpr getY) [v_v1]) + (FCall (asExpr getY) [v_v2]))
      ]
    )
  ]
  
vectSub = funcDef "vectSub" [v_v1, v_v2] (Vect Rational)
  [
    FRet (FCall (asExpr vect)
      [
        ((FCall (asExpr getX) [v_v1]) - (FCall (asExpr getX) [v_v2])),
        ((FCall (asExpr getY) [v_v1]) - (FCall (asExpr getY) [v_v2]))
      ]
    )
  ]
  
vectMult = funcDef "vectMult" [v_v1, v_v2] (Vect Rational)
  [
    FRet (FCall (asExpr vect)
      [
        ((FCall (asExpr getX) [v_v1]) * (FCall (asExpr getX) [v_v2])),
        ((FCall (asExpr getY) [v_v1]) * (FCall (asExpr getY) [v_v2]))
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

vectDot = funcDef "vectDot" [v_v1, v_v2] Rational
  [
    FRet (
      (FCall (asExpr getX) [v_v1]) *
      (FCall (asExpr getX) [v_v2]) +
      (FCall (asExpr getY) [v_v1]) *
      (FCall (asExpr getY) [v_v2])
    )
  ]

vectCross = funcDef "vectCross" [v_v1, v_v2] Rational
  [
    FRet (
      ((FCall (asExpr getX) [v_v1]) *
      (FCall (asExpr getY) [v_v2])) -
      ((FCall (asExpr getY) [v_v1]) *
      (FCall (asExpr getX) [v_v2])) 
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
   
vectProject = funcDef "vectProject" [v_v1, v_v2] (Vect Rational)   
  [ 
    FRet (FCall (asExpr vectMult) 
      [
        v_v2,
        ((FCall (asExpr vectDot) [v_v1, v_v2]) / (FCall (asExpr vectDot) [v_v2, v_v2]))
      ]
    )
  ]

vectForAngle = funcDef "vectForAngle" [rad] (Vect Rational)
  [
    FRet (FCall (asExpr vect)
      [
        (Cos rad]),
        (Sin rad)
      ]
    )
  ]

vectToAngle = funcDef "vectToAngle" [v_v] Radians
  [
    FRet (FCall (asExpr atan2) 
      [
        (FCall (asExpr getY) [v_v]),
        (FCall (asExpr getX) [v_v]
      ]
    )
  ]

vectRotate = funcDef "vectRotate" [v_v1, v_v2] (Vect Rational)
  [
    FRet (FCall (asExpr vect)
      [
        ((FCall (asExpr getX) [v_v1]) * (FCall (asExpr getX) [v_v2])) -
        ((FCall (asExpr getY) [v_v1]) * (FCall (asExpr getY) [v_v2])),
        ((FCall (asExpr getX) [v_v1]) * (FCall (asExpr getY) [v_v2])) +
        ((FCall (asExpr getY) [v_v1]) * (FCall (asExpr getX) [v_v2]))
      ]
    )
  ]
  
vectUnrotate = funcDef "vectUnrotate" [v_v1, v_v2] (Vect Rational)
  [
    FRet (FCall (asExpr vect)
      [
        ((FCall (asExpr getX) [v_v1]) * (FCall (asExpr getX) [v_v2])) +
        ((FCall (asExpr getY) [v_v1]) * (FCall (asExpr getY) [v_v2])),
        ((FCall (asExpr getY) [v_v1]) * (FCall (asExpr getX) [v_v2])) -
        ((FCall (asExpr getX) [v_v1]) * (FCall (asExpr getY) [v_v2]))
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
        [FCall (asExpr vectNormalize) [v_v], length]
      ] 
  ]

vectLerp = funcDef "vectLerp" [v_v1, v_v2, t] (Vect Rational)
  [
    FRet (FCall (asExpr vectAdd)
      [
        (FCall (asExpr vectMult) [(v_v1), (1.0 - t)]),
        (FCall (asExpr vectMult) [(v_v2), t])
      ]
  ]

vectNormalize = funcDef "vectNormalize" [v_v] (Vect Rational)
  [
    FRet (FCall (asExpr vectMult) 
      [v_v, (1.0 / (FCall (asExpr vectLength) [v_v]) + DBL_MIN)]
    )
  ]

vectDistSq = funcDef "vectDistSq" [v_v1, v_v2] Rational
  [
    FRet (FCall (asExpr vectLengthSq) [(FCall (asExpr vectSub) [v_v1, v_v2]]))
  ]

vectDist = funcDef "vectDist" [v_v1, v_v2] Rational
  [
    FRet (Sqrt (FCall (asExpr vectDistSq) [v_v1, v_v2]))
  ]


vectNear = funcDef "vectNear" [v_v1, v_v2, dist] Boolean
  [
    FRet ((FCall (asExpr vectDist) [v_v1, v_v2]) :< dist) 
  ]

DBL_MIN = fasg v_DBL_MIN (2.2250738585072014e-308) 

asExpr :: FuncDef -> Expr
asExpr (FuncDef n _ _ _) = C $ makeVC n (nounPhraseSP n) (Atomic n)
