This page is for keeping track of the instances of the main `UnitalChunk` constructors, as discussed in [#3199](https://github.com/JacquesCarette/Drasil/issues/3199).

# Summary of `UnitalChunk` constructors
## From `Concept`
- `uc`: 84
- `ucs'`: 69
- **Total**: 153
## From Components
- `uc'`: 41
- `ucStaged`: 4
- `ucs`: 11
- `ucsWS`: 16
- `makeUCWDS`: 44
- `cuc'`: 5
- `uqc`: 24
- `uqcND`: 3
- **Total**: 148

```haskell
-- from drasil-lang
-- Chunk.Constrained
cuc' :: (IsUnit u) => String -> NP -> String -> Symbol -> u
            -> Space -> [ConstraintE] -> Expr -> ConstrConcept
cuc' nam trm desc sym un space cs rv =
  ConstrConcept (dqd (cw (ucs nam trm desc sym space un)) sym space uu) cs (Just rv)
  where uu = unitWrapper un

-- Chunk.UncertainQuantity
uqc :: (IsUnit u) => String -> NP -> String -> Symbol -> u -> Space
                -> [ConstraintE] -> Expr -> Uncertainty -> UncertQ
uqc nam trm desc sym un space cs val = uq (cuc' nam trm desc sym un space cs val)

uqcND :: (IsUnit u) => String -> NP -> Symbol -> u -> Space -> [ConstraintE]
                  -> Expr -> Uncertainty -> UncertQ
uqcND nam trm sym un space cs val = uq (cuc' nam trm "" sym un space cs val)

-- from drasil-data
-- Quantity.Math
area        = ucs' CM.area     cA   Real m_2
diameter    = ucs' CM.diameter lD   Real metre
surface     = ucs' CM.surface  cS   Real m_2
surArea     = ucs' CM.surArea  cA   Real m_2
orientation = ucs' CM.orient   lPhi Real radian

-- Quantity.PhysicalProperties
density    = uc CPP.density    lRho   densityU
specWeight = uc CPP.specWeight lGamma specificWeight
mass       = uc CPP.mass       lM     kilogram
len        = uc CPP.len        cL     metre
vol        = uc CPP.vol        cV     m_3

-- Quantity.Physics
acceleration         = ucs' CP.acceleration (vec lA) (Vect Real) accelU
angularAccel         = uc CP.angAccel lAlpha angAccelU
angularDisplacement  = uc CP.angDisp lTheta radian
angularFrequency     = uc CP.angFreq cOmega second
angularVelocity      = uc CP.angVelo lOmega angVelU
chgInVelocity        = uc CP.chgInVelocity (Atop Delta $ vec lV) velU
constAccel           = uc CP.constAccel (sup lA lC) accelU
displacement         = uc CP.displacement (vec lU) metre
distance             = uc CP.distance lD metre
energy               = uc CP.energy cE joule
force                = uc CP.force (vec cF) newton
frequency            = uc CP.frequency lF hertz
gravitationalAccel   = uc CP.gravitationalAccel (vec lG) accelU
gravitationalConst   = uc CP.gravitationalConst cG gravConstU
height               = uc CP.height lH metre
impulseS             = uc CP.impulseS lJ impulseU
impulseV             = uc CP.impulseV (vec cJ) impulseU
kEnergy              = uc CP.kEnergy  (Concat [cK, cE]) joule
linearAccel          = uc CP.linAccel (Concat [vec lA, label "(", lT, label ")"]) accelU
linearDisplacement   = uc CP.linDisp  (Concat [vec lU, label "(", lT, label ")"]) metre
linearVelocity       = uc CP.linVelo  (Concat [vec lV, label "(", lT, label ")"]) velU
momentOfInertia      = uc CP.momentOfInertia (vec cI) momtInertU
chgMomentum          = uc CP.chgMomentum (Concat [cDelta,vec cP]) impulseU
momentum             = uc CP.momentum (vec cP) impulseU
moment               = uc CP.moment   (vec cM) torqueU
moment2D             = uc CP.moment   cM       torqueU
period               = uc CP.period cT second
position             = uc CP.position (vec lP) metre
positionVec          = uc CP.positionVec (vec lR) metre
potEnergy            = uc CP.potEnergy (Concat [cP, cE]) joule
pressure             = uc CP.pressure lP pascal
speed                = uc CP.speed lV velU
scalarAccel          = uc CP.scalarAccel lA accelU
scalarPos            = uc CP.scalarPos lP metre
tension              = uc CP.tension (vec cT) newton
time                 = uc CP.time lT second
torque               = uc CP.torque (vec lTau) torqueU
velocity             = ucs' CP.velocity (vec lV) (Vect Real) velU
weight               = uc CP.weight cW newton
fOfGravity           = uc CP.fOfGravity (sub (vec cF) (vec lG)) newton
xDist = uc CP.xDist (subX lR) metre
yDist = uc CP.yDist (subY lR) metre
iPos = uc CP.iPos (sup lP initial) metre
xPos = uc CP.xPos (subX lP) metre
yPos = uc CP.yPos (subY lP) metre
ixPos = uc CP.ixPos (sup (subX lP) initial) metre
iyPos = uc CP.iyPos (sup (subY lP) initial) metre
fSpeed = uc CP.fSpeed (sup lV final) velU
iSpeed = uc CP.iSpeed (sup lV initial) velU
ixSpeed = uc CP.ixSpeed (sub lU initial) velU
iySpeed = uc CP.iySpeed (sub lW initial) velU
fVel = uc CP.fVel (sup (vec lV) final) velU
iVel = uc CP.iVel (sup (vec lV) initial) velU
xVel = uc CP.xVel (subX lV) velU
yVel = uc CP.yVel (subY lV) velU
ixVel = uc CP.ixVel (sup (subX lV) initial) velU
iyVel = uc CP.iyVel (sup (subY lV) initial) velU
xAccel = uc CP.xAccel (subX lA) accelU
yAccel = uc CP.yAccel (subY lA) accelU
constAccelV = uc CP.constAccelV (sup (vec  lA) constant) accelU
xConstAccel = uc CP.xConstAccel (sup (subX lA) constant) accelU
yConstAccel = uc CP.yConstAccel (sup (subY lA) constant) accelU

-- Quantities.SolidMechanics
elastMod = uc CSM.elastMod cE pascal 
mobShear = uc CSM.mobShear cS newton 
shearRes = uc CSM.shearRes cP newton 
stffness = uc CSM.stffness cK stiffnessU 
nrmStrss = uc CSM.nrmStrss lSigma pascal

-- Quantities.Thermodynamics
boilPt        = uc CT.boilPt (sub cT (label "boil")) centigrade
temp          = uc CT.temp cT centigrade
heatCapSpec   = uc CT.heatCapSpec cC UT.heatCapSpec
htFlux        = uc CT.htFlux lQ UT.thermalFlux
latentHeat    = uc CT.latentHeat cQ joule
meltPt        = uc CT.meltPt (sub cT (label "melt")) centigrade
sensHeat      = uc CT.sensHeat cE joule

-- from drasil-example
-- DblPendulum.Unitals
lenRod_1 = makeUCWDS "l_1" (nounPhraseSent $ phraseNP(len `the_ofThe` firstRod))
        (S "The" +:+ phraseNP (len `the_ofThe` firstRod)) -- Fix me, can have more information 
        (sub cL label1) metre

lenRod_2 = makeUCWDS "l_2" (nounPhraseSent $ phraseNP(len `the_ofThe` secondRod))
        (S "The" +:+ phraseNP (len `the_ofThe` secondRod))
        (sub cL label2) metre

massObj_1 = makeUCWDS "m_1" (nounPhraseSent $ phraseNP (mass `the_ofThe` firstObject))
        (S "The" +:+ phraseNP (mass `the_ofThe` firstObject))
        (sub lM label1) kilogram

massObj_2 = makeUCWDS "m_2" (nounPhraseSent $ phraseNP (mass `the_ofThe` secondObject))
        (S "The" +:+ phraseNP (mass `the_ofThe` secondObject))
        (sub lM label2) kilogram

xPos_1 = makeUCWDS "p_x1" (nounPhraseSent $ phraseNP (horizontalPos `the_ofThe` firstObject))
        (S "The" +:+ phraseNP (QP.position `the_ofThe` firstObject) `S.inThe` phrase CM.xDir)
        (sub lP (Concat [labelx, label1])) metre

xPos_2 = makeUCWDS "p_x2" (nounPhraseSent $ phraseNP (horizontalPos `the_ofThe` secondObject))
        (S "The" +:+ phraseNP (QP.position `the_ofThe` secondObject) `S.inThe` phrase CM.xDir)
        (sub lP (Concat [labelx, label2])) metre

yPos_1 = makeUCWDS "p_y1" (nounPhraseSent $ phraseNP (verticalPos `the_ofThe` firstObject))
        (S "The" +:+ phraseNP (QP.position `the_ofThe` firstObject) `S.inThe` phrase CM.yDir)
        (sub lP (Concat [labely, label1])) metre

yPos_2 = makeUCWDS "p_y2" (nounPhraseSent $ phraseNP (verticalPos `the_ofThe` secondObject))
        (S "The" +:+ phraseNP (QP.position `the_ofThe` secondObject) `S.inThe` phrase CM.yDir)
        (sub lP (Concat [labely, label2])) metre

xVel_1 = makeUCWDS "v_x1" (nounPhraseSent $ phraseNP (horizontalVel `the_ofThe` firstObject))
        (S "The" +:+ phraseNP (QP.angularVelocity `the_ofThe` firstObject) `S.inThe` phrase CM.xDir)
        (sub lV (Concat [labelx, label1])) velU

xVel_2 = makeUCWDS "v_x2" (nounPhraseSent $ phraseNP (horizontalVel `the_ofThe` secondObject))
        (S "The" +:+ phraseNP (QP.angularVelocity `the_ofThe` secondObject) `S.inThe` phrase CM.xDir)
        (sub lV (Concat [labelx, label2])) velU

yVel_1 = makeUCWDS "v_y1" (nounPhraseSent $ phraseNP (verticalVel `the_ofThe` firstObject))
        (S "The" +:+ phraseNP (QP.angularVelocity `the_ofThe` firstObject) `S.inThe` phrase CM.yDir)
        (sub lV (Concat [labely, label1])) velU

yVel_2 = makeUCWDS "v_y2" (nounPhraseSent $ phraseNP (verticalVel `the_ofThe` secondObject))
        (S "The" +:+ phraseNP (QP.angularVelocity `the_ofThe` secondObject) `S.inThe` phrase CM.yDir)
        (sub lV (Concat [labely, label2])) velU

xAccel_1 = makeUCWDS "a_x1" (nounPhraseSent $ phraseNP (horizontalAccel `the_ofThe` firstObject))
        (S "The" +:+ phraseNP (QP.acceleration `the_ofThe` firstObject) `S.inThe` phrase CM.xDir)
        (sub lA (Concat [labelx, label1])) accelU

xAccel_2 = makeUCWDS "a_x2" (nounPhraseSent $ phraseNP (horizontalAccel `the_ofThe` secondObject))
        (S "The" +:+ phraseNP (QP.acceleration `the_ofThe` secondObject) `S.inThe` phrase CM.xDir)
        (sub lA (Concat [labelx, label2])) accelU

yAccel_1 = makeUCWDS "a_y1" (nounPhraseSent $ phraseNP (verticalAccel `the_ofThe` firstObject))
        (S "The" +:+ phraseNP (QP.acceleration `the_ofThe` firstObject) `S.inThe` phrase CM.yDir)
        (sub lA (Concat [labely, label1])) accelU

yAccel_2 = makeUCWDS "a_y2" (nounPhraseSent $ phraseNP (verticalAccel `the_ofThe` secondObject))
        (S "The" +:+ phraseNP (QP.acceleration `the_ofThe` secondObject) `S.inThe` phrase CM.yDir)
        (sub lA (Concat [labely, label2])) accelU

angularAccel_1 = makeUCWDS "alpha_x1" (nounPhraseSent $ phraseNP (QP.angularAccel `the_ofThe` firstObject))
        (S "The" +:+ phraseNP (QP.angularAccel `the_ofThe` firstObject) `S.inThe` phrase CM.xDir)
        (sub lAlpha label1) angAccelU

angularAccel_2 = makeUCWDS "alpha_y1" (nounPhraseSent $ phraseNP (QP.angularAccel `the_ofThe` secondObject))
        (S "The" +:+ phraseNP (QP.angularAccel `the_ofThe` secondObject) `S.inThe` phrase CM.yDir)
        (sub lAlpha label2) angAccelU

tension_1 = makeUCWDS "T_1" (nounPhraseSent $ phraseNP (QP.tension `the_ofThe` firstObject))
        (S "The" +:+ phraseNP (QP.tension `the_ofThe` firstObject))
        (sub (vec cT) label1) newton

tension_2 = makeUCWDS "T_2" (nounPhraseSent $ phraseNP (QP.tension `the_ofThe` secondObject))
        (S "The" +:+ phraseNP (QP.tension `the_ofThe` secondObject))
        (sub (vec cT) label2) newton

angularVel_1 = makeUCWDS "w_1" (nounPhraseSent $ phraseNP (QP.angularVelocity `the_ofThe` firstObject))
        (S "The" +:+ phraseNP (QP.angularVelocity `the_ofThe` firstObject))
        (sub lW label1) angVelU

angularVel_2 = makeUCWDS "w_2" (nounPhraseSent $ phraseNP (QP.angularVelocity `the_ofThe` secondObject))
        (S "The" +:+ phraseNP (QP.angularVelocity `the_ofThe` secondObject))
        (sub lW label2) angVelU

pendDisAngle_1 = makeUCWDS "theta_1" (nounPhraseSent $ phraseNP (angle `the_ofThe` firstRod))
        (S "The" +:+ phraseNP (angle `the_ofThe` firstRod))
        (sub lTheta label1) radian

pendDisAngle_2 = makeUCWDS "theta_2" (nounPhraseSent $ phraseNP (angle `the_ofThe` secondRod))
        (S "The" +:+ phraseNP (angle `the_ofThe` secondRod))
        (sub lTheta label2) radian

pendDisAngle = cuc' "pendDisAngle"
  (nounPhraseSP "dependent variables")
  "Column vector of displacement of rods with its derivatives" 
  lTheta' radian (Vect Real)
  [physc $ UpFrom (Inc, exactDbl 0)] (exactDbl 0)

-- GamePhysics.Unitals
forceParam, massParam, timeParam :: String -> String -> Symbol -> UnitalChunk
forceParam n w s = ucs'
 (dccWDS ("force" ++ n) (cn $ "force exerted by the " ++ w ++ 
  " body (on another body)") (phrase QP.force)) 
  (sub (eqSymb QP.force) s) Real newton

massParam n w s = ucs'
 (dccWDS ("mass" ++ n) (cn $ "mass of the " ++ w ++ " body") 
  (phrase QPP.mass)) (sub (eqSymb QPP.mass) s) Real kilogram

timeParam n w s = ucs'
 (dccWDS ("time" ++ n) (cn $ "time at a point in " ++ w ++ " body ") 
  (phrase QP.time)) (sub (eqSymb QP.time) s) Real second

contParam :: String -> String -> Symbol -> Symbol -> UnitalChunk
contParam n m w s = ucs'
 (dccWDS ("r_" ++ n ++ m) contdispN (phrase QP.displacement))
  (sub (eqSymb QP.displacement) (Concat [w, s])) Real metre
  where contdispN = cn $ "displacement vector between the centre of mass of rigid body " ++
                         n ++ " and contact point " ++ m

angParam, momtParam, perpParam, rigidParam, velBodyParam, velParam :: String -> Symbol -> UnitalChunk
angParam n w = ucs'
 (dccWDS ("angular velocity" ++ n) (compoundPhrase'
  (cn $ n ++ " body's") (QP.angularVelocity ^. term))
  (phrase QP.angularVelocity)) (sub (eqSymb QP.angularVelocity) w) Real angVelU

momtParam n w = ucs'
 (dccWDS ("momentOfInertia" ++ n) (compoundPhrase'
  (QP.momentOfInertia ^. term) (cn $ "of rigid body " ++ n))
  (phrase QP.momentOfInertia)) (sub (eqSymb QP.momentOfInertia) w) Real momtInertU

perpParam n w = ucs'
 (dccWDS ("|| r_A" ++ n ++ " x n ||") 
  (compoundPhrase' (QPP.len `ofThe` QM.perpVect)
  (cn $ "to the contact displacement vector of rigid body " ++ n)) 
  (phrase QM.perpVect)) (Atop Magnitude $ Concat [w, label "*", --should be x for cross
  eqSymb QM.perpVect]) Real metre

rigidParam n w = ucs'
 (dccWDS ("rig_mass" ++ n) (compoundPhrase' (QPP.mass ^. term)
  (cn $ "of rigid body " ++ n)) (phrase QPP.mass)) (sub (eqSymb QPP.mass) w) Real kilogram

velBodyParam n w = ucs'
 (dccWDS ("velocity" ++ n) (compoundPhrase' (QP.velocity ^. term)
  (cn $ "of the  " ++ n ++ " body")) (phrase QP.velocity)) (sub (eqSymb QP.velocity) w) Real velU

velParam n w = ucs'
 (dccWDS ("velocity" ++ n) (compoundPhrase' (QP.velocity ^. term)
  (cn $ "at point " ++ n)) (phrase QP.velocity)) (sub (eqSymb QP.velocity) w) Real velU

iVect = ucs' (dccWDS "unitVect" (compoundPhrase' (cn "horizontal")
               (QM.unitVect ^. term)) (phrase QM.unitVect)) 
               (eqSymb QM.unitVect) Real metre
jVect       = ucs' (dccWDS "unitVectJ" (compoundPhrase' (cn "vertical")
               (QM.unitVect ^. term)) (phrase QM.unitVect)) (vec $ hat lJ) Real metre
normalVect  = ucs' (dccWDS "normalVect" (nounPhraseSent (S "collision" +:+
                   phrase QM.normalVect)) (phrase QM.normalVect)) 
                   (eqSymb QM.normalVect) (Vect Real) metre

dVect = ucs' (dccWDS "unitVect" 
          (cn "unit vector directed from the center of the large mass to the center of the smaller mass") 
                   (phrase QM.unitVect)) (vec (hat lD)) Real metre

dispNorm = ucs' (dccWDS "euclideanNormDisp" (cn "Euclidean norm of the distance between the center of mass of two bodies")
               (phrase QM.euclidNorm) ) (eqSymb QM.euclidNorm) Real metre

distMass = ucs' (dccWDS "distMass" (cn "distance between the center of mass of the rigid bodies") 
                 (phrase QP.distance)) (vec lD) Real metre

sqrDist = ucs' (dccWDS "euclideanNorm" (cn' "squared distance")
               (phrase QM.euclidNorm)) (sup (eqSymb QM.euclidNorm) 
               label2) Real m_2
             
rOB    = uc' "rOB" 
  (nounPhraseSP "displacement vector between the origin and point B")
  "FIXME: Define this or remove the need for definitions" 
  (sub (eqSymb QP.displacement) (Concat [lOrigin, lBodyB])) metre
  
posCM = ucs "p_CM" (nounPhraseSP "Center of Mass")
  "FIXME: Define this or remove the need for definitions" 
  (sub (eqSymb QP.position) lCMass) Real metre

massj = ucs' (dccWDS "m_j" (compoundPhrase' (QPP.mass ^. term)
                (cn "of the j-th particle")) (phrase QPP.mass)) 
                (sub (eqSymb QPP.mass) lJ) Real kilogram

posj = ucs' (dccWDS "p_j" (compoundPhrase' (QP.position ^. term) 
               (cn "vector of the j-th particle")) (phrase QP.position))
               (sub (eqSymb QP.position) lJ) Real metre

accj = ucs' (dccWDS "accj" (compoundPhrase' (cn "j-th body's")
               (QP.acceleration ^. term)) (phrase QP.acceleration))
               (sub (eqSymb QP.acceleration) lJ) Real accelU

angAccj = ucs' (dccWDS "angAccj" (nounPhrase'' (S "j-th body's" +:+
               phrase QP.angularAccel) (S "j-th body's" +:+
               phrase QP.angularAccel) CapWords CapWords) (phrase QP.angularAccel))
               (sub (eqSymb QP.angularAccel) lJ) Real angAccelU

velj = ucs' (dccWDS "velj" (compoundPhrase' (QP.velocity ^. term)
               (cn "of the j-th body")) (phrase QP.velocity))
               (sub (eqSymb QP.velocity) lJ) Real velU

torquej = ucs' (dccWDS "torquej" 
               (cn "torque applied to the j-th body")
               (phrase QP.torque)) (sub (eqSymb QP.torque) lJ) Real torqueU

mTot = ucs' (dccWDS "M_T" (compoundPhrase' (cn "total mass of the") 
                 (CP.rigidBody ^. term)) (phrase QPP.mass))
                 (sub (eqSymb QPP.mass) cT) Real kilogram

mLarger = ucs' (dccWDS "mLarger" (compoundPhrase' (cn "mass of the larger") 
                 (CP.rigidBody ^. term)) (phrase QPP.mass)) cM Real kilogram

timeC = ucs' (dccWDS "timeC" (cn "denotes the time at collision") 
                (phrase QP.time)) (sub (eqSymb QP.time) lColl) Real second

initRelVel = ucs' (dccWDS "v_i^AB" (compoundPhrase'
                 (compoundPhrase' (cn "initial relative") (QP.velocity ^. term))
                 (cn "between rigid bodies of A and B")) (phrase QP.velocity))
                 (sup (sub (eqSymb QP.velocity) QP.initial) (Concat [lBodyA, lBodyB])) (Vect Real) velU

finRelVel = ucs' (dccWDS "v_f^AB" (compoundPhrase'
                 (compoundPhrase' (cn "final relative") (QP.velocity ^. term))
                 (cn "between rigid bodies of A and B")) (phrase QP.velocity))
                 (sup (sub (eqSymb QP.velocity) QP.final) (Concat [lBodyA, lBodyB])) (Vect Real) velU

massIRigidBody = ucs' (dccWDS "massj" (compoundPhrase' (QPP.mass ^. term) 
                (cn "of the j-th rigid body")) (phrase QPP.mass)) 
                (sub (eqSymb QPP.mass) lJ) Real kilogram
normalLen = ucs' (dccWDS "length of the normal vector" (
                  QPP.len `ofThe` QM.normalVect) 
                  (phrase QM.normalVect))
                  (Atop Magnitude $ eqSymb QM.normalVect) Real metre

rRot = ucs' (dccWDS "r_j" (compoundPhrase' (QP.distance ^. term)
                (cn "between the j-th particle and the axis of rotation")) (phrase QP.distance)) 
                (sub (eqSymb QP.distance) lJ) Real metre

timeT = ucs' (dccWDS "t" (cn "point in time") (phrase QP.time))
                (eqSymb QP.time) Real second

inittime = ucs' (dccWDS "t_0" (cn "denotes the initial time") 
                (phrase QP.time)) (sub (eqSymb QP.time) label0) Real second

pointOfCollision = ucs' (dccWDS "point_c" (cn "point of collision") 
                 (S "point")) cP Real metre

collisionImpulse = ucs' (dccWDS "collisionImp" (compoundPhrase' 
                (cn "collision") (QP.impulseS ^. term)) (phrase QP.impulseS)) 
                (eqSymb QP.impulseS) Real impulseU

forcej = ucs' (dccWDS "forcej" (compoundPhrase' 
      (QP.force ^. term) (cn "applied to the j-th body at time t")) 
      (phrase QP.force)) (sub (eqSymb QP.force) lJ) Real newton

velAP = ucs' (dccWDS "v^AP" (compoundPhrase' (QP.velocity ^. term)
              (cn "of the point of collision P in body A")) 
              (phrase QP.velocity)) (sup (eqSymb QP.velocity)(Concat [lBodyA, lPoint])) 
              (Vect Real) velU
velBP = ucs' (dccWDS "v^BP" (compoundPhrase' (QP.velocity ^. term)
              (cn "of the point of collision P in body B")) 
              (phrase QP.velocity)) (sup (eqSymb QP.velocity)(Concat [lBodyB, lPoint]))
              (Vect Real) velU

force_1    = forceParam "1" "first"  label1
force_2    = forceParam "2" "second" label2
mass_1     = massParam  "1" "first"  label1
mass_2     = massParam  "2" "second" label2
velA       = velParam   "A" lBodyA
velB       = velParam   "B" lBodyB
velO       = velParam   "origin" lOrigin
angVelA    = angParam   "A" lBodyA
angVelB    = angParam   "B" lBodyB
perpLenA   = perpParam  "A" $ eqSymb contDispA
perpLenB   = perpParam  "B" $ eqSymb contDispB
momtInertA = momtParam  "A" lBodyA
momtInertB = momtParam  "B" lBodyB
momtInertK = momtParam  "k" lK
contDispA  = contParam  "A" "P" lBodyA lPoint
contDispB  = contParam  "B" "P" lBodyB lPoint
contDispK  = contParam  "k" "P" lK     lPoint
massA      = rigidParam "A" lBodyA
massB      = rigidParam "B" lBodyB
velo_1     = velBodyParam  "first"  label1
velo_2     = velBodyParam  "second" label2
time_1     = timeParam "1" "first"  label1
time_2     = timeParam "2" "second" label2

-- GlassBR.Unitals
modElas = uc' "modElas" (nounPhraseSP "modulus of elasticity of glass")
  "the ratio of tensile stress to tensile strain of glass" cE pascal

plateLen = uqcND "plateLen" (nounPhraseSP "plate length (long dimension)")
  lA metre Real 
  [ gtZeroConstr,
    physc $ UpFrom (Inc, sy plateWidth),
    sfwrc $ Bounded (Inc , sy dimMin) (Inc , sy dimMax)] (dbl 1.5) defaultUncrt

plateWidth = uqcND "plateWidth" (nounPhraseSP "plate width (short dimension)")
  lB metre Real
  [ physc $ Bounded (Exc, exactDbl 0) (Inc, sy plateLen),
    sfwrc $ Bounded (Inc, sy dimMin) (Inc, sy dimMax)] (dbl 1.2) defaultUncrt

charWeight = uqcND "charWeight" (nounPhraseSP "charge weight") 
  lW kilogram Real
  [ gtZeroConstr,
    sfwrc $ Bounded (Inc, sy cWeightMin) (Inc, sy cWeightMax)]
    (exactDbl 42) defaultUncrt

demand      = ucs' demandq lQ Real pascal
tmDemand    = ucs' load (variable "Load") Real pascal  
lRe         = ucs' loadResis (variable "LR") Real pascal
tmLRe       = ucs' capacity (variable "capacity") Real pascal
nonFactorL  = ucs' nonFactoredL (variable "NFL") Real pascal
eqTNTWeight = ucs' eqTNTChar (sub (eqSymb charWeight) (eqSymb tNT)) Real kilogram

-- NoPCM.Unitals
tempInit = uqc "tempInit" (nounPhraseSP "initial temperature")
  "the temperature at the beginning of the simulation"
  (sub (eqSymb temp) lInit) centigrade Real
  [physc $ Bounded (Exc, exactDbl 0) (Exc, exactDbl 100)] (exactDbl 40) defaultUncrt

-- Projectile.Unitals
flightDur = constrainedNRV' (ucs'  C.flightDur (subStr lT "flight") Real second)  [gtZeroConstr]
landPos   = constrainedNRV' (ucs'  C.landPos   (subStr lP "land"  ) Real metre)   [gtZeroConstr]
launAngle = constrained'    (dqd' C.launAngle (autoStage lTheta)   Real (Just radian)) [physc $ Bounded (Exc, exactDbl 0) (Exc, half $ sy pi_)] (sy pi_ $/ exactDbl 4)
launSpeed = constrained'    (ucs'  C.launSpeed (subStr lV "launch") Real velU)    [gtZeroConstr] (exactDbl 100)
offset    = constrainedNRV' (ucs'  C.offset    (subStr lD "offset") Real metre)   [physc $ UpFrom (Exc, neg $ sy targPos) ]
targPos   = constrained'    (ucs' C.targPos   (subStr lP "target") Real metre)   [gtZeroConstr] (exactDbl 1000)

-- SglPendulum.Unitals
lenRod = makeUCWDS "l_rod" (cn "length of the rod")
        (phraseNP (len `the_ofThe` rod))
        (sub cL lRod) metre

pendDisplacementAngle = makeUCWDS "pendDisplacementAngle" (cn "displacement angle of the pendulum")
        (phraseNP (angle `the_ofThe` pendulum))
        (sub lTheta lP) radian

initialPendAngle = makeUCWDS "initialPendAngle" (cn "initial pendulum angle")
        (phraseNP (NP.the (CM.iAngle `of_` pendulum)))
        (sub lTheta lI) radian

-- SSP.Unitals
slopeDist = uq (constrained' (ucsWS "x_slope,i"
  (nounPhraseSent $ plural xCoord `S.of_` S "the slope")
  (plural xCoord `S.of_` S "points on the soil slope")
  (sub (vec lX) lSlope) (Vect Real) metre) [] (exactDbl 0)) defaultUncrt

slopeHght = uq (constrained' (ucsWS "y_slope,i"
  (nounPhraseSent $ plural yCoord `S.of_` S "the slope")
  (plural yCoord `S.of_` S "points on the soil slope")
  (sub (vec lY) lSlope) (Vect Real) metre) [] (exactDbl 0)) defaultUncrt

waterDist = uqc "x_wt,i" (nounPhraseSent $ plural xCoord `S.of_` S "the water table")
  "x-positions of the water table"
  (sub (vec lX) lWatTab) metre (Vect Real) [] (exactDbl 0) defaultUncrt

waterHght = uqc "y_wt,i" (nounPhraseSent $ plural yCoord `S.of_` S "the water table")
  "heights of the water table"
  (sub (vec lY) lWatTab) metre (Vect Real) [] (exactDbl 0) defaultUncrt

xMaxExtSlip = uq (constrained' (makeUCWDS "x_slip^maxExt"
  (nounPhraseSent $ S "maximum exit" +:+ phrase xCoord)
  (S "the maximum potential" +:+ phrase xCoord +:+ S "for the exit point of a slip surface")
  (sup (sub lX lSlip) lMaxExt) metre) [] (exactDbl 100)) defaultUncrt

xMaxEtrSlip = uq (constrained' (makeUCWDS "x_slip^maxEtr" 
  (nounPhraseSent $ S "maximum entry" +:+ phrase xCoord)
  (S "the maximum potential" +:+ phrase xCoord +:+ S "for the entry point of a slip surface")
  (sup (sub lX lSlip) lMaxEtr) metre) [] (exactDbl 20)) defaultUncrt
  
xMinExtSlip = uq (constrained' (makeUCWDS "x_slip^minExt"
  (nounPhraseSent $ S "minimum exit" +:+ phrase xCoord)
  (S "the minimum potential" +:+ phrase xCoord +:+ S "for the exit point of a slip surface")
  (sup (sub lX lSlip) lMinExt) metre) [] (exactDbl 50)) defaultUncrt

xMinEtrSlip = uq (constrained' (makeUCWDS "x_slip^minEtr"
  (nounPhraseSent $ S "minimum entry" +:+ phrase xCoord)
  (S "the minimum potential" +:+ phrase xCoord +:+ S "for the entry point of a slip surface")
  (sup (sub lX lSlip) lMinEtr) metre) [] (exactDbl 0)) defaultUncrt

yMaxSlip = uq (constrained' (makeUCWDS "y_slip^max"
  (nounPhraseSent $ S "maximum" +:+ phrase yCoord)
  (S "the maximum potential" +:+ phrase yCoord `S.of_` S "a point on a slip surface")
  (supMax (sub lY lSlip)) metre) [] (exactDbl 30)) defaultUncrt

yMinSlip = uq (constrained' (makeUCWDS "y_slip^min"
  (nounPhraseSent $ S "minimum" +:+ phrase yCoord)
  (S "the minimum potential" +:+ phrase yCoord `S.of_` S "a point on a slip surface")
  (supMin (sub lY lSlip)) metre) [] (exactDbl 0)) defaultUncrt

effCohesion = uqc "c'" (cn "effective cohesion")
  "the internal pressure that sticks particles of soil together"
  (prime $ variable "c") pascal Real [gtZeroConstr] (exactDbl 10000) defaultUncrt

fricAngle = uqc "varphi'" (cn "effective angle of friction")
  ("the angle of inclination with respect to the horizontal axis of " ++
  "the Mohr-Coulomb shear resistance line") --http://www.geotechdata.info
  (prime vPhi) degree Real [physc $ Bounded (Exc, exactDbl 0) (Exc, exactDbl 90)]
  (exactDbl 25) defaultUncrt

dryWeight = uqc "gamma" (cn "soil dry unit weight")
  "the weight of a dry soil/ground layer divided by the volume of the layer"
  (sub lGamma lDry) specificWeight Real [gtZeroConstr]
  (exactDbl 20000) defaultUncrt

satWeight = uqc "gamma_sat" (cn "soil saturated unit weight")
  "the weight of saturated soil/ground layer divided by the volume of the layer"
  (sub lGamma lSat) specificWeight Real [gtZeroConstr]
  (exactDbl 20000) defaultUncrt

waterWeight = uqc "gamma_w" (cn "unit weight of water")
  "the weight of one cubic meter of water"
  (sub lGamma lW) specificWeight Real [gtZeroConstr]
  (exactDbl 9800) defaultUncrt

intNormForce = ucsWS "G_i" (cn "interslice normal forces")
  (S "the forces per meter" `S.inThe` phrase zDir +:+
   S "exerted between each pair of adjacent slices")
  (vec cG) (Vect Real) forcePerMeterU

slipHght = ucs "y_slip,i" (nounPhraseSent $ plural yCoord +:+ S "of the slip surface")
  "heights of the slip surface"
  (sub (vec lY) lSlip) (Vect Real) metre

slipDist = ucsWS "x_slip,i" (nounPhraseSent $ plural xCoord +:+ S "of the slip surface")
  (plural xCoord `S.of_` S "points on the slip surface")
  (sub (vec lX) lSlip) (Vect Real) metre

xi     = makeUCWDS "x_i" (nounPhraseSent $ phrase xCoord)
  (phraseNP (NP.the (xCoord `inThe` cartesian))) lX metre

yi     = makeUCWDS "y_i" (nounPhraseSent $ phrase yCoord)
  (phraseNP (NP.the (yCoord `inThe` cartesian))) lY metre

zcoord = makeUCWDS "z"   (nounPhraseSent $ phrase zCoord)
  (phraseNP (NP.the (zCoord `inThe` cartesian))) lZ metre

critCoords = makeUCWDS "(xcs,ycs)" (cn "critical slip surface coordinates")
  (S "the set" `S.of_` pluralNP (xCoord `and_PP` yCoord) +:+
   S "that describe the vertices of the critical slip surface")
  (Concat [sub (vec lX) lCSlip, label ",", sub (vec lY) lCSlip]) metre

mobilizedShear = uc' "mobilizedShear" (cn' "mobilized shear force")
  "the shear force in the direction of potential motion" cS newton

resistiveShear = makeUCWDS "resistiveShear" (cn' "resistive shear force")
  (S "the Mohr Coulomb frictional force that describes the limit" `S.of_`
    phrase mobilizedShear +:+ S "that can be withstood before failure")
  cP newton

mobShrI = makeUCWDS "mobShrFs" (cn' "mobilized shear force")
  (phraseNP (the mobilizedShear) +:+ S "per meter" `S.inThe` phrase zDir +:+
   S "for each slice")
  (vec cS) forcePerMeterU 

shrResI = makeUCWDS "shrRes" (cn "resistive shear forces")
  (S "the Mohr Coulomb frictional forces per meter" `S.inThe` phrase zDir +:+
   S "for each slice that describe the limit" `S.of_` phrase mobilizedShear +:+
   S "the slice can withstand before failure")
  (vec cP) forcePerMeterU

shearFNoIntsl = ucsWS "T_i" (cn ("mobilized shear forces " ++ wiif)) 
  (pluralNP (the mobilizedShear) +:+ S "per meter" +:+ S wiif `S.inThe`
   phrase zDir +:+  S "for each slice")
  (vec cT) (Vect Real) forcePerMeterU

shearRNoIntsl = ucsWS "R_i" (cn ("resistive shear forces " ++ wiif))
  (pluralNP (the resistiveShear) +:+ S "per meter" +:+ S wiif `S.inThe`
   phrase zDir +:+ S "for each slice")
  (vec cR) (Vect Real) forcePerMeterU

slcWght = ucsWS "W_i" (cn "weights")
  (S "the downward force per meter" `S.inThe` phrase zDir +:+
   S "on each slice caused by" +:+ phrase gravity)
  (vec cW) (Vect Real) forcePerMeterU

watrForce = ucsWS "H_i" (cn "interslice normal water forces") 
  (S "the normal water forces per meter" `S.inThe` phrase zDir +:+
   S "exerted" `S.inThe` phrase xDir +:+ S "between each pair of adjacent slices")
  (vec cH) (Vect Real) forcePerMeterU

intShrForce = ucsWS "X_i" (cn "interslice shear forces") 
  (S "the shear forces per meter" `S.inThe` phrase zDir +:+ S "exerted between adjacent slices")
  (vec cX) (Vect Real)forcePerMeterU

baseHydroForce = ucsWS "U_b,i" (cn "base hydrostatic forces")
  (S "the forces per meter" `S.inThe` phrase zDir +:+ S "from water pressure within each slice")
  (sub (vec cU) lBase) (Vect Real) forcePerMeterU

surfHydroForce = ucsWS "U_t,i" (cn "surface hydrostatic forces")
  (S "the forces per meter" `S.inThe` phrase zDir +:+ S "from water pressure acting" +:+
   S "into each slice from standing water on the slope surface")
  (sub (vec cU) lSurface) (Vect Real) forcePerMeterU

totNrmForce = ucsWS "N_i" (cn "normal forces")
  (S "the total reactive forces per meter" `S.inThe` phrase zDir +:+
   S "for each slice of a soil surface subject to a body resting on it")
  (vec cN) (Vect Real) forcePerMeterU

nrmFSubWat = ucsWS "N'_i" (cn "effective normal forces")
  (S "the forces per meter" `S.inThe` phrase zDir +:+ S "for each slice of a soil surface" `sC`
   S "subtracting pore water reactive force from total reactive force") 
  (vec (prime $ variable "N")) (Vect Real) forcePerMeterU

surfLoad = ucsWS "Q_i" (cn "external forces") 
  (S "the forces per meter" `S.inThe` phrase zDir +:+
   S "acting into the surface from the midpoint of each slice")
  (vec cQ) (Vect Real) forcePerMeterU

baseAngle = ucs "alpha_i" (cn "base angles")
  "the angles between the base of each slice and the horizontal"
  (vec lAlpha) (Vect Real) degree

surfAngle = ucs "beta_i" (cn "surface angles")
  "the angles between the surface of each slice and the horizontal"
  (vec lBeta) (Vect Real) degree

impLoadAngle = ucs "omega_i" (cn "imposed load angles")
  "the angles between the external force acting into the surface of each slice and the vertical"
  (vec lOmega) (Vect Real) degree

baseWthX = ucsWS "b_i" (cn "base width of slices")
  (S "the width of each slice" `S.inThe` phrase xDir)
  (vec lB) (Vect Real) metre

baseLngth = ucs "l_b,i" (cn "total base lengths of slices") 
  "the lengths of each slice in the direction parallel to the slope of the base"
  (sub (vec cL) lB) (Vect Real) metre

surfLngth = ucs "l_s,i" (cn "surface lengths of slices")
  "the lengths of each slice in the direction parallel to the slope of the surface"
  (sub (vec cL) lS) (Vect Real) metre

midpntHght = ucsWS "h_i" (nounPhraseSent $ phrase yDir +:+ S "heights of slices")
  (S "the heights" `S.inThe` phrase yDir +:+ S "from the base of each slice" `S.toThe`
   S "slope surface, at the" +:+ phrase xDir +:+ S "midpoint of the slice")
  (vec lH) (Vect Real) metre

porePressure = uc' "u" (cn "pore pressure")
  "the pressure that comes from water within the soil" lU pascal
  
shrStress = uc' "tau_i" (cn "shear strength")
  "the strength of a material against shear failure" (sup lTau (label "f")) pascal

sliceHght = makeUCWDS "h_z,i" (cn "heights of interslice normal forces")
  (pluralNP (height `inThePS` yDir) `S.the_ofThe` S "interslice normal forces on each slice")
  (subZ (vec lH)) metre

sliceHghtW = makeUCWDS "h_z,w,i" (cn "heights of the water table")
  S.inThe` phrase yDir +:+ S "from the base of each slice to the water table")
  (sub (vec lH) lHeights) metre

nrmShearNum = ucs "C_num,i" (cn "proportionality constant numerator")
  ("values for each slice that sum together to form the numerator of the " ++
  "interslice normal to shear force proportionality constant")
  (sub (vec cC) lNum) (Vect Real) newton
  
nrmShearDen = ucs "C_den,i" (cn "proportionality constant denominator")
  ("values for each slice that sum together to form the denominator of the " ++
  "interslice normal to shear force proportionality constant")
  (sub (vec cC) lDen) (Vect Real) newton

fx = makeUCWDS "fx" (nounPhraseSent $ phrase xCoord +:+ S "of the force")
  (S "the force acting" `S.inThe` phrase xDir) (subX cF) newton

fy = makeUCWDS "fy" (nounPhraseSent $ phrase yCoord +:+ S "of the force")
  (S "the force acting" `S.inThe` phrase yDir) (subY cF) newton

fn = uc' "F_n" (cn "total normal force") "component of a force in the normal direction"
  (sub cF (label "n")) newton

ft = uc' "F_t" (cn "tangential force") "component of a force in the tangential direction"
  (sub cF (label "t")) newton

nrmForceSum = uc' "F_x^G" (cn "sums of the interslice normal forces") 
  "the sums of the normal forces acting on each pair of adjacent interslice boundaries"
  (sup (subX (vec cF)) lNorm) newton

watForceSum = uc' "F_x^H" (cn "sums of the interslice normal water forces") 
  "the sums of the normal water forces acting on each pair of adjacent interslice boundaries"
  (sup (subX (vec cF)) lNormWat) newton

sliceHghtRight = ucs "h^R" (cn "heights of the right side of slices") 
  "the heights of the right side of each slice, assuming slice surfaces have negative slope"
  (sup (vec lH) lRight) (Vect Real) metre

sliceHghtLeft = ucs "h^L" (cn "heights of the left side of slices") 
  "the heights of the left side of each slice, assuming slice surfaces have negative slope"
  (sup (vec lH) lLeft) (Vect Real) metre

totNormStress = uc' "sigma" (cn' "total normal stress")
  "the total force per area acting on the soil mass" lSigma pascal

tangStress = uc' "tau" (cn' "tangential stress")
  "the shear force per unit area" lTau pascal

effectiveStress = uc' "sigma'" (cn' "effective stress")
  ("the stress in a soil mass that is effective in causing volume changes " ++
   "and mobilizes the shear strength arising from friction; represents the " ++
   "average stress carried by the soil skeleton")
  (prime lSigma) pascal

effNormStress = uc' "sigmaN'" (cn' "effective normal stress")
  ("the normal stress in a soil mass that is effective in causing volume " ++
   "changes; represents the average normal stress carried by the soil skeleton")
  (prime $ sub lSigma cN) pascal

dryVol = uc' "V_dry" (cn "volumes of dry soil")
  "the amount of space occupied by dry soil for each slice"
  (sub (vec cV) lDry) m_3

satVol = uc' "V_sat" (cn "volumes of saturated soil")
  "the amount of space occupied by saturated soil for each slice"
  (sub (vec cV) lSat) m_3

rotForce = uc' "F_rot" (cn "force causing rotation") 
  "a force in the direction of rotation" (sub cF lRot) newton
  
momntArm = uc' "r" (cn' "length of the moment arm") 
  "the distance between a force causing rotation and the axis of rotation"
  lR metre

-- SWHS.Unitals
inSA = uc' "inSA" (nounPhraseSP
  "surface area over which heat is transferred in")
  "surface area over which thermal energy is transferred into an object"
  (sub cA lIn) m_2

outSA = uc' "outSA" (nounPhraseSP
  "surface area over which heat is transferred out")
  "surface area over which thermal energy is transferred out of an object"
  (sub cA lOut) m_2

htCapL = uc' "htCapL" (nounPhraseSP "specific heat capacity of a liquid")
  ("the amount of energy required to raise the temperature of a given " ++
  "unit mass of a given liquid by a given amount")
  (sup (eqSymb heatCapSpec) lLiquid) UT.heatCapSpec

htCapS = uc' "htCapS"
  (nounPhraseSP "specific heat capacity of a solid")
  ("the amount of energy required to raise the temperature of " ++
  "a given unit mass of a given solid by a given amount")
  (sup (eqSymb heatCapSpec) lSolid) UT.heatCapSpec

htCapV = uc' "htCapV"
  (nounPhraseSP "specific heat capacity of a vapour")
  ("the amount of energy required to raise the temperature of a given " ++
  "unit mass of vapour by a given amount")
  (sup (eqSymb heatCapSpec) lVapour) UT.heatCapSpec

pcmInitMltE = uc' "pcmInitMltE" (nounPhraseSP
  "change in heat energy in the PCM at the instant when melting begins")
  "change in thermal energy in the phase change material at the melting point"
  (sup (sub (sub (eqSymb sensHeat) lPCM) lMelt) lInit) joule

volHtGen = uc' "volHtGen"
  (nounPhraseSP "volumetric heat generation per unit volume")
  "Amount of thermal energy generated per unit volume" lG UT.volHtGenU

htTransCoeff = uc' "htTransCoeff"
  (nounPhraseSP "convective heat transfer coefficient")
  ("the proportionality constant between the heat flux and the " ++
  "thermodynamic driving force for the flow of thermal energy")
  lH UT.heatTransferCoef

pcmMass = uc' "pcmMass" (nounPhraseSP "mass of phase change material")
  "the quantity of matter within the phase change material"
  (sub (eqSymb mass) lPCM) kilogram

wMass = uc' "wMass" (nounPhraseSP "mass of water")
  "the quantity of matter within the water" (sub (eqSymb mass) lWater) kilogram

thFluxVect = uc' "thFluxVect" (nounPhraseSP "thermal flux vector")
  "vector denoting the direction of thermal flux through a surface"
  (vec lQ) UT.thermalFlux

htFluxC = uc' "htFluxC"
  (nounPhraseSP "heat flux into the water from the coil")
  "the rate of heat energy transfer into the water from the coil per unit time"
  (sub (eqSymb htFlux) lCoil) UT.thermalFlux

htFluxIn = uc' "htFluxIn" (nounPhraseSP "heat flux input")
  "the rate of heat energy transfer into an object per unit time"
  (sub (eqSymb htFlux) lIn) UT.thermalFlux

htFluxOut = uc' "htFluxOut" (nounPhraseSP "heat flux output")
  "the rate of heat energy transfer into an object per unit time"
  (sub (eqSymb htFlux) lOut) UT.thermalFlux

htFluxP = uc' "htFluxP" (nounPhraseSP "heat flux into the PCM from water")
  ("the rate of heat energy transfer into the phase" ++
  "change material from the water per unit time")
  (sub (eqSymb htFlux) lPCM) UT.thermalFlux

latentEP = uc' "latentEP" (nounPhraseSP "latent heat energy added to PCM")
  ("energy released or absorbed, by a body or a thermodynamic system, "++
  "during a constant-temperature process and absorbed by the phase" ++
  "change material") (sub (eqSymb latentHeat) lPCM) joule

tempEnv = uc' "tempEnv" (nounPhraseSP "temperature of the environment")
  "the tempature of a given environment"
  (sub (eqSymb temp) lEnv) centigrade

tInitMelt = uc' "tInitMelt"
  (nounPhraseSP "time at which melting of PCM begins")
  ("time at which the phase change material " ++
    "begins changing from a solid to a liquid")
  (sup (sub (eqSymb time) lMelt) lInit) second

tFinalMelt = uc' "tFinalMelt"
  (nounPhraseSP "time at which melting of PCM ends")
  ("time at which the phase change material " ++
    "finishes changes from a solid to a liquid")
  (sup (sub (eqSymb time) lMelt) lFinal) second
  
tankVol = uc' "tankVol" (nounPhraseSP "volume of the cylindrical tank")
  "the amount of space encompassed by a tank"
  (sub (eqSymb vol) lTank) m_3

wVol = uc' "wVol" (vol `of_` water)
  "the amount of space occupied by a given quantity of water"
  (sub (eqSymb vol) lWater) m_3

deltaT = uc' "deltaT" (nounPhraseSP "change in temperature")
  "change in the average kinetic energy of a given material"
  (Atop Delta $ eqSymb temp) centigrade

tau = ucStaged "tau" (nounPhraseSP "dummy variable for integration over time")
  "binary value representing the presence or absence of integration over time"
  (autoStage lTau) second

tauLP = ucStaged "tauLP" (nounPhraseSP "ODE parameter for liquid PCM")
  ("derived through melting of phase change material, which " ++
  "changes ODE parameter for solid PCM into parameter for liquid")
  (autoStage $ sup (sub lTau lPCM) lLiquid) second

tauSP = ucStaged "tauSP" (nounPhraseSP "ODE parameter for solid PCM")
  "derived parameter based on rate of change of temperature of phase change material"
  (autoStage $ sup (sub lTau lPCM) lSolid) second

tauW = ucStaged "tauW" (nounPhraseSP "ODE parameter for water related to decay time")
  "derived parameter based on rate of change of temperature of water"
  (autoStage $ sub lTau lWater) second

simTime = uc' "simTime" (compoundPhrase' (simulation ^. term)
  (time ^. term)) "time over which the simulation runs"
  lT second

thickness = uc'  "thickness" (nounPhraseSP "Minimum thickness of a sheet of PCM")
  "the minimum thickness of a sheet of PCM"
  (subMin lH) metre

tankLength = uqc "tankLength" (nounPhraseSP "length of tank")
  "the length of the tank" cL metre Real
  [gtZeroConstr,
  sfwrc $ Bounded (Inc, sy tankLengthMin) (Inc, sy tankLengthMax)] (dbl 1.5)
  defaultUncrt

diam = uqc "diam" (nounPhraseSP "diameter of tank")
  "the diameter of the tank" cD metre Real
  [gtZeroConstr, sfwrc $ Bounded (Inc, sy arMin) (Inc, sy arMax)]
  (dbl 0.412) defaultUncrt

pcmVol = uqc "pcmVol" (nounPhraseSP "volume of PCM")
  "the amount of space occupied by a given quantity of phase change material"
  (sub (eqSymb vol) lPCM) m_3 Real
  [physc $ Bounded (Exc, exactDbl 0) (Exc, sy tankVol),
   sfwrc $ UpFrom (Inc, sy fracMin `mulRe` sy tankVol)] 
  (dbl 0.05) defaultUncrt

pcmSA = uqc "pcmSA"
  (compoundPhrase (nounPhrase'' (S "phase change material")
  (S "phase change material")
  CapFirst CapWords) (nounPhrase'' (phrase surArea) (phrase surArea)
  CapFirst CapWords))
  "area covered by the outermost layer of the phase change material"
  (sub cA lPCM) m_2 Real
  [gtZeroConstr,
  sfwrc $ Bounded (Inc, sy pcmVol) (Inc, (exactDbl 2 $/ sy thickness) `mulRe` sy tankVol)]
  (dbl 1.2) defaultUncrt

tempMeltP = uqc "tempMeltP"
  (nounPhraseSP "melting point temperature for PCM")
  "temperature at which the phase change material transitions from a solid to a liquid"
  (sup (sub (eqSymb temp) lMelt) lPCM) centigrade Real
  [physc $ Bounded (Exc, exactDbl 0) (Exc, sy tempC)] (dbl 44.2) defaultUncrt

htCapSP = uqc "htCapSP"
  (nounPhraseSP "specific heat capacity of PCM as a solid")
  ("the amount of energy required to raise the temperature of a " ++
  "given unit mass of solid phase change material by a given amount")
  (sup (sub (eqSymb heatCapSpec) lPCM) lSolid) UT.heatCapSpec Real
  [gtZeroConstr,
  sfwrc $ Bounded (Exc, sy htCapSPMin) (Exc, sy htCapSPMax)]
  (exactDbl 1760) defaultUncrt

htCapLP = uqc "htCapLP"
  (nounPhraseSP "specific heat capacity of PCM as a liquid")
  ("the amount of energy required to raise the temperature of a " ++
  "given unit mass of liquid phase change material by a given amount")
  (sup (sub (eqSymb heatCapSpec) lPCM) lLiquid) UT.heatCapSpec Real
  [gtZeroConstr,
  sfwrc $ Bounded (Exc, sy htCapLPMin) (Exc, sy htCapLPMax )]
  (exactDbl 2270) defaultUncrt

htFusion = uqc "htFusion" (nounPhraseSP "specific latent heat of fusion")
  "amount of thermal energy required to completely melt a unit mass of a substance"
  (sub cH lFusion) specificE Real
  [gtZeroConstr,
  sfwrc $ Bounded (Exc, sy htFusionMin) (Exc, sy htFusionMax)] (exactDbl 211600) defaultUncrt

coilSA = uqc "coilSA"
  (compoundPhrase (nounPhrase'' (S "heating coil") (S "heating coil") CapFirst CapWords)
  (nounPhrase'' (phrase surArea) (phrase surArea) CapFirst CapWords))
  "area covered by the outermost layer of the coil" (sub cA lCoil) m_2 Real
  [gtZeroConstr,
  sfwrc $ UpTo (Inc, sy coilSAMax)] (dbl 0.12) defaultUncrt

tempC = uqc "tempC" (nounPhraseSP "temperature of the heating coil")
  "the average kinetic energy of the particles within the coil"
  (sub (eqSymb temp) lCoil) centigrade Real
  [physc $ Bounded (Exc, exactDbl 0) (Exc, exactDbl 100)] (exactDbl 50) defaultUncrt

htCapW = uqc "htCapW" (heatCapSpec `of_` water)
  ("the amount of energy required to raise the " ++
   "temperature of a given unit mass of water by a given amount")
  (sub (eqSymb heatCapSpec) lWater) UT.heatCapSpec Real
  [gtZeroConstr,
  sfwrc $ Bounded (Exc, sy htCapWMin) (Exc, sy htCapWMax)] (exactDbl 4186) defaultUncrt
  
coilHTC = uqc "coilHTC" (nounPhraseSP
  "convective heat transfer coefficient between coil and water")
  ("the convective heat transfer coefficient that models " ++
  "the thermal flux from the coil to the surrounding water")
  (sub (eqSymb htTransCoeff) lCoil)
  UT.heatTransferCoef Real
  [gtZeroConstr,
  sfwrc $ Bounded (Inc, sy coilHTCMin) (Inc, sy coilHTCMax)] (exactDbl 1000) defaultUncrt

pcmHTC = uqc "pcmHTC"
  (nounPhraseSP "convective heat transfer coefficient between PCM and water")
  ("the convective heat transfer coefficient that models " ++
   "the thermal flux from the phase change material to the surrounding water")
  (sub lH lPCM) UT.heatTransferCoef Real
  [gtZeroConstr,
  sfwrc $ Bounded (Inc, sy pcmHTCMin) (Inc, sy pcmHTCMax)] (exactDbl 1000) defaultUncrt

tempInit = uqc "tempInit" (nounPhraseSP "initial temperature")
  "the temperature at the beginning of the simulation"
  (sub (eqSymb temp) lInit) centigrade Real
  [physc $ Bounded (Exc, exactDbl 0) (Exc, sy meltPt)] (exactDbl 40) defaultUncrt
  
timeFinal = uqc "timeFinal" (nounPhraseSP "final time")
  ("the amount of time elapsed from the beginning of the " ++
   "simulation to its conclusion") (sub (eqSymb time) 
  lFinal) second Real
  [gtZeroConstr,
  sfwrc $ UpTo (Exc, sy timeFinalMax)] (exactDbl 50000) defaultUncrt

timeStep = uqc "timeStep" (nounPhraseSP "time step for simulation")
  ("the finite discretization of time used in the numerical method " ++
   "for solving the computational model")
  (sub (eqSymb time) lStep) second Real
  [physc $ Bounded (Exc, exactDbl 0) (Exc, sy timeFinal)]
  (dbl 0.01) defaultUncrt

tempW = cuc' "tempW"
  (nounPhraseSP "temperature of the water")
  "the average kinetic energy of the particles within the water" 
  (sub (eqSymb temp) lWater) centigrade (Vect Real)
  [physc $ Bounded (Inc, sy tempInit) (Inc, sy tempC)] (exactDbl 0)

tempPCM = cuc' "tempPCM"
  (nounPhraseSP "temperature of the phase change material")
  "the average kinetic energy of the particles within the phase change material"
  (sub (eqSymb temp) lPCM) centigrade Real
  [physc $ Bounded (Inc, sy tempInit) (Inc, sy tempC)] (exactDbl 0)
  
watE = cuc' "watE" (nounPhraseSP "change in heat energy in the water")
  "change in thermal energy within the water" 
  (sub (eqSymb sensHeat) lWater) joule Real
  [physc $ UpFrom (Inc, exactDbl 0)] (exactDbl 0)
  
pcmE = cuc' "pcmE" (nounPhraseSP "change in heat energy in the PCM")
  "change in thermal energy within the phase change material" 
  (sub (eqSymb sensHeat) lPCM) joule Real
  [physc $ UpFrom (Inc, exactDbl 0)] (exactDbl 0)
```