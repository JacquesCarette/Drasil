## Specific Examples

Representative of the Population:

Defining terms w/ English definitions. e.g.,
`drasil-data/lib/Data/Drasil/Concepts/Physics.hs`:
```haskell
displacement = dccWDS "displacement" (cn' "displacement")
  (S "the change in" +:+ (position ^. defn))
```

Defining “domains” (I don't understand how this is supposed to work). e.g.,
`drasil-data/lib/Data/Drasil/Concepts/Documentation.hs`:
```haskell
-- | Root SRS Domain.
srsDom :: ConceptChunk
srsDom = dcc "srsDom" (srs ^. term) "srs"

goalStmtDom, assumpDom, reqDom, funcReqDom, nonFuncReqDom, chgProbDom, 
  likeChgDom, unlikeChgDom, refByDom, refNameDom :: ConceptChunk
goalStmtDom   = ccs (mkIdea "goalStmtDom"   (goalStmt ^. term)                 $ Just "GS")       EmptyS [srsDom]
assumpDom     = ccs (mkIdea "assumpDom"     (assumption ^. term)               $ Just "A")        EmptyS [srsDom]
reqDom        = ccs (mkIdea "reqDom"        (requirement ^. term)              $ Just "R")        EmptyS [srsDom]
...
```

Defining terms w/ English definitions including sources. e.g.,
`drasil-example/glassbr/lib/Drasil/GlassBR/Unitals.hs`:
```haskell
terms :: [ConceptChunk]
terms = [..., annealedGl, ...]

..., annealedGl, ... :: ConceptChunk
annealedGl    = cc' annealed
  (S "a flat, monolithic, glass lite which has uniform thickness where" +:+
  S "the residual surface stresses are almost zero, as defined in"+:+ refS astm2016)
```

Extracting English descriptions + terms from other chunks (such as
`QuantityDict`s). e.g., see `cw` use in
`drasil-example/glassbr/lib/Drasil/GlassBR/Body.hs`:
```haskell
symbMap :: ChunkDB
symbMap = cdb thisSymbols (map nw acronyms ++ map nw thisSymbols ++ map nw con
  ++ map nw con' ++ map nw terms ++ map nw doccon ++ map nw doccon' ++ map nw educon
  ++ [nw sciCompS] ++ map nw compcon ++ map nw mathcon ++ map nw mathcon'
  ++ map nw softwarecon ++ map nw terms ++ [nw lateralLoad, nw materialProprty]
   ++ [nw distance, nw algorithm] ++
  map nw fundamentals ++ map nw derived ++ map nw physicalcon)
  (map cw symb ++ terms ++ Doc.srsDomains) (map unitWrapper [metre, second, kilogram]
  ++ map unitWrapper [pascal, newton]) GB.dataDefs iMods [] tMods concIns
  labCon allRefs
```

`drasil-theory/lib/Theory/Drasil/Theory.hs` also has a bit of dead code using `ConceptChunk`. Irrelevant.

## Constructor Use

2 constructors:
```haskell
dcc :: String -> NP -> String -> ConceptChunk 
-- | Smart constructor for creating concept chunks given a 'UID', 'NounPhrase'
-- ('NP') and definition (as a 'String').
dcc i ter des = ConDict (mkIdea i ter Nothing) (S des) []
-- ^ Concept domain tagging is not yet implemented in this constructor.

-- | Similar to 'dcc', except the definition takes a 'Sentence'.
dccWDS :: String -> NP -> Sentence -> ConceptChunk
dccWDS i t d = ConDict (mkIdea i t Nothing) d []
```

4 extractors:
```haskell
-- | Constructor for projecting an idea into a 'ConceptChunk'. Takes the
-- definition of the 'ConceptChunk' as a 'String'. Does not allow concept domain
-- tagging.
cc :: Idea c => c -> String -> ConceptChunk
cc n d = ConDict (nw n) (S d) []

-- | Same as 'cc', except definition is a 'Sentence'.
cc' :: Idea c => c -> Sentence -> ConceptChunk
cc' n d = ConDict (nw n) d []

-- | Similar to 'cc'', but allows explicit domain tagging.
ccs :: (Idea c, Concept d) => c -> Sentence -> [d] -> ConceptChunk --Explicit tagging
ccs n d l = ConDict (nw n) d $ map (^. uid) l

-- | For projecting out to the 'ConceptChunk' data-type.
cw :: Concept c => c -> ConceptChunk
cw c = ConDict (nw c) (c ^. defn) (cdom c)
```

<details>

<summary>`rg " (dcc|dccWDS|cc|cc'|ccs|cw) " -ths --sort-files`</summary>

```console
drasil-data/lib/Data/Drasil/Concepts/Computation.hs
14:algorithm = dcc "algorithm" (cn' "algorithm")
16:absTolerance = dcc "absTolerance"   (cn' "Absolute tolerance") "a fixed number that is used to make direct comparisons"
17:relTolerance = dcc "relTolerance"   (cn' "Relative tolerance") " maximum amount of error that the user is willing to allow in the solution"
20:modCalcDesc = dccWDS "modCalcDesc" (cn' "calculation")

drasil-data/lib/Data/Drasil/Concepts/Documentation.hs
327:srsDom = dcc "srsDom" (srs ^. term) "srs"
331:goalStmtDom   = ccs (mkIdea "goalStmtDom"   (goalStmt ^. term)                 $ Just "GS")       EmptyS [srsDom]
332:assumpDom     = ccs (mkIdea "assumpDom"     (assumption ^. term)               $ Just "A")        EmptyS [srsDom]
333:reqDom        = ccs (mkIdea "reqDom"        (requirement ^. term)              $ Just "R")        EmptyS [srsDom]
334:funcReqDom    = ccs (mkIdea "funcReqDom"    (functionalRequirement ^. term)    $ Just "FR")       EmptyS [reqDom]
335:nonFuncReqDom = ccs (mkIdea "nonFuncReqDom" (nonfunctionalRequirement ^. term) $ Just "NFR")      EmptyS [reqDom]
336:chgProbDom    = ccs (nc "chgProbDom" $ cn' "change")                                              EmptyS [srsDom]
337:likeChgDom    = ccs (mkIdea "likeChgDom"    (likelyChg ^. term)                $ Just "LC")       EmptyS [chgProbDom]
338:unlikeChgDom  = ccs (mkIdea "unlikeChgDom"  (unlikelyChg ^. term)              $ Just "UC")       EmptyS [chgProbDom]
339:refByDom      = ccs (mkIdea "refByDom"      (refBy ^. term)                    $ Just "RefBy")    EmptyS [srsDom]
340:refNameDom    = ccs (mkIdea "refNameDom"    (refName ^. term)                  $ Just "RefName")  EmptyS [srsDom]

drasil-data/lib/Data/Drasil/Concepts/Math.hs
32:amplitude   = dcc "amplitude"    (nounPhraseSP "amplitude")      "The peak deviation of a function from zero"
33:angle       = dcc "angle"        (cn' "angle")                   "the amount of rotation needed to bring one line or plane into coincidence with another"
34:area        = dcc "area"         (cn' "area")                    "a part of an object or surface"
35:axis        = dcc "axis"         (cn' "axis")                    "a fixed reference line for the measurement of coordinates" 
36:calculation = dcc "calculation"  (cn' "calculation")             "a mathematical determination of the size or number of something"
37:cartesian   = dccWDS "cartesian" (pn' "Cartesian coordinate system") $ S "a coordinate system that specifies each point uniquely in a plane by a set" `S.of_`
41:centre       = dcc "centre"       (cn' "centre")                  "the middle point of an object"
42:change       = dcc "change"       (cn' "change")                  "Difference between relative start and end states of an object"
43:component    = dcc "component"    (nounPhrase "component" "components") ("The scalar quantity defining the contribution " ++
45:constraint   = dcc "constraint"   (cn' "constraint")              "A condition that the solution must satisfy"
46:diameter     = dcc "diameter"     (cn' "diameter")                ("Any straight line segment that passes through the center of the circle" ++
48:direction    = dcc "direction"    (cn' "direction")               "'which way' a vector points, extending from the tail to the tip"
49:equation     = dcc "equation"     (cn' "equation")                "A statement that the values of two mathematical expressions are equal "
50:euclidSpace  = dcc "euclidSpace"  (cn' "Euclidean")               ("Denoting the system of geometry corresponding to the geometry of ordinary" ++
52:gradient     = dcc "gradient"     (cn' "gradient")                "degree of steepness of a graph at any point"
53:graph        = dcc "graph"        (cn' "graph")                   "A diagram showing the relation between variable quantities"
54:laplaceTransform = dcc "laplaceTransform" (cn' "laplace transform") ("An integral transform that converts a function of a real variable t " ++
56:law          = dcc "law"          (cn' "law")                     "a generalization based on a fact or event perceived to be recurrent"
57:line         = dccWDS "line"      (pn' "line")                    $ S "An interval between two points" +:+
59:matrix       = dcc "matrix"       (cnICES "matrix")               ("A rectangular array of quantities or expressions in rows and columns that" ++
61:norm        = dcc "norm"         (cn' "norm")                    "the positive length or size of a vector"
62:normal      = dcc "normal"       (cn' "normal" )                 "an object that is perpendicular to a given object"
63:number      = dcc "number"       (cn' "number")                  "a mathematical object used to count, measure, and label"
64:orient      = dcc "orientation"  (cn' "orientation")             "the relative physical position or direction of something"
65:origin      = dcc "origin"       (cn' "origin")                  "a fixed point of reference for the geometry of the surrounding space"
66:parameter   = dcc "parameter"    (cn' "parameter")               "a quantity whose value is selected depending on particular circumstances"
68:perp         = dcc "perp"         (cn' "perpendicular")          "At right angles"
69:pi_          = dcc "pi"           (cn' "ratio of circumference to diameter for any circle") "The ratio of a circle's circumference to its diameter"
70:posInf       = dcc "PosInf"       (cn' "Positive Infinity")      "the limit of a sequence or function that eventually exceeds any prescribed bound"
71:negInf       = dcc "NegInf"       (cn' "Negative Infinity")      "Opposite of positive infinity"
72:positive     = dcc "positive"     (cn' "positive")               "greater than zero"
73:negative     = dcc "negative"     (cn' "negative")               "less than zero"
74:point        = dccWDS "point"     (pn' "point")                   $ S "An exact location, it has no size, only position" +:+
76:probability  = dcc "probability"  (cnIES "probability")          "The likelihood of an event to occur"
77:rate         = dcc "rate"         (cn' "rate")                   "Ratio that compares two quantities having different units of measure"
78:rightHand    = dcc "rightHand"    (cn' "right-handed coordinate system")  "A coordinate system where the positive z-axis comes out of the screen."
79:shape        = dcc "shape"        (cn' "shape")                  "The outline of an area or figure"
80:surface      = dcc "surface"      (cn' "surface")                "The outer or topmost boundary of an object"
81:unit_        = dcc "unit"         (cn' "unit")                   "Identity element"
82:vector       = dcc "vector"       (cn' "vector")                 "Object with magnitude and direction"
84:xAxis = dcc "xAxis" (nounPhraseSent $ P lX :+: S "-axis") "the primary axis of a system of coordinates"
85:yAxis = dcc "yAxis" (nounPhraseSent $ P lY :+: S "-axis") "the secondary axis of a system of coordinates"
86:zAxis = dcc "zAxis" (nounPhraseSent $ P lZ :+: S "-axis") "the tertiary axis of a system of coordinates"
88:xCoord = dcc "xCoord" (nounPhraseSent $ P lX :+: S "-coordinate") "the location of the point on the x-axis"
89:yCoord = dcc "yCoord" (nounPhraseSent $ P lY :+: S "-coordinate") "the location of the point on the y-axis"
90:zCoord = dcc "zCoord" (nounPhraseSent $ P lZ :+: S "-coordinate") "the location of the point on the z-axis"
92:xComp = dcc "xComp" (nounPhraseSent $ P lX :+: S "-component") "the component of a vector in the x-direction"
93:yComp = dcc "yComp" (nounPhraseSent $ P lY :+: S "-component") "the component of a vector in the y-direction"
94:zComp = dcc "zComp" (nounPhraseSent $ P lZ :+: S "-component") "the component of a vector in the z-direction"
96:xDir = dcc "xDir" (nounPhraseSent $ P lX :+: S "-direction") "the direction aligned with the x-axis"
97:yDir = dcc "yDir" (nounPhraseSent $ P lY :+: S "-direction") "the direction aligned with the y-axis"
98:zDir = dcc "zDir" (nounPhraseSent $ P lZ :+: S "-direction") "the direction aligned with the z-axis"
99:iAngle = dcc "iAngle" (cn "initial angle")                      "The initial angle where the body is being displaced"
112:euclidN = dcc "euclidNorm"    (combineNINI euclidSpace norm) "euclidean norm"
113:normalV = dcc "normal vector" (combineNINI normal vector) "unit outward normal vector for a surface"
114:perpV   = dcc "perp_vect"     (combineNINI perp vector) "vector perpendicular or 90 degrees to another vector"
115:rOfChng = dcc "rOfChng"       (rate `of_` change) "ratio between a change in one variable relative to a corresponding change in another"
116:surArea = dcc "surArea"       (combineNINI surface area) "a measure of the total area that the surface of the object occupies"
117:unitV   = dcc "unit_vect"     (combineNINI unit_ vector) "a vector that has a magnitude of one"

drasil-data/lib/Data/Drasil/Concepts/PhysicalProperties.hs
20:gaseous    = dcc "gaseous"    (cn''' "gas"          ) "gaseous state"
21:liquid     = dcc "liquid"     (cn' "liquid"         ) "liquid state"
22:solid      = dcc "solid"      (cn' "solid"          ) "solid state"
23:ctrOfMass  = dcc "ctrOfMass"  (centre `of_PS` mass  ) "the mean location of the distribution of mass of the object"
24:dimension  = dcc "dimension"  (cn' "dimension"      ) "any of a set of basic kinds of quantity, as mass, length, and time"
25:density    = dcc "density"    (cnIES "density"      ) "the mass per unit volume"
26:specWeight = dcc "specWeight" (cn' "specific weight") "the weight per unit volume"
27:flexure    = dcc "flexure"    (cn' "flexure"        ) "a bent or curved part"
28:len        = dcc "length"     (cn' "length"         ) ("the straight-line distance between two points along an object, " ++
30:mass       = dcc "mass"       (cn''' "mass"         ) "the quantity of matter in a body"
31:vol        = dcc "volume"     (cn' "volume"         ) "the amount of space that a substance or object occupies"

drasil-data/lib/Data/Drasil/Concepts/Physics.hs
59:acceleration = dccWDS "acceleration" (cn' "acceleration")
61:angular = dcc "angular" (cn' "angular")
63:body = dccWDS "body" (cnIES "body")
65:chgInVelocity = dccWDS "chgInVelocity" (cn desc)
68:chgMomentum = dccWDS "chgMomentum" (cn' "change in momentum")
70:collision = dcc "collision" (cn' "collision")
72:cohesion = dccWDS "cohesion" (cn "cohesion")
74:compression = dccWDS "compression" (cn' "compression")
76:damping = dccWDS "damping" (pn' "damping")
79:dampingCoeff = dcc "dampingCoeff" (cn' "damping coefficient")
81:displacement = dccWDS "displacement" (cn' "displacement")
83:distance = dcc "distance" (cn' "distance")
85:elasticity = dcc "elasticity" (cnIES "elasticity")
87:energy = dcc "energy" (cn "energy")
89:fbd = dcc "FBD" (cn' "free body diagram")
92:force = dcc "force" (cn' "force")
94:frequency = dcc "frequency" (cn' "frequency")
96:friction = dcc "friction" (cn' "friction")
98:fOfGravity = dcc "fOfGravity" (cn "force of gravity")
100:gravity = dcc "gravity" (cn "gravity")
102:gravitationalAccel = dcc "gravitationalAccel" (cn "gravitational acceleration")
104:gravitationalConst = dcc "gravitationalConst" (cn "gravitational constant")
106:gravitationalMagnitude = dcc "gravitationalMagnitude" (cn "magnitude of gravitational acceleration")
108:height = dccWDS "height" (cn' "height")
110:horizontalMotion = dccWDS "horizontalMotion" (cn "horizontal motion")
112:isotropy = dccWDS "isotropy" (cn "isotropy")
115:joint = dcc "joint" (cn' "joint")
117:kEnergy = dccWDS "kEnergy" (cn "kinetic energy")
119:kinematics = dccWDS "kinematics" (cn "kinematics")
122:linear = dcc "linear" (cn' "linear")
124:mechEnergy = dcc "mechEnergy" (cn "mechanical energy")
126:momentum = dccWDS "momentum" (cn "momentum")
129:moment = dccWDS "moment" (cn' "moment")
131:motion = dccWDS "motion" (cn "motion")
133:period = dccWDS "period" (cn' "period")
135:pendulum = dccWDS "pendulum" (cn "pendulum")
138:position = dcc "position" (cn' "position")
140:positionVec = dccWDS "positionVec" (cn' "position vector")
143:potEnergy = dccWDS "potEnergy" (cn "potential energy")
145:pressure = dccWDS "pressure" (cn' "pressure")
147:rectilinear = dcc "rectilinear" (cn "rectilinear")
149:rigidBody = dcc "rigidBody" (cnIES "rigid body")
151:space = dcc "space" (cn' "space")
153:scalarAccel = dccWDS "scalarAccel" (cn' "scalar acceleration")
155:scalarPos = dccWDS "scalarPos" (cn' "scalar position")
157:shm = dcc "SHM" (nounPhraseSP "simple harmonic motion") ("Periodic motion through an equilibrium position. " ++ 
160:speed = dccWDS "speed" (cn' "speed")
162:stiffCoeff = dcc "stiffnessCoeff" (cn' "stiffness coefficient") 
164:strain = dccWDS "strain" (cn' "strain") 
168:stress = dcc "stress" (cn''' "stress")
171:tension = dccWDS "tension" (cn' "tension")
173:time = dcc "time" (cn' "time")
175:torque = dcc "torque" (cn' "torque")
177:velocity = dccWDS "velocity" (cnIES "velocity")
179:verticalMotion = dccWDS "verticalMotion" (cn "vertical motion")
181:weight = dcc "weight" (cn' "weight")
189:xDist = dccWDS "xDist" (distance `inThe` xDir) (atStartNP $ distance `inThe` xDir)
190:yDist = dccWDS "yDist" (distance `inThe` yDir) (atStartNP $ distance `inThe` yDir)
192:iPos = dccWDS "iPos" (cn "initial position") (S "The" +:+ phrase position +:+ S "at the body's initial point")
193:xPos = dccWDS "xPos" (xComp `of_` position) (atStartNP $ NP.the $ xComp `of_` position)
194:yPos = dccWDS "yPos" (yComp `of_` position) (atStartNP $ NP.the $ yComp `of_` position)
196:ixPos = dccWDS "ixPos" (xComp `of_` iPos) (atStartNP $ NP.the $ xComp `of_` iPos)
197:iyPos = dccWDS "iyPos" (yComp `of_` iPos) (atStartNP $ NP.the $ yComp `of_` iPos)
199:fSpeed = dccWDS "fSpeed" (cn "final speed")   (S "The" +:+ phrase speed +:+ S "at the body's final point")
200:iSpeed = dccWDS "iSpeed" (cn "initial speed") (S "The" +:+ phrase speed +:+ S "at the body's initial point")
202:ixSpeed = dccWDS "ixSpeed" (xComp `of_` iSpeed) (atStartNP $ NP.the $ xComp `of_` iSpeed)
203:iySpeed = dccWDS "iySpeed" (yComp `of_` iSpeed) (atStartNP $ NP.the $ yComp `of_` iSpeed)
205:fVel = dccWDS "fVel" (cn "final velocity")   (S "The" +:+ phrase velocity +:+ S "at the body's final point")
206:iVel = dccWDS "iVel" (cn "initial velocity") (S "The" +:+ phrase velocity +:+ S "at the body's initial point")
207:xVel = dccWDS "xVel" (xComp `of_` velocity) (atStartNP $ NP.the $ xComp `of_` velocity)
208:yVel = dccWDS "yVel" (yComp `of_` velocity) (atStartNP $ NP.the $ yComp `of_` velocity)
210:ixVel = dccWDS "ixVel" (xComp `of_` iVel) (atStartNP $ NP.the $ xComp `of_` iVel)
211:iyVel = dccWDS "iyVel" (yComp `of_` iVel) (atStartNP $ NP.the $ yComp `of_` iVel)
213:xAccel = dccWDS "xScalAcc" (xComp `of_` acceleration) (atStartNP $ NP.the $ xComp `of_` acceleration)
214:yAccel = dccWDS "yScalAcc" (yComp `of_` acceleration) (atStartNP $ NP.the $ yComp `of_` acceleration)
216:constAccelV = dccWDS "constAccelV" (cn "constant acceleration vector") (S "The" +:+ phrase constAccel +:+ S "vector")
217:xConstAccel = dccWDS "xConstAccel" (xComp `of_` constAccel) (atStartNP $ NP.the $ xComp `of_` constAccel)
218:yConstAccel = dccWDS "yConstAccel" (yComp `of_` constAccel) (atStartNP $ NP.the $ yComp `of_` constAccel)
223:angDisp = dcc "angularDisplacement" (combineNINI angular displacement)
225:angVelo = dcc "angularVelocity" (combineNINI angular velocity)
227:angAccel = dcc "angularAcceleration" (combineNINI angular acceleration)
229:constAccel = dcc "constantAcceleration" (cn "constant acceleration")
231:linDisp = dcc "linearDisplacement" (combineNINI linear displacement) 
233:linVelo = dcc "linearVelocity" (combineNINI linear velocity) 
235:linAccel = dcc "linearAcceleration" (combineNINI linear acceleration) 
241:restitutionCoef = dcc "restitutionCoef" (cn "coefficient of restitution")
243:momentOfInertia = dcc "momentOfInertia" (cn "moment of inertia")
245:angFreq = dcc "angularFrequency" (cn "angular frequency")
248:impulseV = dcc "impulseV" (cn "impulse (vector)")
250:impulseS = dcc "impulseS" (cn "impulse (scalar)")

drasil-data/lib/Data/Drasil/Concepts/Software.hs
22:c       = dcc "c" (pn "C") 
24:physLib = dcc "physLib" (cnIES "physics library") 
26:program = dcc "program" (cn' "program")
28:errMsg  = dcc "errMsg" (cn' "error message") 
39:accuracy          = dcc "accuracy"          (nounPhraseSP "accuracy")
42:correctness       = dcc "correctness"       (nounPhraseSP "correctness")
45:maintainability   = dcc "maintainability"   (nounPhraseSP "maintainability")
48:performance       = dcc "performance"       (nounPhraseSP "performance")
51:performanceSpd    = dcc "performanceSpd"    (cn' "performance speed")
54:portability       = dcc "portability"       (nounPhraseSP "portability")
57:reliability       = dcc "reliability"       (nounPhraseSP "reliability")
61:reusability       = dcc "reusability"       (nounPhraseSP "reusability")
64:understandability = dcc "understandability" (nounPhraseSP "understandability")
67:verifiability     = dcc "verifiability"     (nounPhraseSP "verifiability")
76:hwHiding = dcc "hwHiding" (cn "hardware hiding")
81:modBehavHiding = dccWDS "modBehavHiding" (cn "behaviour hiding") (foldlSent_
89:modControl = dcc "modControl" (cn' "control module") "provides the main program"
92:modSfwrDecision = dccWDS "modSfwrDecision" (cn' "software decision module") (foldlSent_
97:modInputFormat = dcc "modInputFormat" (cn' "input format module")
101:modInputParam = dccWDS "modInputParam" (cn' "input parameter module") (foldlSent_
107:modInputConstraint = dcc "modInputConstraint" (cn' "input constraint module") 
112:modInputVerif = dccWDS "modInputVerif" (cn' "input verification module") (foldlSent
119:modDerivedVal = dccWDS "modDerivedVal" (cn' "derived value module") (foldlSent_
124:modInterpolation = dccWDS "modInterpolation" (cn "interpolation module") (foldlSent_
129:modInterpDatum = dccWDS "modInterpDatum" (cn "interpolation datum module") (foldlSent_
136:modSeqServ = dccWDS "modSeqServ" (cn' "sequence data structure")
141:modLinkedServ = dccWDS "modLinkedServ" (cn' "linked data structure")
146:modAssocServ = dccWDS "modAssocServ" (cn' "associative data structure")
151:modVectorServ = dccWDS "modVectorServ" (cn' "vector")
156:modPlotDesc = dcc "modPlotDesc" (cn' "plotting") "provides a plot function"
159:modOutputfDescFun desc = dccWDS "modOutputfDescFun" (cn' "output format")
164:modOdeDesc = dccWDS "modOdeDesc" (nounPhraseSP "ODE solver")

drasil-data/lib/Data/Drasil/Concepts/SolidMechanics.hs
17:elastMod   = dccWDS "elastMod" (cn "elastic modulus") 
21:mobShear   = dccWDS "mobShear" (cn "mobilized shear force") 
25:normForce  = dccWDS "normForce" (cn' "normal force")
28:nrmStrss   = dccWDS "nrmStrss" (cn "normal stress") 
31:poissnsR   = dccWDS "poissnsR" (nounPhraseSP "Poisson's ratio") 
34:shearRes   = dccWDS "shearRes" (cn "resistive shear force") 
38:shearForce = dccWDS "shearForce" (cn' "shear force")
41:stffness   = dccWDS "stffness" (cn "stiffness") 

drasil-data/lib/Data/Drasil/Concepts/Thermodynamics.hs
25:boiling           = dcc "boiling"           (cn "boiling")
27:boilPt            = dcc "boilPt"           (cn' "boiling point temperature")
29:degree_'          = dcc "degree"            (cn' "degree")
31:heat              = dcc "heat"              (cn "heat")
34:heatTrans         = dcc "heatTrans"         (cn' "heat transfer")
37:heatCapSpec       = dcc "heatCapSpec"       (cnIES "specific heat capacity")
40:htFlux            = dcc "htFlux"            (cn'' "heat flux") 
43:latentHeat        = dcc "latentHeat"        (cn' "latent heat")
46:lawConsEnergy     = dcc "lawConsEnergy"     (nounPhraseSP "law of conservation of energy")
48:lawConvCooling    = dcc "lawConvCooling"    (nounPhraseSP "Newton's law of cooling")
50:melting           = dcc "melting"           (cn "melting")
52:meltPt            = dcc "meltPt"            (cn' "melting point temperature")
54:phaseChange       = dcc "phaseChange"       (cn' "phase change")
56:sensHeat          = dcc "sensHeat"          (cn' "sensible heat")
60:temp              = dcc "temperature"       (cn' "temperature")
62:thermalAnalysis   = dcc "thermalAnalysis"
65:thermalConduction = dcc "thermalConduction" (nounPhraseSP "thermal conduction")
67:thermalConductor  = dcc "thermalConductor"  (cn' "thermal conductor")
69:thermalEnergy     = dcc "thermalEnergy"     (cnIES "thermal energy")
72:enerSrc           = dcc "enerSrc"     (combineNINI energy source)
74:htTransTheo       = dcc "htTransTheo" (combineNINI heatTrans theory)

drasil-example/dblpend/lib/Drasil/DblPend/Body.hs
153:  (map cw iMods ++ srsDomains) (map unitWrapper [metre, second, newton, kilogram, degree, radian, hertz])

drasil-example/dblpend/lib/Drasil/DblPend/Concepts.hs
42:arcLen = dcc "arc length" (nounPhraseSP "arc length") "the distance between two points on a curve"

drasil-example/gamephysics/lib/Drasil/GamePhysics/Body.hs
153:  (map cw defSymbols ++ srsDomains ++ map cw iMods) units dataDefs

drasil-example/glassbr/lib/Drasil/GlassBR/Body.hs
143:  (map cw symb ++ terms ++ Doc.srsDomains) (map unitWrapper [metre, second, kilogram]

drasil-example/glassbr/lib/Drasil/GlassBR/Unitals.hs
289:annealedGl    = cc' annealed
292:aspectRatioCon   = cc aR
299:blast         = dcc "blast"       (cn' "blast")
301:blastResisGla = dcc "blastResisGla"    (nounPhraseSP "blast resistant glazing")
303:blastTy       = dcc "blastTy"     (cn' "blast type")
306:bomb          = dcc "bomb"        (cn' "bomb") ("a container filled " ++
308:capacity      = dcc "capacity"    (nounPhraseSP "capacity or load resistance")
310:demandq       = dcc "demandq"     (nounPhraseSP "applied load (demand)")
312:eqTNTChar     = dcc "eqTNTChar"   (nounPhraseSP "equivalent TNT charge mass")
314:explosion     = dcc "explosion"   (cn' "explosion")
316:fTemperedGl   = cc' fullyT
321:glassGeo      = dccWDS "glassGeo"    (cnIES "glass geometry")
324:glassTy       = dcc "glassTy"     (cn' "glass type") "type of glass"
325:glassWL       = dcc "glassWL"     (nounPhraseSP "glass weight load")
327:glBreakage    = dcc "glBreakage"  (nounPhraseSP "glass breakage")
329:glTyFac       = cc' glassTypeFac
334:hStrengthGl   = cc' heatS
339:lateral       = dcc "lateral"     (nounPhraseSP "lateral")
341:lite          = dcc "lite"        (cn' "lite")
343:load          = dcc "load"        (nounPhraseSP "applied load (demand) or pressure")
345:loadResis     = cc' lResistance
349:loadShareFac  = cc' lShareFac
354:longDurLoad   = dcc "longDurLoad"        (nounPhraseSP "long duration load")
356:nonFactoredL  = cc' nFL
360:notSafe       = dcc "notSafe"     (nounPhraseSP "not safe")
362:probBreak     = cc' probBr
366:safeMessage   = dcc "safeMessage" (nounPhraseSP "safe")
368:sD            = cc' stdOffDist
371:shortDurLoad  = dcc "shortDurLoad"       (nounPhraseSP "short duration load")
373:specA         = dcc "specA"       (nounPhraseSP "specifying authority")
379:specDeLoad    = dcc "specDeLoad"  (nounPhraseSP "specified design load")

drasil-example/pdcontroller/lib/Drasil/PDController/Body.hs
146:  (map cw inpConstrained ++ srsDomains)

drasil-example/pdcontroller/lib/Drasil/PDController/Concepts.hs
30:  = dcc "pdCtrlLoop" (nounPhraseSP "PD Control Loop") ("Closed-Loop control " ++
34:  = dcc "pdController" (nounPhraseSP "PD Controller") 
38:  = dcc "summingPoint" (nounPhraseSP "Summing Point") ("Control block where " ++
43:  = dcc "powerPlant" (nounPhraseSP "Power Plant") 
47:  = dcc "secondOrderSystem" (nounPhraseSP "Second Order System") 
52:  = dcc "processError" (nounPhraseSP "Process Error") 
56:stepTime = dcc "stepTime" (nounPhraseSP "Step Time") "Simulation step time"
59:  = dcc "simulationTime" (nounPhraseSP "Simulation Time") 
63:  = dcc "processVariable" (nounPhraseSP "Process Variable") 
67:  = dcc "controlVariable" (nounPhraseSP "Control Variable") 
71:  = dcc "setPoint" (nounPhraseSP "Set-Point") 
76:  = dcc "propGain" (nounPhraseSP "Proportional Gain") 
80:  = dcc "derGain" (nounPhraseSP "Derivative Gain") 
84:  = dcc "propControl" (nounPhraseSP "Proportional control")
89:  = dcc "derControl" (nounPhraseSP "Derivative control")
94:  = dcc "simulation" (cn' "simulation") 
98:  = dcc "frequencyDomain" (nounPhraseSP "frequency domain") 
103:  = dcc "timeDomain" (nounPhraseSP "time domain")
107:  = dcc "laplaceTransform" (cn' "Laplace transform") 
112:  = dcc "absoluteTolerance" (nounPhraseSP "Absolute Tolerance") 
116:  = dcc "relativeTolerance" (nounPhraseSP "Relative Tolerance") 
120:  = dcc "transferFxn" (nounPhraseSP "Transfer Function")
125:  = dcc "dampingCoeff" (nounPhraseSP "Damping Coefficient")
129:  = dcc "stiffnessCoeff" (nounPhraseSP "Stiffness Coefficient")

drasil-example/projectile/lib/Drasil/Projectile/Body.hs
178:  (cw pi_ : map cw constrained ++ srsDomains) (map unitWrapper [metre, radian, second]) 

drasil-example/projectile/lib/Drasil/Projectile/Concepts.hs
37:launcher   = dcc "launcher"   (nounPhraseSP "launcher")  ("where the projectile is launched from " ++
39:projectile = dcc "projectile" (nounPhraseSP "projectile") "the object to be launched at the target"
40:target     = dcc "target"     (nounPhraseSP "target")     "where the projectile should be launched to"
42:projSpeed  = dccWDS "projSpeed" (nounPhraseSP "1D speed")    (getAcc oneD +:+ phrase speed +:+ S "under" +:+ phrase constant +:+ phrase acceleration)
43:projPos    = dccWDS "projPos"   (nounPhraseSP "1D position") (getAcc oneD +:+ phrase position +:+ S "under" +:+ phrase constant +:+ phrase speed)
46:landPos = cc' landingPosNC
50:launAngle = cc' launchAngleNC
54:launSpeed = cc' launchSpeedNC (phraseNP (iSpeed `the_ofThe` projectile) +:+ S "when launched")
55:offset = cc' offsetNC (S "the offset between the" +:+ phraseNP (targetPosNC `andThe` landingPosNC))
56:targPos = cc' targetPosNC (phraseNP (the distance) `S.fromThe` phraseNP (launcher `toThe` target))
57:flightDur = cc' flightDurNC (foldlSent_ [phraseNP (the time), S "when the", phrase projectile, S "lands"])

drasil-example/sglpend/lib/Drasil/SglPend/Body.hs
141:  (map cw iMods ++ srsDomains) (map unitWrapper [metre, second, newton, kilogram, degree, radian, hertz]) dataDefs

drasil-example/ssp/lib/Drasil/SSP/Body.hs
164:  (map cw SSP.iMods ++ map cw symbols ++ srsDomains) units SSP.dataDefs SSP.iMods

drasil-example/ssp/lib/Drasil/SSP/Defs.hs
63:effFandS = dccWDS "effective forces and stresses" 
70:slpSrf = dccWDS "slip surface" (cn' "slip surface")
75:plnStrn = dccWDS "plane strain" (cn' "plane strain") 
85:crtSlpSrf = dccWDS "critical slip surface" (cn' "critical slip surface") 
90:fsConcept = dccWDS "FS" factorOfSafety
97:waterTable = dcc "water table" (cn' "water table") ("The upper boundary of a" ++

drasil-example/swhs/lib/Drasil/SWHS/Body.hs
117:  (cw heatEInPCM : map cw symbols ++ srsDomains ++ map cw specParamValList) -- FIXME: heatEInPCM?
377:terms = map cw [htFlux, phaseChangeMaterial, cw heatCapSpec, thermalConduction, transient]

drasil-example/swhs/lib/Drasil/SWHS/Concepts.hs
46:charging = dcc "charging" (nounPhraseSP "charging") "charging of the tank"
48:coil = dcc "coil" (cn' "heating coil")
51:discharging = dcc "discharging" (nounPhraseSP "discharging")
54:transient = dcc "transient" (nounPhraseSP "transient") "changing with time"
56:gaussDiv = dcc "gaussDiv" (nounPhraseSP "gauss's divergence theorem")
61:perfectInsul = dcc "perfectInsul" (nounPhraseSP "perfectly insulated")
65:phaseChangeMaterial = dcc "pcm" (phsChgMtrl ^. term)
69:tankParam = dcc "tankParam" (compoundPhrase' (tank ^. term)
73:tank  = dcc "tank"  (cn' "tank") "enclosure containing some kind of substance"
74:sWHT  = dcc "sWHT"  (cn' "solar water heating tank") "solar water heating tank"
75:water = dcc "water" (cn' "water") "the liquid with which the tank is filled"
77:tankPCM = dcc "tankPCM" (nounPhrase''

drasil-example/swhs/lib/Drasil/SWHS/TMods.hs
48:  where consCC = dccWDS "consThermECS"

drasil-example/swhsnopcm/lib/Drasil/SWHSNoPCM/Body.hs
212:  (map cw symbols ++ srsDomains) units NoPCM.dataDefs NoPCM.iMods genDefs

drasil-lang/lib/Language/Drasil/Chunk/DefinedQuantity.hs
82:dqdQd c cc = DQD cc (symbol c) (c ^. typ) (getUnit c)

drasil-lang/lib/Language/Drasil/Chunk/UnitDefn.hs
118:unitCon s = dcc s (cn' s) s

drasil-theory/lib/Theory/Drasil/Theory.hs
120:  TM mkind [] [] (map qw q) (map cw c) dq inv dfn r (shortname' $ S lbe)
128:  TM mkind [] [] (map qw q) (map cw c) dq inv dfn [] (shortname' $ S lbe)
```

</details>
