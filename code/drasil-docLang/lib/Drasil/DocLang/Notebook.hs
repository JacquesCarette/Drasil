module Drasil.DocLang.Notebook where

import Language.Drasil

import qualified Data.Drasil.Concepts.Documentation as Doc (introduction, prpsOfDoc, 
  review, body, mainIdea, procForAnls, summary, methAndAnls, coordinateSystem, 
  example, appendix, reference)
import qualified Data.Drasil.Concepts.Physics as P (motion, horizontalMotion, verticalMotion, kinematics)

--  Notebook constructor. 
-- Create the notebook from given system name, authors, and sections
--doc :: NamedIdea c => c -> Sentence -> [Section] -> Document
--doc  sys = Document (Doc.notebook `S.forTPS` sys)

-- * Section Constructors

-- | Section constructors for the lesson plan documents/jupyter notebooks.
intro, prpsOfDoc, body, review, mainIdea, motion, hormotion, vermotion, methAndAnls,
  coorSyst, kinematic, procForAnls, summary, appendix, reference, example :: String -> Int -> [Contents] -> Section
  
intro       p d cs = section (mkUid p) d (titleize Doc.introduction)     cs introLabel
prpsOfDoc   p d cs = section (mkUid p) d (titleize Doc.prpsOfDoc)        cs docPurposeLabel

body        p d cs = section (mkUid p) d (titleize Doc.body)             cs bodyLabel
review      p d cs = section (mkUid p) d (titleize Doc.review)           cs reviewLabel

mainIdea    p d cs = section (mkUid p) d (titleize Doc.mainIdea)         cs mainIdeaLabel
motion      p d cs = section (mkUid p) d (titleize P.motion)             cs motionLabel
hormotion   p d cs = section (mkUid p) d (titleize P.horizontalMotion)   cs hormotionLabel
vermotion   p d cs = section (mkUid p) d (titleize P.verticalMotion)     cs vermotionLabel

methAndAnls p d cs = section (mkUid p) d (titleize' Doc.methAndAnls)     cs methsAndanlsLabel

summary     p d cs = section (mkUid p) d (titleize Doc.summary)          cs summaryLabel

procForAnls p d cs = section (mkUid p) d (titleize Doc.procForAnls)     cs anlsProcLabel
coorSyst    p d cs = section (mkUid p) d (titleize Doc.coordinateSystem) cs coorSystLabel
kinematic   p d cs = section (mkUid p) d (titleize P.kinematics)         cs kinematicLabel

appendix    p d cs = section (mkUid p) d (titleize Doc.appendix)         cs appendixLabel

reference   p d cs = section (mkUid p) d (titleize' Doc.reference)       cs referenceLabel
example     p d cs = section (mkUid p) d (titleize Doc.example)          cs exampleLabel

--Labels--
sectionReferences :: [Reference]
sectionReferences = [introLabel, docPurposeLabel, methsAndanlsLabel, referenceLabel,
  bodyLabel, reviewLabel, mainIdeaLabel, motionLabel, hormotionLabel, vermotionLabel, 
  appendixLabel, coorSystLabel, kinematicLabel, summaryLabel, anlsProcLabel, exampleLabel]

-- * Section References

-- | Individual section reference labels. Used in creating example sections for the notebook.
introLabel, docPurposeLabel, methsAndanlsLabel, referenceLabel, bodyLabel,
  reviewLabel, mainIdeaLabel, motionLabel, hormotionLabel, vermotionLabel, 
  appendixLabel, coorSystLabel, kinematicLabel, summaryLabel, anlsProcLabel, exampleLabel :: Reference
introLabel          = makeSecRef "Intro"            $ titleize Doc.introduction
docPurposeLabel     = makeSecRef "DocPurpose"       $ titleize Doc.prpsOfDoc
methsAndanlsLabel   = makeSecRef "MethsAndAnls"     $ titleize' Doc.methAndAnls
referenceLabel      = makeSecRef "References"       $ titleize' Doc.reference
bodyLabel           = makeSecRef "Body"             $ titleize Doc.body
reviewLabel         = makeSecRef "Review"           $ titleize Doc.review
mainIdeaLabel       = makeSecRef "MainIdea"         $ titleize Doc.mainIdea        
motionLabel         = makeSecRef "Motion"           $ titleize P.motion              
hormotionLabel      = makeSecRef "HorizontalMotion" $ titleize P.horizontalMotion
vermotionLabel      = makeSecRef "VerticalMotion"   $ titleize P.verticalMotion
appendixLabel       = makeSecRef "Appendix"         $ titleize Doc.appendix
coorSystLabel       = makeSecRef "CoordinateSystem" $ titleize Doc.coordinateSystem
kinematicLabel      = makeSecRef "Kinematic"        $ titleize P.kinematics
summaryLabel        = makeSecRef "Summary"          $ titleize Doc.summary
anlsProcLabel       = makeSecRef "AnlsProc"         $ titleize Doc.procForAnls
exampleLabel        = makeSecRef "Example"          $ titleize Doc.example