module Drasil.DocLang.Notebook where

import Language.Drasil

import qualified Data.Drasil.Concepts.Documentation as Doc (introduction, learnObj, 
  prpsOfDoc, review, body, mainIdea, procForAnls, summary, methAndAnls, coordinateSystem, 
  example, appendix, reference)
import qualified Data.Drasil.Concepts.Physics as P (motion, horizontalMotion, verticalMotion, kinematics)

--  Notebook constructor. 
-- Create the notebook from given system name, authors, and sections
--doc :: NamedIdea c => c -> Sentence -> [Section] -> Document
--doc  sys = Document (Doc.notebook `S.forTPS` sys)

-- * Section Constructors

-- | Section constructors for the lesson plan documents/jupyter notebooks.
-- should be fixed once we have a more concrete idea of the notebook structure
intro, learnObj, prpsOfDoc, body, review, mainIdea, motion, hormotion, vermotion, methAndAnls,
  coorSyst, kinematic, procForAnls, summary, appendix, reference, example :: [Contents] -> [Section] -> Section
intro       cs ss = section (titleize Doc.introduction)      cs ss introLabel
learnObj    cs ss = section (titleize' Doc.learnObj)          cs ss learnObjLabel
prpsOfDoc   cs ss = section (titleize Doc.prpsOfDoc)         cs ss docPurposeLabel

body        cs ss = section (titleize Doc.body)              cs ss bodyLabel
review      cs ss = section (titleize Doc.review)            cs ss reviewLabel

mainIdea    cs ss = section (titleize Doc.mainIdea)          cs ss mainIdeaLabel
motion      cs ss = section (titleize P.motion)              cs ss motionLabel
hormotion   cs ss = section (titleize P.horizontalMotion)    cs ss hormotionLabel
vermotion   cs ss = section (titleize P.verticalMotion)      cs ss vermotionLabel

methAndAnls cs ss = section (titleize' Doc.methAndAnls)      cs ss methsAndanlsLabel

summary     cs ss = section (titleize Doc.summary)           cs ss summaryLabel

procForAnls cs ss = section  (titleize Doc.procForAnls)      cs ss anlsProcLabel
coorSyst    cs ss = section  (titleize Doc.coordinateSystem) cs ss coorSystLabel
kinematic   cs ss = section  (titleize P.kinematics)         cs ss kinematicLabel

appendix    cs ss = section (titleize Doc.appendix)          cs ss appendixLabel

reference   cs ss = section (titleize' Doc.reference)        cs ss referenceLabel
example     cs ss = section (titleize Doc.example)           cs ss exampleLabel

--Labels--
sectionReferences :: [Reference]
sectionReferences = [introLabel, learnObjLabel, docPurposeLabel, methsAndanlsLabel, referenceLabel,
  bodyLabel, reviewLabel, mainIdeaLabel, motionLabel, hormotionLabel, vermotionLabel, 
  appendixLabel, coorSystLabel, kinematicLabel, summaryLabel, anlsProcLabel, exampleLabel]

-- * Section References

-- | Individual section reference labels. Used in creating example sections for the notebook.
introLabel, learnObjLabel, docPurposeLabel, methsAndanlsLabel, referenceLabel, 
  bodyLabel, reviewLabel, mainIdeaLabel, motionLabel, hormotionLabel, vermotionLabel, 
  appendixLabel, coorSystLabel, kinematicLabel, summaryLabel, anlsProcLabel, exampleLabel :: Reference
introLabel          = makeSecRef "Intro"            $ titleize Doc.introduction
learnObjLabel       = makeSecRef "LearnObj"         $ titleize' Doc.learnObj
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