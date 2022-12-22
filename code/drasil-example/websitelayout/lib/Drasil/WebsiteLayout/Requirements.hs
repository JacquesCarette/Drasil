module Drasil.WebsiteLayout.Requirements (funcReqs, nonfuncReqs, inReqDesc) where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators

import Data.Drasil.Concepts.Documentation (information, nonFuncReqDom, system, 
  funcReqDom)

{--Functional Requirements--}

funcReqs :: [ConceptInstance]
funcReqs = [introductionSec, gettingStartedSec, aboutSec, generatedExamplesSec, 
  caseStudiesSec, haddockDocSec, analysisDrasilSec]

introductionSec, gettingStartedSec, aboutSec, generatedExamplesSec, caseStudiesSec, 
  haddockDocSec, analysisDrasilSec:: ConceptInstance

introductionSec       = cic "introductionSec"       introductionSecDesc        
  "introduction-section"           funcReqDom
gettingStartedSec     = cic "gettingStartedSec"     gettingStartedSecDesc      
  "getting-started-section"        funcReqDom
aboutSec              = cic "aboutSec"              aboutSecDesc               
  "about-section"                  funcReqDom
generatedExamplesSec  = cic "generatedExamplesSec"  generatedExamplesSecDesc   
  "generated-examples-section"     funcReqDom
caseStudiesSec        = cic "caseStudiesSec"        caseStudiesSecDesc         
  "case-studies-section"           funcReqDom
haddockDocSec         = cic "haddockDocSec"         haddockDocSecDesc          
  "haddock-documentation-section"  funcReqDom
analysisDrasilSec     = cic "analysisDrasilSec"     analysisDrasilSecDesc      
  "analysis-drasil-section"        funcReqDom

introductionSecDesc, gettingStartedSecDesc, aboutSecDesc, generatedExamplesSecDesc,
  caseStudiesSecDesc, haddockDocSecDesc, analysisDrasilSecDesc :: Sentence

introductionSecDesc = foldlSent [atStartNP (the system), S "shall have an Introduction" +:+
  S "section", S "that serves as an elevator pitch for Drasil"]

gettingStartedSecDesc = foldlSent [atStartNP (the system), S "shall have a Getting Started" +:+
  S "section", S "linking to revelant wikis in GitHub"]

aboutSecDesc = foldlSent [atStartNP (the system), S "shall have an About section",
  S "describing Drasil, including the artifacts Drasil generates, and important Drasil concepts"]

generatedExamplesSecDesc = foldlSent [atStartNP (the system), S "shall have an Examples section",
  S "listing current Drasil examples"]

caseStudiesSecDesc = foldlSent [atStartNP (the system), S "shall have a Case Studies section",
  S "lisitng design decision for each Drasil example"]

haddockDocSecDesc = foldlSent [atStartNP (the system), S "shall have a Haddock section",
  S "that links to Drasil Haddock documentation"]

analysisDrasilSecDesc = foldlSent [atStartNP (the system), S "shall have an Analysis section",
  S "that contains graphs and tables that may be used to analyze the structure of the Drasil framework."]

inReqDesc :: Sentence
inReqDesc = S "!!!!! THERE ARE NO INPUTS !!!!!"

{--Nonfunctional Requirements--}

nonfuncReqs :: [ConceptInstance]
nonfuncReqs = [understandable, maintainable]

understandable :: ConceptInstance
understandable = cic "understandable" (foldlSent [
  atStartNP (the information), S "is modularized and presented in a logical order"
  ]) "Understandable" nonFuncReqDom

maintainable :: ConceptInstance
maintainable = cic "maintainable" (foldlSent [
  atStartNP (the information), S "is not repeated in the artifact"]) "Maintainable" 
  nonFuncReqDom

