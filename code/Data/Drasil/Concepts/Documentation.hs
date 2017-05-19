module Data.Drasil.Concepts.Documentation where

import Language.Drasil

import Data.Drasil.Concepts.Math (graph)

assumption, dataDefn, genDefn, goalStmt, inModel, likelyChg, physSyst,
  requirement, srs, thModel, mg, vav, desSpec, constraint :: CINP
--FIXME: Add compound NounPhrases instead of cn'
    --UPDATE: Added compoundPhrase where it could be applied. Verify that this is complete.
assumption  = commonINP "assumption"  (cn' "assumption")                            "A"
dataDefn    = commonINP "dataDefn"    (cn' "data definition")               "DD"
desSpec     = commonINP "desSpec"     (compoundPhrase design specification)           "DS"
genDefn     = commonINP "genDefn"     (cn' "general definition")                    "GD"
goalStmt    = commonINP "goalStmt"    (compoundPhrase goal statement)                "GS" 
inModel     = commonINP "inModel"     (compoundPhrase instance_ model)                "IM" 
likelyChg   = commonINP "likelyChg"   (cn' "likely change")                         "LC"
physSyst    = commonINP "physSyst"    (compoundPhrase physicalSystem description)     "PS" 
requirement = commonINP "requirement" (cn' "requirement")                           "R"
thModel     = commonINP "thModel"     (cn' "theoretical model")                     "T"
mg          = commonINP "mg"          (compoundPhrase module_ guide)                  "MG" 
srs         = commonINP "srs"         (compoundPhrase software reqSpec)               "SRS"
vav         = commonINP "vav"         (cn' "verification and validation")           "VAV"
constraint  = commonINP "constraint"  (cn' "constraint")                            "CSTR" -- FIXME: Eventually only have one constraint 

---------------------------------------------------------------------

-- concepts relating to the templates and their contents

analysis, appendix, characteristic, client, column, component, 
  condition, constraint_, connection, context, customer, datum, definition, 
  dependency, description, design, document, documentation, element, 
  environment, figure, functional, game, general, goal, guide, individual, information, 
  input_, instance_, intReader, introduction, item, label, library, limitation, method_,
  module_, model, name_, nonfunctional, offShelf, open, organization, 
  output_, performance, physics, physical, priority, problem, product_, project, 
  property, purpose, quantity, realtime, reference, requirement_, reviewer, 
  scope, source, section_, simulation, software, solution, specific, 
  specification, stakeholder, statement, symbol_, system, table_, template, 
  terminology, theory, traceyMatrix, user, useCase, value, variable, 
  video, verification, uncertainty :: NPNC

analysis        = npnc "analysis"       (cnIS "analysis")
appendix        = npnc "appendix"       (cnICES "appendix")
characteristic  = npnc "characteristic" (cn' "characteristic")
client          = npnc "client"         (cn' "client")
column          = npnc "column"         (cn' "column") --general enough to be in Documentation?
component       = npnc "component"      (cn' "component")
condition       = npnc "condition"      (cn' "condition")
constraint_     = npnc "constraint"     (cn' "constraint") -- FIXME: Eventually only have one constraint 
connection      = npnc "connection"     (cn' "connection")
context         = npnc "context"        (cn' "context")
customer        = npnc "customer"       (cn' "customer")
datum           = npnc "datum"          (cnUM  "datum")
definition      = npnc "definition"     (cn' "definition")
dependency      = npnc "dependency"     (cnIES "dependency")
description     = npnc "description"    (cn' "description")
design          = npnc "design"         (cn' "design")
document        = npnc "document"       (cn' "document")
documentation   = npnc "documentation"  (cn' "documentation")
element         = npnc "element"        (cn' "element")
environment     = npnc "environment"    (cn' "environment") -- Is this term in the right spot?
figure          = npnc "figure"         (cn' "figure")
functional      = npnc "functional"     (cn' "functional") --FIXME: Adjective
game            = npnc "game"           (cn' "game")
general         = npnc "general"        (cn' "general")  -- FIXME: Adjective
goal            = npnc "goal"           (cn' "goal")
guide           = npnc "guide"          (cn' "guide")
individual      = npnc "individual"     (cn "individual")
information     = npnc "information"    (cn "information")
input_          = npnc "input"          (cn' "input")
instance_       = npnc "instance"       (cn' "instance")
intReader       = npnc "intReader"      (cn "intended reader")
introduction    = npnc "introduction"   (cn' "introduction")
item            = npnc "item"           (cn' "item")
label           = npnc "label"          (cn' "label")
library         = npnc "library"        (cnIES "library")
limitation      = npnc "limitation"     (cn' "limitation")
method_         = npnc "method"         (cn' "method")
module_         = npnc "module"         (cn' "module")
model           = npnc "model"          (cn' "model")
name_           = npnc "name"           (cn' "name")
nonfunctional   = npnc "non-functional" (cn' "non-functional") -- FIXME: Adjective
offShelf        = npnc "Off-the-Shelf"  (cn' "Off-the-Shelf")
open            = npnc "open"           (cn' "open")
organization    = npnc "organization"   (cn' "organization")
output_         = npnc "output"         (cn' "output")
performance     = npnc "performance"    (cn' "performance")
physics         = npnc "physics"        (cn' "physics")
physical        = npnc "physical"       (cn' "physical") -- FIXME: Adjective
priority        = npnc "priority"       (cnIES "priority")
problem         = npnc "problem"        (cn' "problem")
product_        = npnc "product"        (cn' "product")
project         = npnc "project"        (cn' "project")
property        = npnc "property"       (cnIES "property")
purpose         = npnc "purpose"        (cn' "purpose")
quantity        = npnc "quantity"       (cnIES "quantity") --general enough to be in documentaion.hs?
reference       = npnc "reference"      (cn' "reference")
requirement_    = npnc "requirement"    (cn' "requirement") -- FIXME: Eventually only have one requirement
reviewer        = npnc "reviewer"       (cn' "reviewer") 
scope           = npnc "scope"          (cn' "scope")
source          = npnc "source"         (cn' "source")
section_        = npnc "section"        (cn' "section")
simulation      = npnc "simulation"     (cn' "simulation")
solution        = npnc "solution"       (cn' "solution")
software        = npnc "software"       (cn' "software")
specific        = npnc "specific"       (cn' "specific") -- FIXME: Adjective
specification   = npnc "specification"  (cn' "specification")
stakeholder     = npnc "stakeholder"    (cn' "stakeholder")
statement       = npnc "statement"      (cn' "statement")
symbol_         = npnc "symbol"         (cn' "symbol")
system          = npnc "system"         (cn' "system")
table_          = npnc "table"          (cn' "table")
template        = npnc "template"       (cn' "template")
terminology     = npnc "terminology"    (cnIES "terminology")
theory          = npnc "theory"         (cnIES "theory")
traceyMatrix    = npnc "traceyMatrix"   (cnICES "traceability matrix")
uncertainty     = npnc "uncertainty"    (cn' "uncertainty")
user            = npnc "user"           (cn' "user")
useCase         = npnc "useCase"        (cn' "use case")
value           = npnc "value"          (cn' "value") --general enough to be in Documentation?
variable        = npnc "variable"       (cn' "variable")
video           = npnc "video"          (cn' "video")
verification    = npnc "verification"   (cn' "verification")
realtime        = npnc "real-time"      (cn' "real-time")



orgOfDoc, prpsOfDoc, refmat, scpOfReq,
  termAndDef, tOfSymb, traceyMandG, corSol, charOfIR, propOfCorSol :: NPNC

corSol       = npnc "corSol"       (cn' "correct solution")
charOfIR     = npnc "charOfIR"     (characteristic `of_''` intReader)
orgOfDoc     = npnc "orgOfDoc"     (organization `of_` document)
propOfCorSol = npnc "propOfCorSol" (property `of__` (a_ corSol))
prpsOfDoc    = npnc "prpsOfDoc"    (purpose `of_` document)
refmat       = npnc "refmat"       (cn' "reference material")
scpOfReq     = npnc "scpOfReq"     (scope `of_'` requirement)
termAndDef   = npnc "termAndDef"   (terminology `and_'` definition)
tOfSymb      = npnc "tOfSymb"      (table_ `of_'` symbol_)
traceyMandG  = npnc "traceyMandG"  (andRT titleize' titleize' traceyMatrix graph)

scpOfTheProj :: (NP -> Sentence) -> NPNC
scpOfTheProj oper = npnc "scpOfTheProj" (scope `of_` theCustom oper project) -- reasonable hack?

-- compounds

characteristicSpecification, generalSystemDescription, indPRCase, 
  physicalConstraint, physicalSystem, problemDescription, prodUCTable, reqSpec, 
  specificsystemdescription, systemdescription, systemConstraint, sysCont, 
  userCharacteristic, datumConstraint, functionalRequirement, 
  nonfunctionalRequirement, softwareDoc, softwareSys, softwareVerif,
  solutionCharSpec, offShelfSolution, physicalSim, productUC, 
  useCaseTable, physicalProperty :: NPNC
  
characteristicSpecification  = compoundNPNC'' plural phrase characteristic specification
generalSystemDescription     = compoundNPNC general systemdescription
indPRCase                    = compoundNPNC individual productUC
physicalConstraint           = compoundNPNC physical constraint_
physicalSystem               = compoundNPNC physical system
physicalProperty             = compoundNPNC physical property
problemDescription           = compoundNPNC problem description
prodUCTable                  = compoundNPNC productUC table_
reqSpec                      = compoundNPNC requirement_ specification
specificsystemdescription    = compoundNPNC specific systemdescription
systemdescription            = compoundNPNC system description
systemConstraint             = compoundNPNC system constraint_
sysCont                      = compoundNPNC system context
userCharacteristic           = compoundNPNC user characteristic
datumConstraint              = compoundNPNC' datum constraint_
functionalRequirement        = compoundNPNC functional requirement_
nonfunctionalRequirement     = compoundNPNC nonfunctional requirement_
solutionCharSpec             = compoundNPNC solution characteristicSpecification
offShelfSolution             = compoundNPNC offShelf solution
physicalSim                  = compoundNPNC physical simulation
productUC                    = compoundNPNC product_ useCase
useCaseTable                 = compoundNPNC useCase table_
softwareDoc                  = compoundNPNC software documentation
softwareSys                  = compoundNPNC software system
softwareVerif                = compoundNPNC software verification
