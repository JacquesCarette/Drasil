module Drasil.Metadata.Documentation
  ( abbAcc, abbreviation, acronym, appendix, assumption, assumpDom, caseProb
  , charOfIR, characteristic, chgProbDom
  , client, code, column, component, consVals
  , constraint, content, context, corSol, customer, datum, datumConstraint
  , decision , definition, dependency, description, desSpec, design, designDoc
  , document, documentation, endUser
  , element, environment, example, form, full, fullForm
  , functional, functionalRequirement, funcReqDom, general, generalSystemDescription
  , goal, goalStmt, goalStmtDom, guide
  , implementation, inDatumConstraint, individual, indPRCase
  , information, input_, interface, interest, introduction, intReader, item
  , learnObj, likelyChg, likeChgDom, limitation, mg, mis, model, module_, moduleInterface
  , nonfunctional, nonfunctionalRequirement, nonFuncReqDom
  , notebook, offShelf, offShelfSolution, organization, orgOfDoc, output_
  , outDatumConstraint
  , physical, physSyst, physicalConstraint, physicalSystem, plan, product_
  , productUC, prodUCTable
  , problem, problemDescription, problemIntro, project, property, propOfCorSol
  , prpsOfDoc, purpose, quantity, reference, refBy, refMat
  , requirement, reqDom, reqInput, review, scope, scpOfTheProj, scpOfReq, section_, sec
  , software, softwareConstraint, softwareDoc, softwareReq, softwareVAV
  , solution, solutionCharSpec
  , solutionCharacteristic
  , specific, specification, specificsystemdescription, srs, srsDom, stakeholder
  , statement, summary, symbol_, system, systemConstraint, sysCont, systemdescription
  , table_, tAuxConsts, template, term_
  , terminology, termAndDef, theory, tOfCont, tOfSymb, tOfUnit, traceyGraph, traceyMandG
  , traceyMatrix, typUnc, uncertainty
  , unlikelyChg, unlikeChgDom, useCase, user, userCharacteristic
  , value, variable, vav, vavPlan, validation, verification
  )
  where

-- General Haskell
import Control.Lens ((^.))

-- General Drasil
import Language.Drasil (CI, NP, IdeaDict, nc, cn, cn', cnIES, cnICES, cnUM,
  commonIdeaWithDict, fterms, compoundPhrase, compoundPhraseP1, titleizeNP',
  term, ConceptChunk,
  ccs, mkIdea, Sentence(EmptyS), dcc)
import Language.Drasil.Chunk.Concept.NamedCombinators
  (combineNINI, compoundNC, compoundNCPP, of_, of_PS, ofAPS, of_NINP, theGen
  , compoundNCPSPP, and_, and_TGen, and_PP)
import Language.Drasil.Development (NPStruct)
--
-- Vocabulary
import Drasil.Metadata.Concepts.Math (graph, unit_)
import Drasil.Metadata.Domains (documentc, softEng)

softReqSpec :: NP
softReqSpec = fterms compoundPhraseP1 softwareReq specification

abbAcc, abbreviation, acronym, appendix, caseProb,
  characteristic, charOfIR, client, code, column, component,
  consVals, constraint, content, context, corSol, customer, datum, decision,
  definition, dependency, description, design, document, documentation,
  endUser, element, environment, example, form, full,
  functional, general, goal, guide, implementation,
  inDatumConstraint, individual, information, input_, interface, interest,
  introduction, intReader, item, limitation, model, module_,
  nonfunctional, outDatumConstraint,
  offShelf, organization, orgOfDoc, output_,
  physical, plan, project, product_, problem, property, propOfCorSol, prpsOfDoc, purpose,
  quantity, reference, refMat, reqInput, review,
  scope, scpOfReq, section_, software, solution, specific,
  specification, stakeholder, statement, summary, symbol_, system, table_, tAuxConsts,
  template, term_, terminology, termAndDef, theory,
  tOfCont, tOfSymb, tOfUnit, traceyGraph, traceyMatrix,
  uncertainty, useCase, user, validation, value, variable, vav, verification :: IdeaDict

abbAcc          = nc "TAbbAcc"        (abbreviation `and_PP` acronym)
abbreviation    = nc "abbreviation"   (cn'    "abbreviation"       )
acronym         = nc "acronym"        (cn'    "acronym"            )
appendix        = nc "appendix"       (cnICES "appendix"           )
caseProb        = nc "caseProb"       (cn' "case problem")
characteristic  = nc "characteristic" (cn'    "characteristic"     )
charOfIR        = nc "charOfIR"       (characteristic `of_PS` intReader)
client          = nc "client"         (cn'    "client"             )
code            = nc "code"           (cn     "code"               )
column          = nc "column"         (cn'    "column"             ) --general enough to be in Documentation?
component       = nc "component"      (cn'    "component"          )
constraint      = nc "constraint"     (cn'    "constraint"         )
consVals        = nc "consVals"       (cn "values of auxiliary constants")
content         = nc "content"        (cn'    "content"            )
context         = nc "context"        (cn'    "context"            )
corSol          = nc "corSol"         (cn' "correct solution")
customer        = nc "customer"       (cn'    "customer"           )
datum           = nc "datum"          (cnUM   "datum"              )
decision        = nc "decision"       (cn'    "decision"           )
definition      = nc "definition"     (cn'    "definition"         )
dependency      = nc "dependency"     (cnIES  "dependency"         )
description     = nc "description"    (cn'    "description"        )
design          = nc "design"         (cn'    "design"             )
document        = nc "document"       (cn'    "document"           )
documentation   = nc "documentation"  (cn'    "documentation"      )
endUser         = nc "end user"       (cn'    "end user"           )
element         = nc "element"        (cn'    "element"            )
environment     = nc "environment"    (cn'    "environment"        ) -- Is this term in the right spot?
example         = nc "example"        (cn'    "example"            )
form            = nc "form"           (cn'    "form"               )
full            = nc "full"           (cn'    "full"               ) --FIXME: Adjective
functional      = nc "functional"     (cn'    "functional"         ) --FIXME: Adjective
general         = nc "general"        (cn'    "general"            ) --FIXME: Adjective
goal            = nc "goal"           (cn'    "goal"               )
guide           = nc "guide"          (cn'    "guide"              )
implementation  = nc "implementation" (cn'    "implementation"     )
inDatumConstraint   = nc "InDataConstraints"  (cn' "input data constraint")
individual      = nc "individual"     (cn'    "individual"         )
information     = nc "information"    (cn     "information"        )
input_          = nc "input"          (cn'    "input"              )
interest        = nc "interest"       (cn'    "interest"           )
interface       = nc "interface"      (cn'    "interface"          )
intReader       = nc "intReader"      (cn'    "intended reader"    )
introduction    = nc "introduction"   (cn'    "introduction"       )
item            = nc "item"           (cn'    "item"               )
limitation      = nc "limitation"     (cn'    "limitation"         )
model           = nc "model"          (cn'    "model"              )
module_         = nc "module"         (cn'    "module"             )
nonfunctional   = nc "non-functional" (cn'    "non-functional"     ) --FIXME: Adjective
offShelf        = nc "Off-the-Shelf"  (cn'    "Off-the-Shelf"      )
organization    = nc "organization"   (cn'    "organization"       )
orgOfDoc        = nc "orgOfDoc"       (organization `of_` document)
output_         = nc "output"         (cn'    "output"             )
outDatumConstraint  = nc "OutDataConstraints" (cn' "output data constraint")
physical        = nc "physical"       (cn'    "physical"           ) --FIXME: Adjective
plan            = nc "plan"           (cn'    "plan"               )
product_        = nc "product"        (cn'    "product"            )
problem         = nc "problem"        (cn'    "problem"            )
project         = nc "project"        (cn'    "project"            )
property        = nc "property"       (cnIES  "property"           )
propOfCorSol    = nc "propOfCorSol"   (property `ofAPS` corSol     )
prpsOfDoc       = nc "prpsOfDoc"      (purpose `of_` document      )
purpose         = nc "purpose"        (cn'    "purpose"            )
quantity        = nc "quantity"       (cnIES  "quantity"           ) --general enough to be in documentaion.hs?
reference       = nc "reference"      (cn'    "reference"          )
refMat          = nc "refMat"         (cn' "reference material"    )
reqInput        = nc "ReqInputs"      (cn' "required input"        )
review          = nc "review"         (cn'    "review"             )
scope           = nc "scope"          (cn'    "scope"              )
scpOfReq        = nc "scpOfReq"       (scope `of_` requirement     )
section_        = nc "section"        (cn'    "section"            )
software        = nc "software"       (cn     "software"           )
solution        = nc "solution"       (cn'    "solution"           )
specific        = nc "specific"       (cn'    "specific"           ) --FIXME: Adjective
specification   = nc "specification"  (cn'    "specification"      )
stakeholder     = nc "stakeholder"    (cn'    "stakeholder"        )
statement       = nc "statement"      (cn'    "statement"          )
summary         = nc "summary"        (cnIES  "summary"            )
symbol_         = nc "symbol"         (cn'    "symbol"             )
system          = nc "system"         (cn'    "system"             )
table_          = nc "table"          (cn'    "table"              )
tAuxConsts      = nc "TAuxConsts"     (cn' "auxiliary constant")
template        = nc "template"       (cn'    "template"           )
term_           = nc "term"           (cn'    "term"               )
terminology     = nc "terminology"    (cnIES  "terminology"        )
termAndDef      = nc "termAndDef"     (terminology `and_` definition)
theory          = nc "theory"         (cnIES  "theory"             )
tOfCont         = nc "tOfCont"        (table_ `of_` content)
tOfSymb         = nc "tOfSymb"        (table_ `of_` symbol_)
tOfUnit         = nc "tOfUnit"        (table_ `of_` unit_)
traceyGraph     = nc "traceyGraph"    (cn'    "traceability graph" )
traceyMatrix    = nc "traceyMatrix"   (cnICES "traceability matrix")
uncertainty     = nc "uncertainty"    (cnIES  "uncertainty"        )
useCase         = nc "useCase"        (cn'    "use case"           )
user            = nc "user"           (cn'    "user"               )
validation      = nc "validation"     (cn'    "validation"         )
value           = nc "value"          (cn'    "value"              )
variable        = nc "variable"       (cn'    "variable"           )
vav             = nc "vav"            (verification `and_` validation)
verification    = nc "verification"   (cn'    "verification"       )

datumConstraint, designDoc, fullForm, functionalRequirement,
  generalSystemDescription, indPRCase, moduleInterface,
  nonfunctionalRequirement, offShelfSolution, physicalConstraint,
  physicalSystem, problemDescription, problemIntro,
  productUC, prodUCTable, softwareReq, solutionCharacteristic, solutionCharSpec,
  softwareConstraint, softwareDoc, softwareVAV,
  specificsystemdescription, sysCont, systemConstraint, systemdescription,
  userCharacteristic, vavPlan :: IdeaDict
datumConstraint              = compoundNCPP datum constraint
designDoc                    = compoundNC design document
fullForm                     = compoundNC full form
functionalRequirement        = compoundNC functional requirement
generalSystemDescription     = compoundNC general systemdescription
indPRCase                    = compoundNC individual productUC
moduleInterface              = compoundNC module_ interface
nonfunctionalRequirement     = compoundNC nonfunctional requirement
offShelfSolution             = compoundNC offShelf solution
problemDescription           = compoundNC problem description
problemIntro                 = compoundNC problem introduction
prodUCTable                  = compoundNC productUC table_
productUC                    = compoundNC product_ useCase
physicalConstraint           = compoundNC physical constraint
physicalSystem               = compoundNC physical system
softwareConstraint           = compoundNC software constraint
softwareDoc                  = compoundNC software documentation
softwareReq                  = compoundNCPP software requirement
softwareVAV                  = compoundNC software vav
solutionCharSpec             = compoundNCPSPP solutionCharacteristic specification
solutionCharacteristic       = compoundNC solution characteristic
specificsystemdescription    = compoundNC specific systemdescription
sysCont                      = compoundNC system context
systemConstraint             = compoundNC system constraint
systemdescription            = compoundNC system description
userCharacteristic           = compoundNC user characteristic
vavPlan                      = compoundNC vav plan

traceyMandG :: IdeaDict
traceyMandG         = nc "traceyMandG"        (and_TGen (\t -> titleizeNP' (t ^. term)) (\t -> titleizeNP' (t ^. term)) traceyMatrix graph)

-- * Domains

-- | Root SRS Domain.
srsDom :: ConceptChunk
srsDom = dcc "srsDom" (srs ^. term) "srs"

assumpDom, chgProbDom, funcReqDom, goalStmtDom, likeChgDom,
  nonFuncReqDom, reqDom, unlikeChgDom :: ConceptChunk
assumpDom     = ccs (mkIdea "assumpDom"     (assumption ^. term)               $ Just "A")        EmptyS [srsDom]
chgProbDom    = ccs (nc "chgProbDom" $ cn' "change")                                              EmptyS [srsDom]
funcReqDom    = ccs (mkIdea "funcReqDom"    (functionalRequirement ^. term)    $ Just "FR")       EmptyS [reqDom]
goalStmtDom   = ccs (mkIdea "goalStmtDom"   (goalStmt ^. term)                 $ Just "GS")       EmptyS [srsDom]
likeChgDom    = ccs (mkIdea "likeChgDom"    (likelyChg ^. term)                $ Just "LC")       EmptyS [chgProbDom]
nonFuncReqDom = ccs (mkIdea "nonFuncReqDom" (nonfunctionalRequirement ^. term) $ Just "NFR")      EmptyS [reqDom]
reqDom        = ccs (mkIdea "reqDom"        (requirement ^. term)              $ Just "R")        EmptyS [srsDom]
unlikeChgDom  = ccs (mkIdea "unlikeChgDom"  (unlikelyChg ^. term)              $ Just "UC")       EmptyS [chgProbDom]

assumption, desSpec, goalStmt, learnObj, likelyChg, mg, mis, notebook, physSyst,
  refBy, requirement, sec, srs, typUnc, unlikelyChg :: CI
assumption  = commonIdeaWithDict "assumption"  (cn' "assumption")                                    "A"       [softEng]
desSpec     = commonIdeaWithDict "desSpec"     (combineNINI design specification)                    "DS"      [softEng]
goalStmt    = commonIdeaWithDict "goalStmt"    (combineNINI goal statement)                          "GS"      [softEng]
learnObj    = commonIdeaWithDict "learnObj"    (cn' "learning objective")                            "LO"      [documentc]
likelyChg   = commonIdeaWithDict "likelyChg"   (cn' "likely change")                                 "LC"      [softEng]
physSyst    = commonIdeaWithDict "physSyst"    (combineNINI physicalSystem description)              "PS"      [softEng]
mg          = commonIdeaWithDict "mg"          (fterms compoundPhrase module_ guide)                 "MG"      [softEng]
mis         = commonIdeaWithDict "mis"         (fterms compoundPhrase moduleInterface specification) "MIS"     [softEng]
notebook    = commonIdeaWithDict "notebook"    (cn' "notebook")                                      "NB"      [softEng]
refBy       = commonIdeaWithDict "refBy"       (cn  "referenced by")                                 "RefBy"   [documentc]
requirement = commonIdeaWithDict "requirement" (cn' "requirement")                                   "R"       [softEng]
sec         = commonIdeaWithDict "section"     (cn' "section")                                       "Sec"     [documentc]
srs         = commonIdeaWithDict "srs"         softReqSpec                                           "SRS"     [softEng]
typUnc      = commonIdeaWithDict "typUnc"      (cn' "typical uncertainty")                           "Uncert." [softEng]
unlikelyChg = commonIdeaWithDict "unlikelyChg" (cn' "unlikely change")                               "UC"      [softEng]

scpOfTheProj :: (IdeaDict -> NPStruct) -> IdeaDict
scpOfTheProj oper = nc "scpOfTheProj" (scope `of_NINP` theGen oper project) -- reasonable hack?
