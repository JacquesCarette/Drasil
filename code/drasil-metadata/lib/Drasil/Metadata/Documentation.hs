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
  , prpsOfDoc, purpose, quantity, reference, refBy, refName, refMat
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
import Drasil.Database (mkUid)
import Language.Drasil (CI, NP, IdeaDict, cn, cn', cnIES, cnICES, cnUM,
  commonIdeaWithDict, fterms, compoundPhrase, compoundPhraseP1, titleizeNP',
  term, ConceptChunk, cncpt, cncpt', Sentence(EmptyS), idea', cncpt''', Sentence(..))
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

abbAcc          = idea' (mkUid "TAbbAcc")        (abbreviation `and_PP` acronym)
abbreviation    = idea' (mkUid "abbreviation")   (cn'    "abbreviation"       )
acronym         = idea' (mkUid "acronym")        (cn'    "acronym"            )
appendix        = idea' (mkUid "appendix")       (cnICES "appendix"           )
caseProb        = idea' (mkUid "caseProb")       (cn' "case problem")
characteristic  = idea' (mkUid "characteristic") (cn'    "characteristic"     )
charOfIR        = idea' (mkUid "charOfIR")       (characteristic `of_PS` intReader)
client          = idea' (mkUid "client")         (cn'    "client"             )
code            = idea' (mkUid "code")           (cn     "code"               )
column          = idea' (mkUid "column")         (cn'    "column"             ) --general enough to be in Documentation?
component       = idea' (mkUid "component")      (cn'    "component"          )
constraint      = idea' (mkUid "constraint")     (cn'    "constraint"         )
consVals        = idea' (mkUid "consVals")       (cn "values of auxiliary constants")
content         = idea' (mkUid "content")        (cn'    "content"            )
context         = idea' (mkUid "context")        (cn'    "context"            )
corSol          = idea' (mkUid "corSol")         (cn' "correct solution")
customer        = idea' (mkUid "customer")       (cn'    "customer"           )
datum           = idea' (mkUid "datum")          (cnUM   "datum"              )
decision        = idea' (mkUid "decision")       (cn'    "decision"           )
definition      = idea' (mkUid "definition")     (cn'    "definition"         )
dependency      = idea' (mkUid "dependency")     (cnIES  "dependency"         )
description     = idea' (mkUid "description")    (cn'    "description"        )
design          = idea' (mkUid "design")         (cn'    "design"             )
document        = idea' (mkUid "document")       (cn'    "document"           )
documentation   = idea' (mkUid "documentation")  (cn'    "documentation"      )
endUser         = idea' (mkUid "end user")       (cn'    "end user"           )
element         = idea' (mkUid "element")        (cn'    "element"            )
environment     = idea' (mkUid "environment")    (cn'    "environment"        ) -- Is this term in the right spot?
example         = idea' (mkUid "example")        (cn'    "example"            )
form            = idea' (mkUid "form")           (cn'    "form"               )
full            = idea' (mkUid "full")           (cn'    "full"               ) --FIXME: Adjective
functional      = idea' (mkUid "functional")     (cn'    "functional"         ) --FIXME: Adjective
general         = idea' (mkUid "general")        (cn'    "general"            ) --FIXME: Adjective
goal            = idea' (mkUid "goal")           (cn'    "goal"               )
guide           = idea' (mkUid "guide")          (cn'    "guide"              )
implementation  = idea' (mkUid "implementation") (cn'    "implementation"     )
inDatumConstraint   = idea' (mkUid "InDataConstraints")  (cn' "input data constraint")
individual      = idea' (mkUid "individual")     (cn'    "individual"         )
information     = idea' (mkUid "information")    (cn     "information"        )
input_          = idea' (mkUid "input")          (cn'    "input"              )
interest        = idea' (mkUid "interest")       (cn'    "interest"           )
interface       = idea' (mkUid "interface")      (cn'    "interface"          )
intReader       = idea' (mkUid "intReader")      (cn'    "intended reader"    )
introduction    = idea' (mkUid "introduction")   (cn'    "introduction"       )
item            = idea' (mkUid "item")           (cn'    "item"               )
limitation      = idea' (mkUid "limitation")     (cn'    "limitation"         )
model           = idea' (mkUid "model")          (cn'    "model"              )
module_         = idea' (mkUid "module")         (cn'    "module"             )
nonfunctional   = idea' (mkUid "non-functional") (cn'    "non-functional"     ) --FIXME: Adjective
offShelf        = idea' (mkUid "Off-the-Shelf")  (cn'    "Off-the-Shelf"      )
organization    = idea' (mkUid "organization")   (cn'    "organization"       )
orgOfDoc        = idea' (mkUid "orgOfDoc")       (organization `of_` document)
output_         = idea' (mkUid "output")         (cn'    "output"             )
outDatumConstraint  = idea' (mkUid "OutDataConstraints") (cn' "output data constraint")
physical        = idea' (mkUid "physical")       (cn'    "physical"           ) --FIXME: Adjective
plan            = idea' (mkUid "plan")           (cn'    "plan"               )
product_        = idea' (mkUid "product")        (cn'    "product"            )
problem         = idea' (mkUid "problem")        (cn'    "problem"            )
project         = idea' (mkUid "project")        (cn'    "project"            )
property        = idea' (mkUid "property")       (cnIES  "property"           )
propOfCorSol    = idea' (mkUid "propOfCorSol")   (property `ofAPS` corSol     )
prpsOfDoc       = idea' (mkUid "prpsOfDoc")      (purpose `of_` document      )
purpose         = idea' (mkUid "purpose")        (cn'    "purpose"            )
quantity        = idea' (mkUid "quantity")       (cnIES  "quantity"           ) --general enough to be in documentaion.hs?
reference       = idea' (mkUid "reference")      (cn'    "reference"          )
refMat          = idea' (mkUid "refMat")         (cn' "reference material"    )
reqInput        = idea' (mkUid "ReqInputs")      (cn' "required input"        )
review          = idea' (mkUid "review")         (cn'    "review"             )
scope           = idea' (mkUid "scope")          (cn'    "scope"              )
scpOfReq        = idea' (mkUid "scpOfReq")       (scope `of_` requirement     )
section_        = idea' (mkUid "section")        (cn'    "section"            )
software        = idea' (mkUid "software")       (cn     "software"           )
solution        = idea' (mkUid "solution")       (cn'    "solution"           )
specific        = idea' (mkUid "specific")       (cn'    "specific"           ) --FIXME: Adjective
specification   = idea' (mkUid "specification")  (cn'    "specification"      )
stakeholder     = idea' (mkUid "stakeholder")    (cn'    "stakeholder"        )
statement       = idea' (mkUid "statement")      (cn'    "statement"          )
summary         = idea' (mkUid "summary")        (cnIES  "summary"            )
symbol_         = idea' (mkUid "symbol")         (cn'    "symbol"             )
system          = idea' (mkUid "system")         (cn'    "system"             )
table_          = idea' (mkUid "table")          (cn'    "table"              )
tAuxConsts      = idea' (mkUid "TAuxConsts")     (cn' "auxiliary constant")
template        = idea' (mkUid "template")       (cn'    "template"           )
term_           = idea' (mkUid "term")           (cn'    "term"               )
terminology     = idea' (mkUid "terminology")    (cnIES  "terminology"        )
termAndDef      = idea' (mkUid "termAndDef")     (terminology `and_` definition)
theory          = idea' (mkUid "theory")         (cnIES  "theory"             )
tOfCont         = idea' (mkUid "tOfCont")        (table_ `of_` content)
tOfSymb         = idea' (mkUid "tOfSymb")        (table_ `of_` symbol_)
tOfUnit         = idea' (mkUid "tOfUnit")        (table_ `of_` unit_)
traceyGraph     = idea' (mkUid "traceyGraph")    (cn'    "traceability graph" )
traceyMatrix    = idea' (mkUid "traceyMatrix")   (cnICES "traceability matrix")
uncertainty     = idea' (mkUid "uncertainty")    (cnIES  "uncertainty"        )
useCase         = idea' (mkUid "useCase")        (cn'    "use case"           )
user            = idea' (mkUid "user")           (cn'    "user"               )
validation      = idea' (mkUid "validation")     (cn'    "validation"         )
value           = idea' (mkUid "value")          (cn'    "value"              )
variable        = idea' (mkUid "variable")       (cn'    "variable"           )
vav             = idea' (mkUid "vav")            (verification `and_` validation)
verification    = idea' (mkUid "verification")   (cn'    "verification"       )

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
traceyMandG         = idea' (mkUid "traceyMandG")        (and_TGen (\t -> titleizeNP' (t ^. term)) (\t -> titleizeNP' (t ^. term)) traceyMatrix graph)

-- * Domains

-- | Root SRS Domain.
srsDom :: ConceptChunk
srsDom = cncpt''' (mkUid "srsDom") (srs ^. term) (S "srs")

assumpDom, chgProbDom, funcReqDom, goalStmtDom, likeChgDom,
  nonFuncReqDom, reqDom, unlikeChgDom :: ConceptChunk
assumpDom     = cncpt  (mkUid "assumpDom")     (assumption ^. term)               EmptyS "A"   [srsDom]
chgProbDom    = cncpt' (mkUid "chgProbDom")    (cn' "change")                     EmptyS       [srsDom]
funcReqDom    = cncpt  (mkUid "funcReqDom")    (functionalRequirement ^. term)    EmptyS "FR"  [reqDom]
goalStmtDom   = cncpt  (mkUid "goalStmtDom")   (goalStmt ^. term)                 EmptyS "GS"  [srsDom]
likeChgDom    = cncpt  (mkUid "likeChgDom")    (likelyChg ^. term)                EmptyS "LC"  [chgProbDom]
nonFuncReqDom = cncpt  (mkUid "nonFuncReqDom") (nonfunctionalRequirement ^. term) EmptyS "NFR" [reqDom]
reqDom        = cncpt  (mkUid "reqDom")        (requirement ^. term)              EmptyS "R"   [srsDom]
unlikeChgDom  = cncpt  (mkUid "unlikeChgDom")  (unlikelyChg ^. term)              EmptyS "UC"  [chgProbDom]

-- FIXME: Some of the below are duplicated knowledge of the above (above preferred). None should be CIs, too.

assumption, desSpec, goalStmt, learnObj, likelyChg, mg, mis, notebook, physSyst,
  refBy, refName, requirement, sec, srs, typUnc, unlikelyChg :: CI
assumption  = commonIdeaWithDict (mkUid "assumption")  (cn' "assumption")                                    "A"       [softEng]
desSpec     = commonIdeaWithDict (mkUid "desSpec")     (combineNINI design specification)                    "DS"      [softEng]
goalStmt    = commonIdeaWithDict (mkUid "goalStmt")    (combineNINI goal statement)                          "GS"      [softEng]
learnObj    = commonIdeaWithDict (mkUid "learnObj")    (cn' "learning objective")                            "LO"      [documentc]
likelyChg   = commonIdeaWithDict (mkUid "likelyChg")   (cn' "likely change")                                 "LC"      [softEng]
physSyst    = commonIdeaWithDict (mkUid "physSyst")    (combineNINI physicalSystem description)              "PS"      [softEng]
mg          = commonIdeaWithDict (mkUid "mg")          (fterms compoundPhrase module_ guide)                 "MG"      [softEng]
mis         = commonIdeaWithDict (mkUid "mis")         (fterms compoundPhrase moduleInterface specification) "MIS"     [softEng]
notebook    = commonIdeaWithDict (mkUid "notebook")    (cn' "notebook")                                      "NB"      [softEng]
refBy       = commonIdeaWithDict (mkUid "refBy")       (cn  "referenced by")                                 "RefBy"   [documentc]
refName     = commonIdeaWithDict (mkUid "refName")     (cn' "reference name")                                "Refname" [documentc]
requirement = commonIdeaWithDict (mkUid "requirement") (cn' "requirement")                                   "R"       [softEng]
sec         = commonIdeaWithDict (mkUid "section")     (cn' "section")                                       "Sec"     [documentc]
srs         = commonIdeaWithDict (mkUid "srs")         softReqSpec                                           "SRS"     [softEng]
typUnc      = commonIdeaWithDict (mkUid "typUnc")      (cn' "typical uncertainty")                           "Uncert." [softEng]
unlikelyChg = commonIdeaWithDict (mkUid "unlikelyChg") (cn' "unlikely change")                               "UC"      [softEng]

scpOfTheProj :: (IdeaDict -> NPStruct) -> IdeaDict
scpOfTheProj oper = idea' (mkUid "scpOfTheProj") (scope `of_NINP` theGen oper project) -- reasonable hack?
