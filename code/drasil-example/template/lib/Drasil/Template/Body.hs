module Drasil.Template.Body where

import Language.Drasil
import Drasil.SRSDocument
import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)
import qualified Utils.Drasil.Sentence as S

import qualified Data.Drasil.Concepts.Documentation as Doc (srs)

srs :: Document
srs = mkDoc mkSRS (S.forGen titleize phrase) si

fullSI :: SystemInformation
fullSI = fillcdbSRS mkSRS si

printSetting :: PrintingInformation
printSetting = piSys fullSI Equational defaultConfiguration

mkSRS :: SRSDecl
mkSRS = []

si :: SystemInformation
si = SI {
  _sys         = example,
  _kind        = Doc.srs,
  _authors     = [authorName],
  _purpose     = [],
  _quants      = [] :: [QuantityDict],
  _concepts    = [] :: [DefinedQuantityDict],
  _instModels  = [] :: [InstanceModel],
  _datadefs   = [] :: [DataDefinition Expr],
  _configFiles = [],
  _inputs      = [] :: [QuantityDict],
  _outputs     = [] :: [QuantityDict],
  _defSequence = [] :: [Block (QDefinition Expr)],
  _constraints = [] :: [ConstrainedChunk],
  _constants   = [] :: [QDefinition Expr],
  _sysinfodb   = symbMap,
  _usedinfodb  = usedDB,
   refdb       = refDB
}

symbMap :: ChunkDB
symbMap = cdb ([] :: [QuantityDict]) [nw example] ([] :: [ConceptChunk])
  ([] :: [UnitDefn]) ([] :: [DataDefinition Expr]) ([] :: [DataDefinition ModelExpr]) 
  ([] :: [InstanceModel]) ([] :: [GenDefn]) ([] :: [TheoryModel])
  ([] :: [ConceptInstance]) ([] :: [Section]) ([] :: [LabelledContent])
  ([] :: [Reference])

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) ([] :: [IdeaDict]) ([] :: [ConceptChunk])
  ([] :: [UnitDefn]) ([] :: [DataDefinition Expr]) ([] :: [DataDefinition ModelExpr]) 
  ([] :: [InstanceModel]) ([] :: [GenDefn]) ([] :: [TheoryModel])
  ([] :: [ConceptInstance]) ([] :: [Section]) ([] :: [LabelledContent])
  ([] :: [Reference])

refDB :: ReferenceDB
refDB = rdb [] []

-- MOVE TO CONCEPTS
example :: CI -- name of example
example = commonIdeaWithDict "example" (pn "Template") "Template" []

-- MOVE TO DATA.PEOPLE
authorName :: Person
authorName = person "Author" "Name"
