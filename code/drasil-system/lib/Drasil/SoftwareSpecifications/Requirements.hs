{-# LANGUAGE TemplateHaskell #-}
module Drasil.SoftwareSpecifications.Requirements (
    RequirementsSpecification,
    SmithEtAlReqSpec,
    smithEtAlReq,
    theoryModels, genDefns, dataDefns, instModels,
    quants, inputs, outputs, constraints, constants
) where

import Control.Lens

import Language.Drasil (DefinedQuantityDict, UncertQ, ConstQDef)
import Theory.Drasil (TheoryModel, GenDefn, DataDefinition, InstanceModel)

data RequirementsSpecification = SmithEtAlStyle SmithEtAlReqSpec

data SmithEtAlReqSpec = SeaRS {
  _theoryModels :: [TheoryModel],
  _genDefns     :: [GenDefn],
  _dataDefns    :: [DataDefinition],
  _instModels   :: [InstanceModel],
  _quants       :: [DefinedQuantityDict],
  _inputs       :: [DefinedQuantityDict],
  _outputs      :: [DefinedQuantityDict],
  _constraints  :: [UncertQ],
  _constants    :: [ConstQDef]
}
makeLenses ''SmithEtAlReqSpec

smithEtAlReq :: [TheoryModel] -> [GenDefn] -> [DataDefinition] -> [InstanceModel] ->
    [DefinedQuantityDict] -> [DefinedQuantityDict] -> [DefinedQuantityDict] ->
    [UncertQ] -> [ConstQDef] -> RequirementsSpecification
smithEtAlReq tms gds dds ims qs ins outs cnstrnts cnstnts =
  SmithEtAlStyle (SeaRS tms gds dds ims qs ins outs cnstrnts cnstnts)
