----------------------------------------------------------
### Quick Summary of Folder Structure and File Contents
April 29, 2018
----------------------------------------------------------

**Attribute**
  - Folder containing attribute data types.

AssumpChunk.hs
  - Assumption Chunks *TODO: Remove from Language and put in
  Examples with DocumentLanguage*

Attribute.hs
  - Attribute type and `HasAttributes` class definition.

Change.hs
  - Likely and Unlikely Change chunks *TODO: Remove from Language and put in
  Examples with DocumentLanguage*

Citation.hs
  - Citation chunks and related data types.
  - Helper functions for creating citations.

Code.hs
  - CodeChunk and CodeDefinition data types.
  - `CodeIdea` class definition.

CommonIdea.hs
  - CI data type.
  - `CommonIdea` class definition.

Concept.hs
  - DefnAndDomain and ConceptChunk and data types.
  - `Definition`, `ConceptDomain`, and `Concept` class definitions.

Constrained.hs
  - `Constrained` and `HasReasVal` class definitions.
  - TheoryConstraint and helper data types.
  - Constraint and helper data types.
  - ConstrainedChunk and ConstrainedConcept data types.

DefinedQuantity
  - DefinedQuantityDict data type.

Eq.hs
  - Defines QDefinitions (quantities with units
      and defining equations).

ExprRelat.hs
  - `ExprRelat` class definition. *TODO: Remove this class when possible*

GenDefn.hs
  - General Definition chunks *TODO: Remove from Language and put in
  Examples with DocumentLanguage*

Goal.hs
  - Goal statement chunks *TODO: Remove from Language and put in
  Examples with DocumentLanguage*

InstanceModel.hs
  - Instance model chunks *TODO: Remove from Language and put in
  Examples with DocumentLanguage*

NamedIdea.hs
  - `NamedIdea` and `Idea` class definitions.
  - NamedChunk and IdeaDict data types.
  - Various NamedChunk combinators.

PhysSystDesc.hs
  - Physical system description chunks *TODO: Remove from Language and put in
  Examples with DocumentLanguage*

Quantity.hs
  - `Quantity` class definition.
  - QuantityDict data type.

Relation.hs
  - Defines Relation chunks (Concepts with relations
      between other chunks).

ReqChunk.hs
  - Requirement chunks *TODO: Remove from Language and put in
  Examples with DocumentLanguage*

SymbolForm.hs
  - `HasSymbol` class definition.
  - Stage data type.
  - Helper functions for getting staged symbols.

Theory.hs
  - `Theory` class definition.
  - TheoryChunk data type.
  - TheoryModel data type *TODO: Remove from Language and put in
  Examples with DocumentLanguage*

UncertainQuantity.hs
  - `UncertainQuantity` class definition.
  - UncertQ and UncertainChunk data types.

Unital.hs
  - Defines Unital chunks (Concepts + Quantities with Units).

Unitary.hs
  - `Unitary` type class.
  - UnitaryChunk data type.

UnitaryConcept.hs
  - UnitaryConceptDict data type *FIXME: Compare with Unital chunks and see if
    they are actually different.*

VarChunk.hs
  - VarChunk data type. *FIXME: How is VarChunk different from QuantityDict?*