----------------------------------------------------------
### Quick Summary of Folder Structure and File Contents
Last updated: July 19, 2018
----------------------------------------------------------

**Concept**
  - DefnAndDomain ConceptChunk, CommonConcept, and ConceptInstance data types

**Constrained**
  - Defines Constraint type

AssumpChunk.hs
  - Assumption Chunks *TODO: Move from drasil-lang to drasil-docLang*

Attribute.hs
  - Helper functions for Attributes (Derivation, ShortName, SourceRef)

Change.hs
  - Likely and Unlikely Change chunks *TODO: Move from drasil-lang to drasil-docLang*

Citation.hs
  - Citation chunks and related data types
  - Helper functions for creating citations

CommonIdea.hs
  - Defines CI data type

Concept.hs
  - Smart constructors for Concept directory

Constrained.hs
  - Defines ConstrainedChunk and ConstrConcept data types

DefinedQuantity.hs
  - Defines DefinedQuantityDict data type and constructors

Derivation.hs
  - Defines Derivation type

Eq.hs
  - Defines QDefinition data type (quantity with units and defining equations)

GenDefn.hs
  - General Definition chunks *TODO: Move from drasil-lang to drasil-docLang*

Goal.hs
  - Goal statement chunks *TODO: Move from drasil-lang to drasil-docLang*

InstanceModel.hs
  - Instance model chunks *TODO: Move from drasil-lang to drasil-docLang*

NamedIdea.hs
  - NamedChunk and IdeaDict data types
  - Various NamedChunk combinators

PhysSystDesc.hs
  - Physical system description chunks *TODO: Move from drasil-lang to drasil-docLang*

Quantity.hs
  - `Quantity` class definition
  - Defines QuantityDict data type

README.md
  - This file

References.hs
  - Defines Reference type

Relation.hs
  - Defines Relation chunks (Concepts with relations between other chunks).

ReqChunk.hs
  - Defines Requirement chunks *TODO: Move from drasil-lang to drasil-docLang*

SymbolForm.hs
  - Helper functions for getting staged symbols

Theory.hs
  - `Theory` class definition
  - TheoryChunk and TheoryModel data types *TODO: Move from drasil-lang to drasil-docLang*

UncertainQuantity.hs
  - `UncertainQuantity` class definition
  - UncertQ and UncertainChunk data types

Unital.hs
  - Defines Unital chunks (Concepts + Quantities with Units).

Unitary.hs
  - `Unitary` type class
  - UnitaryChunk data type

UnitaryConcept.hs
  - Defines UnitaryConceptDict data type *FIXME: Compare with Unital chunks and see if
    they are actually different*

VarChunk.hs
  - Defines VarChunk data type *FIXME: How is VarChunk different from QuantityDict?*