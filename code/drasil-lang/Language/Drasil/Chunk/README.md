----------------------------------------------------------
### Quick Summary of Folder Structure and File Contents
Last updated: May 14, 2019
----------------------------------------------------------

**Concept**
  - ConceptChunk, CommonConcept, and ConceptInstance data types

Citation.hs
  - Citation chunks and related data types
  - Helper functions for creating citations

CommonIdea.hs
  - Defines CI data type

Concept.hs
  - Smart constructors for Concept directory

Constrained.hs
  - Defines ConstrainedChunk and ConstrConcept data types

DataDefinition.hs
  - Defines DataDefinition data type and constructors *TODO: Move from drasil-lang to drasil-theory*

DefinedQuantity.hs
  - Defines DefinedQuantityDict data type and constructors

Eq.hs
  - Defines QDefinition data type (quantity with units and defining equations)

NamedIdea.hs
  - NamedChunk and IdeaDict data types
  - Various NamedChunk combinators

Quantity.hs
  - `Quantity` class definition
  - Defines QuantityDict data type

README.md
  - This file

Relation.hs
  - Defines Relation chunks (Concepts with relations between other chunks).

Theory.hs
  - `Theory` class definition
  - TheoryChunk and TheoryModel data types *TODO: Move from drasil-lang to drasil-theory*

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

UnitDefn.hs
  - Defines UnitDefn data type and constructors and helper functions
