----------------------------------------------------------
### Quick Summary of Folder Structure and File Contents
Last updated: August 13, 2021
----------------------------------------------------------

**Concept**
  - `ConceptChunk` and `ConceptInstance` data types

Citation.hs
  - `Citation` chunks and related data types
  - Helper functions for creating citations

CommonIdea.hs
  - Defines `CI` data type

Constrained.hs
  - Defines `ConstrainedChunk` and `ConstrConcept` data types

DefinedQuantity.hs
  - Defines `DefinedQuantityDict` data type and constructors

Eq.hs
  - Defines `QDefinition` data type (quantity with units and defining equations)

NamedIdea.hs
  - `IdeaDict` data types
  - Various `IdeaDict` combinators

Quantity.hs
  - `Quantity` class definition
  - Defines QuantityDict data type

README.md
  - This file

Relation.hs
  - Defines Relation chunks (Concepts with relations between other chunks).

UncertainQuantity.hs
  - `UncertQ` and `UncertainChunk` data types

Unital.hs
  - Defines Unital chunks (Concepts + Quantities with Units).

Unitary.hs
  - `Unitary` type class
  - `UnitaryChunk` data type

UnitaryConcept.hs
  - Defines `UnitaryConceptDict` data type *FIXME: Compare with Unital chunks and see if
    they are actually different*

UnitDefn.hs
  - Defines `UnitDefn` data types, constructors and helper functions
