> [!NOTE]
> Much of this page is outdated. See https://github.com/JacquesCarette/Drasil/issues/4447.

<details>
<summary>Outdated Information</summary>

As the basis for all information encoding in Drasil, chunks have become an integral part of allowing us to use and maintain the current database of knowledge. At its core, a chunk is a data type specialized in holding a specific type of information for a specific purpose. For example, [`NamedChunks`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:NamedChunk) are often used for objects that have a unique identifier and an associated term. [`ConceptChunks`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:ConceptChunk) mirror real-world concepts by including the idea, definition, and domain for a particular concept. Something like a [`QuantityDict`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:QuantityDict) can have an [idea](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:Idea), the [space](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:Space) in which it exists, [units](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:UnitDefn) and a [symbol](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:Symbol). Many other chunks exist within Drasil that allow the program to hold the required information and its meaning so that knowledge may be used in generated models, definitions, and theories.

## Structure
Chunks are usually made up of lower-level types with different purposes. A chunk whose purpose is to hold all the information needed for a mathematical variable would need a symbol, description/definition, and units (as shown below). This particular example gives a name to the concept which is built from a quantity and its units. The structure of a chunk can be thought of as a wrapper of sorts. It encases only the necessary information to perform its job, but its contents may be unwrapped and used one at a time. The wrapper itself may be wrapped again with more things added to it (like an abbreviation or a domain). This is primarily how one idea can be built upon in Drasil.

![ChunkDiagram](https://user-images.githubusercontent.com/69334555/126186580-4e763924-969e-47b2-aac9-42063efdb7c3.png)

Alternatively, here is a diagram of the 'wrapping' analogy. We first start with an identifier, then build up to an idea with a name, then a concept, so on and so forth:

![image](https://user-images.githubusercontent.com/69334555/129757553-d2ae6f57-f95c-4d4d-bc27-cbf080d4dd02.png)

## Implementation

So, how do we represent this in code? Conveniently, we can use Haskell's record-type syntax along with [lenses](Lenses) to define, set, and get the information we need from within the chunk wrapper. This way, we can wrap wrappers without worrying about the "level" of wrapping around one particular identifier. Using this, one [`UID`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:UID) can be represented in a hierarchy of chunks, with no information loss when upgrading to a larger chunk. A straightforward example of this is the progression from a lower-levelled [`NamedChunk`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:NamedChunk) to something much larger like a [`TheoryModel`](https://jacquescarette.github.io/Drasil/docs/full/drasil-theory-0.1.0.0/Theory-Drasil.html#t:TheoryModel). One of the smallest chunks (`NamedChunk`) is defined as follows:
```Haskell
data NamedChunk = NC {_uu :: UID, _np :: NP}
```
It contains a unique identifier (`UID`) and a term that can be used in creating sentences (as a noun phrase, `NP`). As of now, we don't know what this `NamedChunk` is or what it can do, but we do know that it exists and we can use it in a sentence with proper pluralization and capitalization. Most likely, these chunks will be common nouns that are significant enough to have a name. Two `NamedChunks` may also be combined to produce a new `NamedChunk` that carries both of their terms. We can start to define single words and simple ideas like `table_` and `symbol` and then combine those to make a `tableOfSymbol` `NamedChunk` idea, which is more complex. Using the wrapper analogy, we unwrap the term from `table_` and `symbol`, then rewrap them after placing an "of" between them to get a `tableOfSymbol` chunk.

A `NamedChunk` can either be used as a method for getting a defined term or build upon. The "next step" up from a `NamedChunk` is an [`IdeaDict`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:IdeaDict), which contains a `NamedChunk` and maybe an abbreviation. We can see the direct progress in its type definition:
```Haskell
data IdeaDict = IdeaDict { _nc' :: NamedChunk, mabbr :: Maybe String }
```
As we continue to learn more about what exactly we want this chunk to represent, we can gain more specifics about the idea and directly create a richer type to work with such information. From this point, there are many options available to continue adding information. If the idea should be made into a concept, we can use a [`ConceptChunk`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:ConceptChunk) to wrap the idea along with a definition and its domain:
```Haskell
data ConceptChunk = ConDict { _idea :: IdeaDict -- ^ Contains the idea of the concept.
                            , _defn' :: Sentence -- ^ The definition of the concept.
                            , cdom' :: [UID] -- ^ UID of the domain of the concept.
                            }
```
If we know the concept is a quantity or can be treated as one, it may become a [`QuantityDict`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:QuantityDict) or [`DefinedQuantityDict`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:DefinedQuantityDict):
```Haskell
data DefinedQuantityDict = DQD { _con :: ConceptChunk
                               , _symb :: Stage -> Symbol
                               , _spa :: Space
                               , _unit' :: Maybe UnitDefn
                               }
```
By continuously wrapping the information needed, we can successfully encode relevant knowledge in a useful and practical manner.

Eventually, we build up relevant chunks through seeing common patterns in examples and actual documentation. We have various high-level chunks dedicated to units (`UnitDefn`, `UnitaryConceptDict`, `UnitaryChunk`, `UnitalChunk`), relations (`RelationConcept`), quantities (`QuantityDict`, `DefinedQuantityDict`), uncertainties (`UncertainChunk`, `UncertQ`), and much more. Our foundation of knowledge is built upon these chunks, and the strong typing of Haskell really emphasizes the semantic meaning that should be associated to each type. As Drasil grows, more and more chunks will be added with different chunk types, thereby allowing our database of knowledge to grow alongside it. For more information on the chunks currently available in Drasil, please see the [Haddock documentation](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html).

</details>

## Documentation of Chunks
This section contains a list of the chunks currently defined in `drasil-lang` (as of August 21, 2026), along with a short description for each of them.
| Chunk Name | Description | Example |
| --- | --- | --- |
| [`IdeaDict`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:IdeaDict) | Used to make a concept which has a term and maybe an abbreviation. | The project name "Double Pendulum" may have the abbreviation "DblPendulum". |
| [`CI`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:CI) | A common idea is something that is worth naming. However, it also includes an abbreviation and the domains of knowledge in which it appears. | The term "Operating System" has the abbreviation "OS" and comes from the domain of computer science. |
| [`ConceptChunk`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:ConceptChunk) | Used to make a concept that has a term and definition. It may also be tagged with some domain of knowledge.| The concept of "Accuracy" may be defined as the quality or state of being correct or precise.|
| [`DefinedQuantityDict`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:DefinedQuantityDict) | For when we want to assign a quantity to a concept. Includes the space, symbol, and units for that quantity. | A pendulum arm can be defined as a concept with a symbol (l), space (Real numbers), and units (cm, m, etc.). |
|[`ConstrConcept`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:ConstrConcept) | A `DefinedQuantityDict` with constraints and maybe a reasonable value. | Measuring the length of a pendulum would have some reasonable value (between 1 cm and 2 m) and the constraint that the length cannot be a negative value. |
| [`UnitDefn`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:UnitDefn) | Comprised of a concept, unit symbol, and a list contributing units. | Meter is a unit of length defined by the symbol (m). |
| [`UncertQ`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:UncertQ) | A `ConstrConcept` with an uncertainty. | Measuring the length of a pendulum arm may be recorded with an uncertainty value. |
| [`QDefinition`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:QDefinition) | Building off of a `DefinedQuantityDict`, we now have a defining expression with inputs, a definition. Used to make definitions and models. | Finding the velocity of a pendulum arm through a `QDefinition` would entail an equation to find velocity and input values. |
 
### Analyzing Chunks

It can be quite difficult to see the dependencies of each chunk, so making graphs and data tables (by running `make analysis`) can help us to fine-tune which chunks should exist and which chunks need to be modified.
