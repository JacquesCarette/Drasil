# Summary of Chunks
**[`ConceptChunk`](https://github.com/JacquesCarette/Drasil/blob/09365d7d1f5dc9b54e84a8d64cb601184456ea0c/code/drasil-lang/lib/Language/Drasil/Chunk/Concept/Core.hs#L28-L36):** an `IdeaDict` with a definition (`Sentence`) and a domains(s) (`[UID]`)

```haskell
-- | The ConceptChunk datatype records a concept that contains an idea ('IdeaDict'),
-- a definition ('Sentence'), and an associated domain of knowledge (['UID']).
--
-- Ex. The concept of "Accuracy" may be defined as the quality or state of being correct or precise.
data ConceptChunk = ConDict { _idea :: IdeaDict -- ^ Contains the idea of the concept.
                            , _defn' :: Sentence -- ^ The definition of the concept.
                            , cdom' :: [UID] -- ^ Domain of the concept.
                            }
makeLenses ''ConceptChunk
```

**[`CommonConcept`](https://github.com/JacquesCarette/Drasil/blob/09365d7d1f5dc9b54e84a8d64cb601184456ea0c/code/drasil-lang/lib/Language/Drasil/Chunk/Concept/Core.hs#L51-L54):** a common idea (`CI`) with a definition (`Sentence`)

```haskell
-- | Contains a common idea ('CI') with a definition ('Sentence').
-- Similar to 'ConceptChunk', but must have an abbreviation.
data CommonConcept = ComConDict { _comm :: CI, _def :: Sentence}
makeLenses ''CommonConcept
```

**[`ConceptInstance`](https://github.com/JacquesCarette/Drasil/blob/09365d7d1f5dc9b54e84a8d64cb601184456ea0c/code/drasil-lang/lib/Language/Drasil/Chunk/Concept/Core.hs#L71-L79):** a `ConceptChunk` with a reference address (`String`) and a `ShortName`

```haskell
-- | Contains a 'ConceptChunk', reference address, and a 'ShortName'.
-- It is a concept that can be referred to, or rather, a instance of where a concept is applied.
-- Often used in Goal Statements, Assumptions, Requirements, etc.
--
-- Ex. Something like the assumption that gravity is 9.81 m/s. When we write our equations,
-- we can then link this assumption so that we do not have to explicitly define
-- that assumption when needed to verify our work.
data ConceptInstance = ConInst { _cc :: ConceptChunk , ra :: String, shnm :: ShortName}
makeLenses ''ConceptInstance
```

# Issues with Chunks
## Potential Overlap Between `IdeaDict` and `CI` Based on Abbreviations
An `IdeaDict` is a `NamedChunk` that might have an abbreviation (`Maybe String`). A `CI` is a `NamedChunk` with an abbreviation (`String`) and a domains(s) (`[UID]`). What is the point of differentiating between an idea with and without an abbreviation? In my mind, it would be easier to use one chunk that might have an abbreviation (`Maybe String`), but I know we are trying to move away from `Maybe`s when possible. However, I don't know enough about lenses to know if there's another way around this. Is keeping `IdeaDict` and `CI` separate intentional? Should they be merged? It seems like there is some work being done on this, from [this discussion](https://github.com/JacquesCarette/Drasil/issues/2679#issuecomment-892072342) and [the issue about using `Lens`es instead of `Maybe`s](https://github.com/JacquesCarette/Drasil/issues/2677).

## Domains are Stored at Inconsistent Levels
One quirk of `CI` is that it takes a domain(s) Even though [it's been pointed out](https://github.com/JacquesCarette/Drasil/issues/2679#issuecomment-892072342) that domains shouldn't be added at the `Idea` level. However, `ConceptChunk` chunk that takes an `IdeaDict` _also_ takes a domain(s), so the domain(s) are almost implicitly associated with the `IdeaDict`. Should the domains be added up a level by whatever chunk is using the `CI` instead of being in the `CI` itself? What conditions cause a chunk to require a domain(s)?

Interestingly, `IdeaDict` is also present in `QuantityDict`, which is present in `ConstrainedChunk` and `UnitaryChunk`, where the former is also present in `UncertainChunk`. None of these types are given domains, although they have similar chunks with domains (as shown below):

|        Chunk       |     Similar Chunk     |   Has Domain from...  |
|:------------------:|:---------------------:|:---------------------:|
|  `UncertainChunk`  |       `UncertQ`       |    `ConstrConcept`    |
| `ConstrainedChunk` |    `ConstrConcept`    | `DefinedQuantityDict` |
|   `UnitaryChunk`   |     `UnitalChunk`     | `DefinedQuantityDict` |
|   `QuantityDict`   | `DefinedQuantityDict` |     `ConceptChunk`    |

So all of these chunks get a domain from `ConceptChunk`, which is just a `CommonConcept` with a domain(s) and _definitely_ (as opposed to `Maybe`) has an abbreviation, where `CommonConcept` is just a `CI` with a definition (`Sentence`).
