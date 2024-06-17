The word "attribute" comes from [Formal Concept Analysis](https://en.wikipedia.org/wiki/Formal_concept_analysis). Our context is different: we don't have what they call "objects" as pre-defined. Rather we are trying to discover our objects, by seeing them as those things that share a similar collection of attributes! Do see the [[Nuggets]] page for related thoughts.

Investigating this, we drew the following diagram:
![diagram](https://user-images.githubusercontent.com/1855141/219480019-bb2d65d7-52f9-41cc-afa0-c39ba7704565.jpg). Similar information is contained in Sam's [Chunk Observations](https://github.com/JacquesCarette/Drasil/wiki/Chunk-Observations).

It makes sense to summarize the attributes here in plain text too, annotated with their type, along with some commentary:

0. uid                :: UID
1. term               :: NP
2. abbreviation       :: String (always optional)
3. definition         :: Sentence
4. concept domain     :: [ UID ]
5. reference address  :: String
6. short name         :: ShortName
7. space :: Space
8. symbol :: Stage -> Symbol (always paired with a space)
9. unit :: UnitDefn (always optional)
10. constraints :: [ ConstraintE ]
11. reasonable value :: Expr (always optional)
12. uncertainty :: Uncertainty
13. inputs :: [ UID ]
14. expression :: Expr
15. model-expression :: ModelExpr
16. unit definition :: UnitDefn
17. unit symbol :: UnitSymbol
18. contributing units :: [ UID ]

We want to know which combinations are actually possible, and how do these occur in practice. Because these occur most naturally inside chunks, we need to know what constructors we have, and which ones are actually used, and by whom. There might be constructors used only internally, for example.

Rather than listing the chunks, what I'll list are the information combinations that seem to be possible. So {1} and {1,2} but not {1, 2?}.
- {1}
  - `nc :: String -> NP -> NamedChunk` (used a little in examples, some internally, lots in drasil-data)
  - `ncUID :: UID -> NP -> NamedChunk` (used only internally; but perhaps this should be the main one?)
- {1, 2}
  - sort of: `mkIdea :: String -> NP -> Maybe String -> IdeaDict` (used and abused in drasil-metadata, drasil-example (glassbr) and correctly in drasil-data)
  - sort of: `mkIdeaUID :: UID -> NP -> Maybe String -> IdeaDict` (only used internally in Chunk.Quantity)
  - sort of: commonIdea (see at {1,2,4} for sig) is used with `[]` in drasil-website, drasil-example, drasil-data and in Chunk.Concept)
  - sort of: commonIdeaWithDict (see at {1,2,4} for sig) is used with `[]` in drasil-example)
- {1, 3}
  - `dcc :: String -> NP -> String -> ConceptChunk` (internal (a lot), example, data)
  - `dccWDS :: String -> NP -> Sentence -> ConceptChunk` (internal, example, data)
- {1, 2, 4} 
  - `commonIdea :: String -> NP -> String -> [UID] -> CI` (abused everywhere it is used! See above)
  - `commonIdeaWithDict :: String -> NP -> String -> [IdeaDict] -> CI` (drasil-example, drasil-theory, drasil-data)
- {1, 2, 3}
  - `dcc' :: String -> NP -> String -> String -> CommonConcept` (example, few)
  - `dccWDS' :: String -> NP -> Sentence -> String -> CommonConcept` (never)
  - `cc :: Idea c => c -> String -> ConceptChunk` (will grab what is in the idea, which could be just {1}) (very little)
  - cc' (example and internal)
  
## Current Notes on Chunks
### From group meeting [#3750](https://github.com/JacquesCarette/Drasil/issues/3750)
![AtomsAndChunks](https://github.com/JacquesCarette/Drasil/assets/106560551/eddb4ecc-5b41-4a33-af4e-8fab733a71b3)

