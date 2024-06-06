# Meeting Summary for June 7th, 2023

Originally, “chunks” were captured through a nesting egg structure, where a
hierarchy of extensions on an essential base (e.g., a noun phrase) defines a
single chunk. This has been the working model of defining in Drasil, but has
drawbacks in practice, primarily with ergonomics (it requires a complex network
of types which is difficult to debug and analyze). [@JacquesCarette
analyzed](https://github.com/JacquesCarette/Drasil/wiki/Attributes-and-Chunks)
the existing chunk hierarchy, resulting in groups of chunks that were highly
related. However, we encountered difficulties in recreating the explanation
behind the division of the chunks, leading us to reevaluate their existence.

## Objective

This meeting focused on (re-)understanding the first pool found by
@JacquesCarette's analysis (`a`-`f` on the whiteboard figure --
[`NamedChunk`](https://github.com/JacquesCarette/Drasil/blob/ad9a64f442cf76f2bd52534d9eff797a8f734564/code/drasil-lang/lib/Language/Drasil/Chunk/NamedIdea.hs#L34-L43),
[`IdeaDict`](https://github.com/JacquesCarette/Drasil/blob/ad9a64f442cf76f2bd52534d9eff797a8f734564/code/drasil-lang/lib/Language/Drasil/Chunk/NamedIdea.hs#L67-L75),
[`CommonIdea`](https://github.com/JacquesCarette/Drasil/blob/ad9a64f442cf76f2bd52534d9eff797a8f734564/code/drasil-lang/lib/Language/Drasil/Chunk/CommonIdea.hs#L21-L28),
[`ConceptChunk`](https://github.com/JacquesCarette/Drasil/blob/ad9a64f442cf76f2bd52534d9eff797a8f734564/code/drasil-lang/lib/Language/Drasil/Chunk/Concept/Core.hs#L28-L36),
[`ConceptInstance`](https://github.com/JacquesCarette/Drasil/blob/ad9a64f442cf76f2bd52534d9eff797a8f734564/code/drasil-lang/lib/Language/Drasil/Chunk/Concept/Core.hs#L71-L79),
and
[`CommonConcept`/`CI`](https://github.com/JacquesCarette/Drasil/blob/ad9a64f442cf76f2bd52534d9eff797a8f734564/code/drasil-lang/lib/Language/Drasil/Chunk/Concept/Core.hs#L51-L54))
and updating it as necessary.

## Analysis

The focus around these chunks is to teach Drasil about _names_ (and their
meanings). Names provide at least a few things: giving an identifier to
concepts, unambiguously reusing and referring to those concept, and enabling
pattern discovery in usage thereof. The chunks allow for names to have
abbreviations, definitions, domains, and classifications, but with extra
plumbing around the declarations for flexibility (specifically in declaring).
However, this flexibility is costly to maintain due to needing us to maintain
multiple chunks for a particular name when we typically use them all for many
words. For example, an `IdeaDict` is used to add abbreviations to `NamedChunk`s
(basic noun phrases) while a `CommonConcept` is used to add an abbreviation to a
`ConceptChunk`, the overlap in what they provide largely overlaps, but we have
to create 2 types to add abbreviations to words. A `CommonIdea`/`CI` adds an
abbreviation and a domain of knowledge to `NamedChunk`s, which is very similar
to `ConceptChunk` (a name with a definition and optional domain) and
`CommonConcept`s. A `ConceptInstance` is also similar to `ConceptChunk`s with a
“level” difference but with many of the same possible extensions and plumbing.
Together, these chunks are complex and require careful work to make sure we can
make extensions possible from any particular chunk.

## Results

We deem this group of chunks to be too similar to be unique, and too complex to
be (easily) usable and maintainable. As such, we decided to merge them into a
single chunk based on `NamedChunk`, which we decided to call `Concept`s.

### Concepts

A “Concept” is a noun phrase that denotes _something_ (anything!). Ones that are
explainable have a _definition_ in natural language (e.g., English). Sometimes
they have _abbreviations_ (such as NASA), and _domains_ and/or _classifications_
of which they belong (such as Astronomy and Public Organizations). Concepts
should be used to capture noun phrases that we expect to use (and reuse) or when
we want to discover patterns of their usage.

### Action Items

1. Merge `NamedChunk` and `IdeaDict` into a single type. All `NamedChunk`s can
   become `IdeaDict`s without issue.
2. <<pending @JacquesCarette's analysis>>

## Meeting Blackboard Notes

![20230607_152644](https://github.com/JacquesCarette/Drasil/assets/1627302/ce81c84a-04fc-4ef5-af5b-46d108889aab)
![20230607_152641](https://github.com/JacquesCarette/Drasil/assets/1627302/9897023d-f7c0-41e4-9b9e-782c9799f368)
![20230607_152650](https://github.com/JacquesCarette/Drasil/assets/1627302/31953d60-ac90-495a-8ce9-719653170c98)
