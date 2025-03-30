# Examining Drasil's Chunks

Let:
* Boxes represent chunks or typeclasses:
  * Yellow boxes represent typeclasses.
  * Turquoise boxes represent standalone chunks.
  * Red boxes represent chunks that “extend” others, reusing a `UID`.
* Solid line arrows with an empty triangle head from `B` to `A` represent
  extension.
* Dashed line arrows with an empty triangle head from `B` to `A` represent
  typeclass satisfaction with commentary explaining _how_ `B` satisfies `A`.

in:

```mermaid
---
title: Drasil's Chunks
config:
  class:
    hideEmptyMembersBox
---
classDiagram
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% Drasil.Language.Idea
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    class HasUID["HasUID c"] {
        <<typeclass>>
        uid : Lens' c UID
    }
    style HasUID fill:yellow,fill-opacity:0.5

    class NamedIdea["NamedIdea c"] {
        <<typeclass>>
        term : Lens' c NP
    }
    style NamedIdea fill:yellow,fill-opacity:0.5
    NamedIdea --|> HasUID

    class Idea["Idea c"] {
        <<typeclass>>
        getA : c → Maybe String
    }
    style Idea fill:yellow,fill-opacity:0.5
    Idea --|> NamedIdea

    class IdeaDict {
        + uid : UID
        + np : NP
        + abbr : Maybe String
    }
    IdeaDict ..|> HasUID : uid
    IdeaDict ..|> NamedIdea : np
    IdeaDict ..|> Idea : abbr
    style IdeaDict fill:turquoise,fill-opacity:0.5
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% Drasil.Language.ShortName
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    class HasShortName["HasShortName c"] {
        <<typeclass>>
        shortname : c → ShortName
    }
    style HasShortName fill:yellow,fill-opacity:0.5
    note for HasShortName "ShortName is a Sentence newtype."
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% Drasil.Language.Concept
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    class Definition["Definition c"] {
        <<typeclass>>
        defn : Lens' c Sentence
    }
    style Definition fill:yellow,fill-opacity:0.5

    class ConceptDomain["ConceptDomain c"] {
        <<typeclass>>
        cdom : c → [UID]
    }
    style ConceptDomain fill:yellow,fill-opacity:0.5

    class Concept["Concept c"] {
        <<typeclass>>
    }
    Concept --|> Idea
    Concept --|> Definition
    Concept --|> ConceptDomain
    style Concept fill:yellow,fill-opacity:0.5

    class ConceptChunk {
        ^ _idea : IdeaDict
        + _defn' : Sentence
        + cdom' : [UID]
    }
    style ConceptChunk fill:red,fill-opacity:0.5
    ConceptChunk ..|> HasUID : idea . uid
    ConceptChunk ..|> NamedIdea : idea . term
    ConceptChunk ..|> Idea : idea's getA
    ConceptChunk ..|> Definition : defn'
    ConceptChunk ..|> ConceptDomain : cdom'

    class ConceptInstance {
        ^ _icc : ConceptChunk
        + ra : String
        + shnm : ShortName
    }
    style ConceptInstance fill:red,fill-opacity:0.5
    ConceptInstance ..|> HasUID : icc . idea . uid
    ConceptInstance ..|> NamedIdea : icc . idea . term
    ConceptInstance ..|> Idea : icc . idea's getA
    ConceptInstance ..|> Definition : icc .  defn'
    ConceptInstance ..|> ConceptDomain : icc . cdom'
    ConceptInstance ..|> HasShortName : shnm
    ConceptInstance ..|> HasRefAddress : ???
    ConceptInstance ..|> Referable : ra, getRefAdd
```

Note: Regrettably, the above might be difficult to read. You can try
copy/pasting the code into [mermaid.live](https://mermaid.live/edit) to view a
more interactive version.

## Examples and Usage

* [IdeaDict](./examples/IdeaDict.md)
