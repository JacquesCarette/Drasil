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
        getA : c -> Maybe String
    }
    style Idea fill:yellow,fill-opacity:0.5
    Idea --|> NamedIdea

    class IdeaDict {
        + uid : UID
        + np : NP
        + abbr : Maybe String
    }
    IdeaDict ..|> HasUID : via uid
    IdeaDict ..|> NamedIdea : via np
    IdeaDict ..|> Idea : via abbr
    style IdeaDict fill:turquoise,fill-opacity:0.5
    
    

```
