This page documents certain design choices made during the development of Drasil along with their rationales.

## Synonyms

The idea of synonyms came up on a few occasions, most notably issue [#166](https://github.com/JacquesCarette/literate-scientific-software/issues/166). In our discussion (Nov 2, 2017) we came to the conclusion that there are two main points to consider on whether or not to include them:

- Should Drasil help us with knowledge-curation in our artifacts? This would mean restricting the use of synonyms completely.

OR
- Should Drasil embed the meta-knowledge, thereby linking synonymous chunks in the database? We would allow these synonyms to be used in place of each other as desired throughout the artifacts.

Restricting the use of synonyms would thereby reduce ambiguity in the generated artifacts as the readers would not need to be aware of any synonymous terms. This could raise issues with the *flow* of the English language generation, but would otherwise be clear.

Capturing the meta-knowledge of synonyms would be useful for many domains, but without proper care they could be used in very confusing ways (ie. switching back and forth between synonyms in one artifact, giving the appearance of distinct chunks OR using a different synonym for each artifact giving the appearance of inconsistency).

Several of the case study examples have used synonyms, which has already led to confusion (as evidenced in issue [#163 ](https://github.com/JacquesCarette/literate-scientific-software/issues/163), mentioned in #166), ultimately leading to our decision of foregoing synonyms in favour of reducing ambiguity.

## Lenses, Smart Constructors, and data-types

We want to be as modular as possible.  In particular, we want to be polymorphic over exact details of data-structures. In our implementation, that means that we have a slew of data-structures that offer related interfaces. In our case, all our data-structures are sources of information, so this means that they have 'fields' that contain data of interest, but where the particular storage details are of no concern to us. [Classical encapsulation, the simplest sort of information hiding]. The way we achieve this polymorphism is, of course, through *interfaces*. In this case, we use the *Classy Lenses* approach: the `typeclass` allows us to express "obeying that interface" as a constraint, and the `Lens'` lets us focus in on the needed data however it is stored in what could be an otherwise 'messy' representation.

To take advantage of this, none of the data-constructors themselves are exported (obviously). Instead, functions are given which give a more intensional interface to building these types. They can also, of course, enforce some invariants before actually building the required type. Furthermore, they can be built so that their inputs are themselves specified by constraints rather than via explicit types (at least for some entries). This is extremely convenient, as it allows any data-structure with all the sufficient information to be used, regardless of whatever extra information they may possess. Somewhat like subtyping in OO -- but just for records. So there is no contravariance problem.

## `Label`, `isLabel`, and `ContentChunk`
See [Issue #567](https://github.com/JacquesCarette/Drasil/issues/567#issuecomment-393539890).