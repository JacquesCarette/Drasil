Unlike the [Related Work](Related-Work) the work here has different aims and goals than Drasil. But it contains ideas that have inspired us. At a minimum, we reuse the ideas, but sometimes much more.

## Domain Knowledge

[Domain Knowledge](https://en.wikipedia.org/wiki/Domain_knowledge) goes hand-in-hand with [Domain Engineering](https://en.wikipedia.org/wiki/Domain_engineering) (see also [Domain Engineering for SE](https://en.wikipedia.org/wiki/Domain_(software_engineering))), [Domain-driven design](https://en.wikipedia.org/wiki/Domain-driven_design) and [Product family engineering](https://en.wikipedia.org/wiki/Product-family_engineering) (see also [[Program Families|Inspiration#program-families]] below). Domain Knowledge was coined by the inventor of Draco, a (dead) system with definite overlap with Drasil in its aims.

Note that care must be taken: there are (large) pockets of research around domain knowledge which are suffused with OO and UML. If you see that, run away. Domain knowledge should be completely independent of the paradigm that will eventually be used for implementation.

From here, it's an easy jump to [Ontology](https://en.wikipedia.org/wiki/Ontology_(computer_science)) and [Knowledge Representation](https://en.wikipedia.org/wiki/Knowledge_representation_and_reasoning) as being 'relevant topics'. Unfortunately, those topics are rife with theory and much thinner on practical, actionable advice.

Drasil takes from this the idea that the "domain knowledge" is some of the most important information to have around. It is both stable and highly reusable.

[Dines Bjørner](https://www.imm.dtu.dk/~dibj/) has written some of the clearest work on domain knowledge and its use in SE.

## Biform Theories

[Biform Theories](https://arxiv.org/abs/1805.02709) (see the references in that paper for more) join together axiomatic representations and computational representations of mathematical knowledge.

From this, we generalize that knowledge can have multiple representations that "belong together" as they use a different language (and perhaps logic, paradigm, etc) but are still supposed to 'be the same'.  One can compute that '5 + 9' is '16' and also prove the statement '5 + 9 = 16' (and, in some cases, the proof is trivial because the system internalizes computation). But also "five plus nine is equal to sixteen" is clearly related. This is where 'triform theories' come from.

## Validity Frames

To be precise, we kind of re-invented these, by stumbling towards them informally. Now we have a proper name for some of our constraints as well as our use of theory refinement.

I found the paper [Exploring Validity Frames in Practice](https://link.springer.com/chapter/10.1007/978-3-030-58167-1_10) to be a good introduction.
## Program Families

## Views (from software architecture)

## Reproducible Research

## Formal Concept Analysis

This has [its own page](formal-concept-analysis).