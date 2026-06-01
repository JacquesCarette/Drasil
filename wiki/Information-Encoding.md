Drasil is based on *information* as the principal 'unit of knowledge' being manipulated. Information needs to be 'encoded', otherwise it's basically just a bunch of binary data, with which little can be done. So we need to tag things so that we know what is being represented. Then we build up a database of such information that can be re-used. In particular, we strive very hard to ensure that information exists only once in our database. As a side-benefit, we obtain traceability, as we can see where various pieces come from.

For example, there are a handful of basic physics laws for rigid body movement. Once these are captured, they can be re-used as often as needed. Thus our examples `Projectile`, `Pendulum` and `GamePhysics` all use some of the same base physics knowledge.

## Encoding 

The tricky part is to come up with an encoding for this information, so that the information is re-usable. If we represent `"F = ma"` as a string, the only thing we can do with that is print it. But if we know about force, mass, acceleration, equality, and multiplication, then this can be stored in ways that are more useful.

There ends up being multiple different kinds of information, all of which we want to structure. We need to be able to output text (in English, for now), mathematical expressions and code. We want all of these to be assembled *compositionally* as much as possible.

It is useful to recall that, as we learn from linguistics, philosophy and logic, that language is relative. Said differently, 'words' are an agreed upon convention, sufficiently well-known and well-shared to be useful as a means of communication. The most important conclusion from this is that we don't need to go "all the way down" to some atomic representation of everything. We too can simply use 'words' as encoding certain knowledge without necessarily having the system itself "understand" them beyond being simply words (or symbols or ...).

We know what we want to eventually generate: words, sentences, paragraphs, even whole documents with headings, table of contents and references, also containing formulas, tables and graphs. We'll also want to generate code of various kinds, including 'code' in DSLs such as Makefiles, configuration files, and the like. We have chosen to do this by trying to understand the basic 'units' of all of these artifacts, and the methods of compositions of these units into larger pieces.

Most importantly, we have chosen to approach this from a **bottom up** perspective: put in just enough structure as is useful to generate the artifacts while minimizing duplication. This is done iteratively, where duplication-full, simplistic versions are iteratively abstracted to remove duplication. Duplication removal is the principal source of 'clever encoding'.

### The bottom: math and English.

There are different kinds of information. Some is more of a mathematical nature, what are often called *expressions* and *formulas*. These are represented as embedded languages, [Expr](Expr) and [ModelExpr](ModelExpr). `Expr` is for well-understood mathematical expressions that are translatable to both visual representations and code representations. `ModelExpr` are mathematical expressions that do not necessarily end up in code, but occur commonly in specifications. There is also [CodeExpr](CodeExpr) for representing of code.

### The lower middle: small-scale tagging.

Next comes the kinds of information that is often the subject of things like [Ontologies](https://en.wikipedia.org/wiki/Ontology_(information_science)). We were unsure what information would be needed, so we built our own ``upper ontology'' in an ad hoc way as items arose. For example, things like *concepts*, *named ideas*, *quantities* and *unitals* (quantities with units) all arise naturally when encoding the information contained in research software.

First and foremost, all data at this level has a unique identifier (UID) which we can compare for equality.

For concreteness, a few examples:
- a **named idea** is simply a phrase (in English), like *Physics*, that we do not further define.
- a **common idea** is a named idea that has a (standard) abbreviation, like "one-dimensional" abbreviated as "1D"
- a **ConceptDomain** is a list of ideas; this can be seen as a relation, like "one-dimensional" being tagged as belonging to mathematics and physics.
- a **Definition** is regarded as a `Sentence` that somehow 'defines' an idea; for example "acceleration" is defined to be "the rate of change of a body's velocity".
The full set is documented at [[Chunks]].

However, it is also important to not that, in Drasil, this information occurs in two different guises: concrete representations and abstract interface. 

#### Concrete representation

We call the concrete representation "chunks" (which is a bad name, in retrospect, but we started from ideas from [literate programming](http://www.literateprogramming.com/)) captures *information that we have*. The different [kinds of chunks](Chunks) encode different kinds of information. These form a hierarchy (well, a graph, but it's harmless to think of it as a hierarchy for now), via more or less literal containment. We use Haskell records to implement them.

This representation is fragile because knowledge hierarchies are frequently complex lattices, and most programming languages don't support the kind of 'containment' that we'd really like.

#### Abstract interface

In actual usage patterns, we rarely care that our representation has exactly the information we need: it could have more. Of course, we care very much that it has **all** the information that is needed. Some level of polymorphism over concrete representations would thus be convenient.

This classic problem has a classic solution: interfaces. If our encoding offers the needed interface, then it can be used. In Haskell, interfaces are done via **type classes**.  To get the kind of representational polymorphism we want, we need to use *lenses* on top of that (getters are often sufficient, but we use full lenses nevertheless).  This particular combination of features goes by the name **classy lenses**.

Our abstract interfaces are documented at ????.

### Higher: Theories

At a higher level still, we have **theories** and **models**.

(todo)

### Recipes

(todo)

## Example
It can be difficult to see the volume of information we are communicating from our encoded databases to our generated artifacts. To help visualize just how much information is repeated in a software project, we took a snippet of each section of generated artifacts from GlassBR and organized them in the figure below. The figure itself is meant to look almost chaotic and disorganized, because that is how software tends to organically grow. Each shape/colour represents a section of information that is pulled from the Drasil source (top left box).

![image](https://user-images.githubusercontent.com/69334555/129775028-56b17d61-1ae2-4487-83c4-d1bb54ba8e80.png)

## In Practice
Please follow the guide as discussed in [Guidelines for Adding New Types and Typeclasses](Guidelines-for-Adding-New-Types-and-Typeclasses-in-Drasil#Using_Haskell_types_for_Drasil).
