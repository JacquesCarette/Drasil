_Migrated from [#3196](https://github.com/JacquesCarette/Drasil/issues/3196)._

---

# What is a Chunk?

There are many types of 'fundamental knowledge' in Drasil, including (but not limited to):

* name
* abbreviation
* domains
* term
* symbol
* space / type
* constraints
* reasonable value
* unit
* uncertainty
* definition
* notes
* defining expression
* ...

_(Are these really "fundamental"? We don't have a good answer to that, but it has been sufficient so far.)_

If these fundamental pieces of knowledge are atoms, then chunks are molecules (i.e. collections of atoms). Like molecules, some can arise and some cannot; this means that there is "order" in how things assemble. The molecules that interest us are the ones that end up getting defined. This process was quite ad hoc; when we encountered a bunch of facts about a thing we were interested in that occurred in practice a bunch of times, we named it.

The classes that arise from that allow you to see particular atoms and sub-molecules that make sense on their own. The underlying theory we should be using is that of [Formal Concept Analysis](https://github.com/JacquesCarette/Drasil/wiki/Formal-Concept-Analysis). (Other analytical techniques might also make sense to use.) The properties here would be "has information X in it", with X from the list above. Our chunks are then the nodes of the lattice that occur in practice with classes to help us navigate the lattice.

An understanding of FCA also makes it clear that using `Maybe` is a hack: a proper concept should have an exact list of attributes that it embodies.

## The "Proper" Process for Forming Chunks

1. Settle on an analysis technique for concepts
2. List all the attributes we have
3. Derive the concepts we need (by using co-occurrence in our actual knowledge database)
4. Give names to these extracted concepts
5. Create data structures for these concepts
6. Create accessors for all information

This should really be done from scratch, but Dr. Carette is "quite confident that a lot of what we currently have will stay as is, or with minor modifications."

### Some Potential Chunks to be Formed

_(Note that this is just a brainstorming list; this knowledge could end up inside a chunk or be tracked in a different way.)_

* The local symbol used to represent a quantity. I think we currently "bake" the symbol into quantity making it difficult to change, but symbols aren't universal; they can change. In some cases, symbols are changed to avoid clashes between conventions when different domains are mixed (for instance sigma is used both for standard deviation, stress and the Stefan Boltzmann constant). In other cases, symbols are changed because of author/community preferences.
* The unit system. We implicitly (I believe) assume SI for everything, but we will also want to be able to use imperial units.
* Rationale information. For example, we may want to include rationale for constraints (see [#3197](https://github.com/JacquesCarette/Drasil/issues/3197)). Our "detailed derivations" currently provide rationale information for how we combine theories and assumptions to come up with a new theory.
* Refinement traceability information. Many theories will depend on other theories for their justification (rationale).
* Theory pre-conditions. Conditions that will need to be true to invoke a theory. That is you can only use a theory if you can satisfy the pre-conditions. The pre-conditions will be assumptions.
* Theory post-conditions. The conditions that have to be true once a theory has been invoked.



