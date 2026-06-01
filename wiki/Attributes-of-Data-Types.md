**Note:** The material in this page is likely all obsolete, and will be deleted at some point.

Branching from Issue [#514](https://github.com/JacquesCarette/Drasil/issues/514), it seems as though an attributes field should not necessarily be added to all data types. For example, looking at how types are defined as of commit [eef8b385](https://github.com/JacquesCarette/Drasil/commit/eef8b385823902a8af5f548fac64988877f20ae7#commit) in Constrained.hs, the following description should be kept in mind: 

- `ConstrainedChunk`, `ConstrConcept` (and `QuantityDict`) are all data-structures. We want to abstract over these as much as possible (which, in Haskell, is quite a lot). But, for sure, the constructor for them is not exported.
- `cuc'`, `constrained'` and `constrainedNRV'` are all "smart constructors".  They are not necessarily that 'smart', but what they do is give you a way to build commonly occurring cases in ways that should be more convenient than using the raw constructor. Of course `cuc'` is not particularly convenient... but that's a different issue.
- These data-structures store some data. They happen to be implemented as nested records. They could be implemented as 'flat' records, as tuples, etc. We want to not care about these details.
- As `ConstrainedChunk` already has a field for attributes, we don't want to add a second one. Same for the other data-structures.
- This means that when we build these things, we have two situations that arise:
  1) we're building something "from scratch". Then we want to **set** the attributes.
  2) we're build something from a pre-existing chunk. Then we want to **add** to the attributes.
- In the latter case, we have to make sure that the extra information does not somehow contradict what was there before. But this should not happen, because the chunks have the same `uid`. What this means, to us, but not enforced by the code, is that all chunks with the same `uid` should in fact be about the exact same thing. So adding new attributes is merely incremental building, rather than actual modification.
