_Migrated from [#3196](https://github.com/JacquesCarette/Drasil/issues/3196)._

---

# `Maybe` Abbreviations

Abbreviations shouldn't be required, since a lot of knowledge is well-defined but doesn't possess an abbreviation. However, when abbreviations exist, they should be used. From a pure programming point-of-view, that seems to scream for `Maybe`, but we've learned that this is not a good solution. It seems that a better solution seems to lie at the "knowledge retrieval" stage, where we can have functions that retrieve abbreviations if they exist, and our code should deal with the fact that abbreviations are not always present. (See [When to Use Maybe](https://github.com/JacquesCarette/Drasil/wiki/When-to-Use-Maybe).)

# Chunks that are Guaranteed to Have an Abbreviation?

Reconstructing our thinking from ~6-7 (!!!) years ago, some "concepts" seemed more important than others because they have an abbreviation. While this observation seems to still hold, it was baked into our data representation, which might have been a mistake in retrospect. It seems odd to enforce the existence of an abbreviation, since it is really something that _may_ exist. Therefore, the distinction between `NamedChunk`s and `IdeaDict`s, for example, might not be useful.

We really do need to go back to the blackboard (perhaps even literally!) and revisit all our chunks (their contents, their name, their intent, their constructors). An in-person design meeting is likely needed. (This might be worth it in an even larger sense; see [Chunk Observations](https://github.com/JacquesCarette/Drasil/wiki/Chunk-Observations).)

---

Beyond this, if we decide that attaching a domain at the Idea level is a code smell, then I propose we merge the two chunks. Otherwise, I think that keeping them separate makes sense: both would have a Maybe String for an abbreviation, and CI would also contain a list of domains (we could even make this more explicit by having a CI be an IdeaDict and a list of domains).
