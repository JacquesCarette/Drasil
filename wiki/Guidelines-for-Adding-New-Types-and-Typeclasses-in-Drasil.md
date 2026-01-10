Please see [Chunks](Chunks), [Recipes](Recipes), and [Information Encoding](Information-Encoding) for more details on the use of data types in Drasil.

## Using Haskell types for Drasil

At a higher level, here is how we embed knowledge in Haskell:
- *data constructors* (whether ADTs or GADTs) are useful when we have multiple potential choices, and we need to discover at the point of use what we have, as these can be queried (by pattern matching)
- *class methods* are useful when we have many different representations, but they all share some core ideas. At the point of use, we just want "the idea" (via a function) and don't care about the details at all.
- *record fields* (with data) are useful when we have a specific representation that's made up of pieces, and we want to access the whole and the pieces
- *record fields* (with all functions) are one way to implement fake objects; they're basically equivalent to (Haskell) class methods

The above is a slightly edited copy of [@JacquesCarette's discussion](https://github.com/JacquesCarette/Drasil/issues/2853#issuecomment-1034427454) on a similar topic.

## Type Dependency Graphs

To see a table of all up-to-date type dependency graphs in Drasil, visit our [website](https://jacquescarette.github.io/Drasil/#Sec:Analysis). There are also class-instance dependency graphs along with module dependency graphs.

## Datatypes list (Updated June 2021)

### Types in `drasil-lang`

See the [Haddock Documentation](https://jacquescarette.github.io/Drasil/docs/drasil-lang-0.1.60.0/Language-Drasil.html)

### Typeclasses

The typeclasses used by Drasil can be found in the [Haddock Documentation](https://jacquescarette.github.io/Drasil/docs/index.html). Specifically, the [Language.Drasil](https://jacquescarette.github.io/Drasil/docs/drasil-lang-0.1.60.0/Language-Drasil.html) and [Drasil.Database](https://jacquescarette.github.io/Drasil/docs/drasil-database-0.1.1.0/Drasil-Database.html) contain some that may be helpful to reference.