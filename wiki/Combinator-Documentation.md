# Table of Contents

- [Introduction](#introduction)
- [Naming](#naming)
    - [Note for Users](#note-for-users)
- [Sentence-level Combinators](#sentence-level-combinators)
- [NounPhrase-level Combinators](#nounphrase-level-combinators)
- [Concept-level Combinators](#concept-level-combinators)
- [Higher-level Combinators](#higher-level-combinators)
- [`NounPhrases` in Drasil](#nounphrases-in-drasil)
- [Notes](#notes)

# Introduction
The purpose of this page is to help users and contributors easily see the combinators for English that currently exist in Drasil and are available for use. This will also help to keep all of the different combinators organized and to separate them by their English meanings. As brought up in issue [#2399](https://github.com/JacquesCarette/Drasil/issues/2399), we are trying to encapsulate the knowledge associated with common English combinators as a means to generalize Drasil's language while making readability of the code clearer. So far, we have separated these combinators into four levels:
* [Level 1: `Sentence` level](#sentence-level-Combinators). These combinators can only be used to join two `Sentences`, and they do not hold any semantic meaning usable by Drasil. In other words, they are only surface level combinators and are unable to produce a sophisticated idea from two simpler ideas. When joining concepts, these should only be used when the relationship between the phrases is unable to be expressed with any higher-level combinators.
* [Level 2: `NounPhrase` level](#nounPhrase-level-Combinators). The combinators listed here are better than `Sentence`-level combinators as they can hold information about pluralization and capitalization rules. As a result, Drasil can utilize these combinators in more powerful ways because it can know the rules that govern how an applied `Sentence` should look. Because of the nature of the `NP` type (having the ability to hold knowledge of singular, plural, capital, and title cases), these combinators can easily replace many `Sentence`-level combinators and give Drasil more knowledge when recording information.
* [Level 3: `NamedIdea` or conceptual level](#concept-level-Combinators). These combinators are similar to the `NounPhrase` combinators, except they are able to hold the semantic meaning of the combinator. These represent a new idea that is built upon multiple simpler ideas, allowing Drasil to grow its knowledge of such ideas. The combinators at this level are ideal and practical for use in Drasil, but they are more difficult to implement. One must be able to see the underlying patterns that are within English, and then apply them using these combinators. For example, something like the `ofThe` combinator will allow us to contain the knowledge that the first idea is a unique attribute of the second idea.
* [Level 4: Things above `NamedIdeas` that are amenable to joining](#higher-level-Combinators). This is ideal and something to aim for, but a little too early to start incorporating consistently.

## Naming
The setup for naming combinators is as follows (in order):
- An underscore (`_`) is used when a combinator name would otherwise clash with either Haskell's syntax or any other similarly named functions. Common uses include `of_`, `a_`, `and_`, etc. The only exceptions to this rule are the following combinators - `the_ofThe`, `the_ofGive`, and `the_isExpctToHvC`, which are named so because they prepend the word "the" in front of the first term.
- An appended `C` denotes that the first word is capitalized (for `Sentence` types). For example, the only difference between `the_ofThe` and `the_ofTheC` is that the first word will be capitalized (in this case, the first "The").
- An appended `T` denotes combinators that are meant for use in titles. All words are either capitalized (for `Sentence` types) or have some special attribute defined by the following letters in the function definition that determine the title case (for `NP` and `NamedIdea` types). For example, `forT` will capitalize both terms and insert the word "for" between them. Combinators without the `T` are still perfectly usable in titles, but this is mainly for when we want to give specific functions to display the title case.
- An appended `Gen` denotes the general (or custom) case for a combinator. It often takes functions which can specify the creation of singular, plural, or title cases. An appended `GenGen` takes functions for both singular and plural cases. For example, `forGen` takes two functions and two terms, applies the two arguments to the two terms and then insets "for" between them.
- Two appended `P`s or `S`s often denote the formation of a plural case (commonly used for `NP` and `NamedIdea` types). For example, the plural case of `forPS` makes the first word plural and the second word singular.
- Four appended `P`s or `S`s denote the formation of the singular and plural cases with the first two letters for the singular case and the last two letters for the plural case (commonly used for `NP` and `NamedIdea` types). For example, the singular case of `forPSPP` makes the first term plural and the second term singular while the plural case becomes the plural of both terms.
- Appended `NI`s or `NP`s denote the types of arguments the combinator should take. These combinators have the ability to combine a `NamedIdea` and a `NounPhrase` together respectively and are always appended last to avoid confusion with any appended `P`s or `S`s. For example, `of_PSNPNI` joins a NounPhrase and a NamedIdea together with the word "of" between them. The plural case then follows the `P` and `S` (pluralize the first term, keep the second term singular).  

Here is an example with some of the different possible pseudo-combinations using `ofThe` and the phrase `"angle of the pendulum"`:
| Combinator | Singular Case | Plural Case | Capital Case | Title Case|
|---         |---            |---          |---           |---        |
| `ofThe` | angle of the pendulum | angle of the pendulums | Angle of the pendulum | Angle of the Pendulum |
| `ofThePS` | angle of the pendulum | angles of the pendulum | Angle of the pendulum | Angle of the Pendulum |
| `ofTheSPPP` | angle of the pendulums | angles of the pendulums | Angle of the pendulums | Angle of the Pendulums |
| `ofTheTPS` | angle of the pendulum | angles of the pendulum | Angles of the pendulum | Angles of the Pendulum |

*** Note that these may not make sense in actual English, this is just an example to demonstrate notation. For more information, please visit the [Haddock documentation](https://jacquescarette.github.io/Drasil/docs/index.html).***

## Note for Users
When using these combinators, try to import the lower level modules as qualified. This way, when writing code, we may be deliberate about our choices and conscious of the level of combinator that is being used. This also allows for the naming scheme to work well; each level of combinator can follow the same rules without having to be named in different ways. `Sentence` level combinators should be imported as `S`, `NounPhrase` level as `NP`, and concepts may not be qualified. This also encourages the use of more concept-level combinators, as it makes the code cleaner and easier to read.

## Sentence-level Combinators
Many of these combinators act like inserting words into the middle of an English sentence (unless noted otherwise). Please see the [Haddock documentation](https://jacquescarette.github.io/Drasil/docs/drasil-utils-0.1.1.0/Utils-Drasil-Sentence.html) for more details on each combinator.

Combinators with "the":
- `andThe`
- `fromThe`
- `inThe`
- `isThe`
- `toThe`
- `ofThe`
- `the_ofThe` (combines two `Sentences` with "of the" and prepends "the" to them)

Common combinators (without "the"):
- `of_`
- `ofA`
- `or_`
- `and_`
- `are`
- `in_`
- `is`
- `for`
- `andIts`

Uncommon combinators:
- `the_isExpctToHvC` (prepends "The" before combining with "is expected to have")
- `the_ofGiv` (prepends "the" before combining with "of a given")
- `versus`
- `denotes` (inserts "denotes the")
- `wrt` (inserts "with respect to")
- `defnAs` (inserts "defined as")

Unusual combinators:
- `forT` (titleizes and combines two `NamedIdeas` into a `Sentence` using "for")
- `forGen` (titleizes and takes two functions to apply to `NamedIdeas` before combining into a `Sentence` using "for")
- `forTPS` (titleizes and pluralizes first `NamedIdea` before combining into a `Sentence` using "for")
- `forTPP` (titleizes and pluralizes both `NamedIdeas` before combining into a `Sentence` using "for")

## NounPhrase-level Combinators
These combinators have many variations, as pluralization and capitalization rules may vary greatly between different use cases. Please see the [Haddock documentation](https://jacquescarette.github.io/Drasil/docs/drasil-utils-0.1.1.0/Utils-Drasil-NounPhrase.html) for more details and the specifics of each combinator. (Subject to change: Naming convention appending NP to the end of the combinator name)

Generalized combinators (mostly used as helpers for other combinators but can still be used for a custom combinator):
- `insertString` (inserts a given `String` into two `NPs`. Plural case pluralizes second term.)
- `insertSent` (inserts a given `Sentence` into two `NPs`. Plural case pluralizes second term.)
- `prependString` (prepends a given `String` to a `NP`)
- `prependSent` (prepends a given `Sentence` to a `NP`)

Article combinators (the, a):
- `the`
- `theGen` (accepts a function to determine plural case)
- `a_`
- `a_Gen` (accepts a function to determine plural case)

Common combinators (appending a `'` means plural case pluralizes first term. Appending `''` means plural case accepts two functions.):
- `ofThe`
- `inThe`
- `the_ofThe`
- `for`
- `of_`
- `and_`

Unusual combinators:
- `with` (no special plural or general case)
- `of_GenGen` (accepts functions for both singular and plural cases)
- `and_GenGen` (accepts functions for both singular and plural cases)

## Concept-level Combinators
These combinators take `NamedIdeas` and make a `NP` from them. Because of the variety of pluralization and capitalization, there are many variants of each combinator. Please visit the [Haddock documentation](https://jacquescarette.github.io/Drasil/docs/drasil-utils-0.1.1.0/Utils-Drasil-Concepts.html) for more details and the specifics of each combinator. (Subject to change: Naming scheme with `'` and `_`)

Common combinators:
- `and_`
- `andIts`
- `andThe`
- `for`
- `of_`
- `ofA`
- `the`
- `inThe`
- `with`
- `toThe`
- `onThe`
- `ofThe`
- `the_ofThe`

## Higher-level combinators
Please see [Haddock documentation](https://jacquescarette.github.io/Drasil/docs/drasil-utils-0.1.1.0/Utils-Drasil-Concepts.html) for more information.

`NamedIdea` and `NamedChunk` combinators (does not preserve abbreviations):
- `compoundNC`
- `compoundNCPP`
- `compoundNCGen`
- `compoundNCPS`
- `compoundNCGenP` (takes only one function for the first term)
- `compoundNCPSPP`

Other `NP` and `NamedIdea` combinators (may be moved to Drasil.Language):
- `combineNPNI`
- `combineNINP`

## `NounPhrases` in Drasil
The most common (and complex) construction of a complex `NounPhrase` used in Drasil is by using the `Phrase` constructor. Using this constructor allows us to build up phrases with proper pluralization and capitalization. When we want to use a `NounPhrase` in a `Sentence`, Drasil often calls upon the `Ch` constructor to hold the `UID` of the `NounPhrase` that will be resolved later by looking up the `UID` in the `ChunkDB`. The `Ch` constructor also holds the style and capitalization that the term should be displayed in. The style can either be singular form, short form, or pluralized form. By doing this, each of the above combinators can be used for any of the following forms: singular without capitals, singular with first word capitalized, singular with all words capitalized, plural without capitals, plural with first word capitalized, plural with all words capitalized. So rather than having six combinators for the same phrase, we only use one and determine the specific case we need afterwards. The short forms are more rare and thus use the `Gen` form of a combinator when needed.

## Notes
Future changes include: 
- [x] Removing `s`,`NP` naming convention and allowing qualified imports to determine the different levels of combinators.
- [x] Change method for naming different Singular and Plural operators as follows: 
    - `_` used for combinators that would otherwise clash with Haskell syntax
    - Base combinators will default plural case to `singular t1, plural t2`
    - append `S` and `P` to specify the plural case
    - append `T` to specify title case
    - append `Gen` for general case (no specified plural case)
- [x] Remove duplicate titleized versions of combinators.
- [ ] Eventually move to a richer language that can encompass semantics better, like the [Grammatical Framework](https://www.grammaticalframework.org/).




