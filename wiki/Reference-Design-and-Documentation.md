# Table of Contents

- [Introduction](#introduction)
- [Why Change References](#why-change-references)
- [Changes](#changes)
    - [Using `UID` Lookup](#using-uid-lookup)
    - [Converting `Shortname` from a `String` into a `Sentence`](#converting-shortname-from-a-string-into-a-sentence)
    - [Adding Custom Display-Name Labels](#adding-custom-display-name-labels)
    - [Making `Reference` Constructors Concise](#making-reference-constructors-concise)
    - [Splitting up `References` from `RefInfo`](#splitting-up-references-from-refinfo)
    - [Removing `References` from the `Referable` Class](#removing-references-from-the-referable-class)
    - [Automatically Including References](#automatically-including-references)
- [What is in a Reference?](#what-is-in-a-reference)
- [How to use `References`](#how-to-use-references)
- [Troubleshooting](#troubleshooting)

# Introduction
In Drasil, references are used as a means to link relevant topics to one another, both for ease of reading the generated documentation, and for information encoding by connecting relevant ideas, theories, and definitions. This can be done internally through hyper-references, internally with citations, or externally with links. 


# Why Change References?
Previous to the Summer 2021 redesign of [`References`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:Reference), each reference was created and used in-line with `Sentences`. This meant that the `Sentence` itself actually held all of the data needed to produce a reference without needing a database. It only encapsulated the necessary surface-level information to display a reference. However, this method of forming references meant that `Sentences` could not be used to create `References` in a reliable manner. As such, it also restricted the use of `NamedIdeas` as a `shortname` for the reference, which in turn blocked the ability of linking complex ideas with the usual concept-level combinators. In the [Combinator Documentation](https://github.com/JacquesCarette/Drasil/wiki/Combinator-Documentation), many concept-level combinators have a variant with an appended `T`. The problem here is that the need for a "title-version" of a combinator should not exist; Drasil knows (or should know) how to handle different cases through the `NounPhrase` class and the use of `Sentences`. Before this revision, `References` used `Strings` for display names when it was not necessary. Using `Sentences` would be a much better fit for display names, as it allows Drasil to learn more about the reference itself. This way, we should be able to reduce the need for duplicate title-case versions of combinators and allow complex ideas with context to be referenced. In addition, this should allow for references to be created within normal sentences rather than needing a separate label for everything (see [#1142](https://github.com/JacquesCarette/Drasil/issues/1142), [#2489](https://github.com/JacquesCarette/Drasil/issues/2489), and [#2562](https://github.com/JacquesCarette/Drasil/issues/2562) for more information).


# Changes
### Using `UID` Lookup
[`References`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:Reference) may now be created within `Sentences` with the use of a `UID` rather than the actual reference itself. This forces the `References` themselves to be contained within a chunk database (similar to ConceptChunks, TheoryModels, Contents, etc.). The information for each reference is hidden until it is needed by the printer to be displayed. Any additional display information may be attached to a `Reference` when it is used in a `Sentence` by using `makeCiteInfo`. The information may contain things like page numbers and equation referrals. See the [`RefInfo`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:RefInfo) documentation for more information.
In turn, `References` must also be added to the chunk database (`ChunkDB`) through a reference map. The bottom of most example files contain a list of references to be mapped. Then they are all imported to `Body.hs` and placed in the reference map for the `ChunkDB` of that particular example.

### Converting `ShortName` from a `String` into a `Sentence`
`References` will now accept a `Sentence` as its `ShortName` rather than a `String`. The change allows for more flexibility in defining references and allows for `NamedIdeas` to translate directly into a reference. 

Primarily, this change was aimed at defining section labels in a cleaner way. Section labels should not need to use a `String`, instead they should be built off of a `NamedIdea` so that traceability and maintainability may be better preserved. Currently, each section will have a related `NamedIdea` that can be made into a reference.

As a second bonus to adding this, `ShortNames` can now be anything that exists within the bounds of a `Sentence`. This means that custom display name labels will be possible for in-text referencing.

### Adding Custom Display-Name Labels
Referring to issue [#1142](https://github.com/JacquesCarette/Drasil/issues/1142), the generated documents may now contain references with a label that is different from its internal `ShortName`. We can now use in-line references with custom text in a similar way to that of GitHub. With this change, a reference link does not necessarily have to be the internal label that was applied to it (or even a related label, for that matter). Of course, it is still good practice to give a descriptive label to all references, but this is especially useful for making the generated documents easier to read and give an overall cleaner style. It also has the added benefit of reducing duplicate occurrences of a phrase in a sentence and its related label.

### Making `Reference` Constructors Concise
Previously, `References` had to be defined by either building them manually or using these functions: `makeRef2`, `makeRef2S`, `makeCite`, `makeCiteS`, `makeCiteInfo`, `makeCiteInfoS`, `rw`. They all used to perform different actions, however they have recently become very similar. With the change to how `References` are put in a `Sentence` (by `UID` lookup), the different functions are not really needed. Instead, this change allows for more general functions to be defined. `ref` is now the primary way to create a reference, while `refS`, `refInfo`, `namedRef`, and `namedComplexRef` will be created. The functions for each of these will be defined in the [how-to section](#how-to-use-references) below.

### Splitting up `References` from `RefInfo`
Before this change, `References` had the option to carry display-level reference information (as [`RefInfo`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:RefInfo)). This display-level information included specific pointers to page numbers, equations, or an optional note that the Drasil printers would then append to the `Reference` itself. However, since the change to use `UIDs` to look up references in a database, it no longer became practical to keep the `RefInfo` attached to the reference itself. Keeping the `RefInfo` inside a `Reference` caused issues when the same `Reference` was used more than once (and used with different `RefInfo`). This could cause unwanted behavior such as a reference displaying different reference information than what was written in the actual code. Since the database is only able to hold one `Reference` per one `UID`, users would not have been able to include `RefInfo` inside a `Reference` internally. Instead, `RefInfo` is now used at a [`Sentence`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:Sentence) level. It is not encoded directly with the `Reference`, but instead appended to it and used when needed (much like a custom display name). So whenever a user needs to show display-level reference information, it will be encoded into Drasil at the `Sentence` level, which is meant to be used for display purposes.

However, there are some cases where we want the `Reference` to actually hold `RefInfo` without sinking to the level of `Sentences`. This is where [`DecRef`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:DecRef) comes in. `DecRefs` are record types that contain a `Reference` and its respective `RefInfo`. This way, we can form a bridge of sorts between `References` and `Sentences`. In particular, these are used in instance & theory models and general & data definitions. Constructing a `DecRef` is quite similar to a `Reference`, where `dRef` takes in a `Reference` and sets the `RefInfo` to `None`, and `dRefInfo` takes in a `Reference` and any additional `RefInfo` to be displayed.

### Removing `References` from the `Referable` Class
The original goal of this change was to make the `HasRefAddress` and `Referable` class distinct from one another. After all, a `Reference` itself is not exactly something `Referable`, but it does have a `HasRefAddress` and should be included in the `ReferenceMap` of the chunk database. Thus, this change encased two smaller changes:
- Changing `ref` to take in `HasRefAddress` rather than `Referable`. Since we want `ref` to be applicable to `References` but not include `Reference` as an instance of `Referable`, this change had to be made. Since the classes were similar, it was a straightforward projection, but it also forced the second change.
- `HasRefAddress` was initially only able to hold a `String`. However, that alone was not enough for Drasil to be able to formulate a full `Reference` from. So we needed a type with more context. Hence, [`LblType`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:LblType) became the new result of the method for the `HasRefAddress`. The `String` form is still available (through the `getAdd` function), but now Drasil could determine a `Reference` with context.

### Automatically Including References
Inside of `drasil-docLang/Drasil/DocumentLanguage.hs`, there is now a new `fillReferences` function. This allows users to use references just about wherever they wish without needing to worry about manually adding references to the project's chunk database. So long as the user plugs in all the models, definitions, sections, labelled content, concept instances, and citations, there should be no need for a user to manually input references. To explain the `fillReferences` function a bit more, it first takes a partially filled database (`System`) and a list of all the sections to be put in the SRS (`SRSDecl`). From there, it first searches through the `SRSDecl` for any `Section` and `LabelledContent` references. Then, it takes all the information from the `System` (everything that is a part of the `HasRefAddress` or `Referable` class), gathers it into one nice list, and places it back in the database of `System` for use by the generators.

# What is in a `Reference`?
A `Reference` contains an identifier, a reference address, a human-readable shortname for a display label, and any extra information about the reference. For more information, please see the [Haddock documentation](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:Reference).
`References` can be created from the following [`Referable`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:Referable) types (linked to the Haddock documentation for each): [`ConceptInstance`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:ConceptInstance), [`Citation`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:Citation), [`LabelledContent`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:LabelledContent), [`Section`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:Section), [`TheoryModel`](https://jacquescarette.github.io/Drasil/docs/full/drasil-theory-0.1.0.0/Theory-Drasil.html#t:TheoryModel), [`InstanceModel`](https://jacquescarette.github.io/Drasil/docs/full/drasil-theory-0.1.0.0/Theory-Drasil.html#t:InstanceModel), [`DataDefinition`](https://jacquescarette.github.io/Drasil/docs/full/drasil-theory-0.1.0.0/Theory-Drasil.html#t:DataDefinition), and [`GenDefn`](https://jacquescarette.github.io/Drasil/docs/full/drasil-theory-0.1.0.0/Theory-Drasil.html#t:GenDefn)

These types should be included in the chunk database as needed.

# How to use `References`
To reference something within a `Sentence`, these functions should be helpful: [`refS`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#v:refS), [`namedRef`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#v:namedRef), and [`namedComplexRef`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#v:namedComplexRef). Sometimes it is necessary to include the references within a TheoryModel, InstanceModel, DataDefinition, or GenDefn when defining a reference map. This can be done by accessing the list of references with a lens and the `getReferences` method from the `HasReference` class (Eg. use `(^. getReferences)`). To map the references to a database, use the [`ref`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#v:ref) function on everything that is referable and then include the list of references in the `symbMap` function of the `Body.hs` file.

Common functions and their uses:
- `ref`: Creates a `Reference` from anything that is `Referable`. Used in defining `References` in other functions or mapping `Referables` to a database
- `refInfo`: Similar to `ref` but allows for additional reference information to be added to a `Reference`.
- `refS`: Creates a `Reference` within a `Sentence`. Used often in drasil-examples.
- `namedRef`: Creates a `Reference` with the given display name into a `Sentence`. Used often in drasil-examples.
- `namedComplexRef`: Creates a `Reference` with the given display name and any additional reference information. Then wraps into a `Sentence`. Used often in drasil-examples.

# Troubleshooting
If an error similar to `Reference: UID not in ReferenceMap` occurs, try using `grep` on the `UID` to pinpoint which variable is being referenced. From there, track any functions that use this variable or include the variable inside the list of references to be added to the chunk database. `drasil-docLang/Drasil/DocLang/SRS.hs` is a common source of this, as sections (and their references) are automatically generated for the most part.

Please see [`Debugging in Drasil`](https://github.com/JacquesCarette/Drasil/wiki/Debugging-in-Drasil) for more information.
