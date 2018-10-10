A guide to Drasil, through its files. If Drasil was done in a literate style, i.e.
Drasil-in-Drasil, then this information would be there, somewhere.

Drasil is divided into package:
- **drasil-lang**: the language of basic Drasil knowledge
- **drasil-code**: representation of output programming languages
- **drasil-printers**: representation of output rendering languages, and renderers
- **drasil data**: actual Drasil knowledge, represented in the Drasil language
- **drasil-docLang**: the language of documents
- **drasil-gen**: the actual top-level generators, which interprets recipes and generates everything
- **drasil-example**: our current examples

So we can in turn dig into each package in more depth.

# drasil-lang

We will go through each of the files, in topological-sort order. Everything here is
under the namespace *Language.Drasil*, which will thus be omitted.

- **People**: Defines Person, which holds a person's name as data. People as lists of Person(s).
  A name can have many parts, and can follow Western or Eastern convention, or by a Mononym.
  Defines HasName class to extract a String version of a name, as well as some more
  specialized renderers.

- **Space**: Supposed to be a notion of 'space' where quantities live. Right now there is
  still confusion between space and type.

- **UID**: Defines abstract type of 'unique identifiers', which are used to tag everything
  uniquely, so that we can insert things into various internal databases. Basically a global
  primary key for all knowledge.

- **RefTypes**: (Should eventually disappear?). Currently defines different kinds of
  definitions (knowledge which belongs to documents), requirements and kinds of
  references. The 'kinds of reference' belongs here, but it shouldn't have a Show instance.
  And lots of the kinds of references are either layout-specific or document-specific, so
  should not be defined globally in either case.
