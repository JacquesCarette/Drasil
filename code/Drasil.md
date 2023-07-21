A guide to Drasil, through its files. If Drasil was done in a literate style, i.e.
Drasil-in-Drasil, then this information would be there, somewhere.

**Last updated April 18, 2019**. For more up-to-date information, please see individual package READMEs or the [wiki](https://github.com/JacquesCarette/Drasil/wiki/SubPackages).

Drasil is divided into packages:
- **drasil-lang**: the language of used to describe basic knowledge
- **drasil-build**: representation of build system languages and renderers
- **drasil-code**: representation of output programming languages
- **drasil-printers**: representation of output rendering languages, and renderers
- **drasil data**: actual knowledge, represented in the Drasil language
- **drasil-docLang**: the language of documents
- **drasil-gen**: the actual top-level generators, which interprets recipes and
  generates everything
- **drasil-example**: our current examples

So we can in turn dig into each package in more depth.

# drasil-lang

We will go through each of the files, in topological-sort order. Everything here is
under the namespace *Language.Drasil*, which will thus be omitted.

- **Data/Date**: representation of dates. Mostly Month for now, but will extend, or
  replace with something more standard. (Data.Dates from dates-0.2.2.2.2 ?)
  (Data.Time from time-1.9.2?)

- **Misc**: Some, well, miscellaneous routines. To make a table, to check that
  a String has not spaces (hard error otherwise) and a routine to sort by 
  (Implementation!) symbol. [The latter function could be moved to docLang, but
  it is still odd]

- **People**: Defines Person, which holds a person's name as data. People as
  lists of Person(s).
  A name can have many parts, and can follow Western or Eastern convention, or by a Mononym.
  Defines HasName class to extract a String version of a name, as well as some more
  specialized renderers.
  Should probably also contain a |Maybe ORCID|

- **Space**: Supposed to be a notion of 'space' where quantities live. Right now there is
  still confusion between space and type.

- **Stages**: (perhaps misnamed?). An indication of which 'stage' of the processing an
  entity belongs to. Mostly used to configure the display of 'variables', i.e. which
  symbol to choose.

- **UID**: Defines abstract type of 'unique identifiers', which are used to tag everything
  uniquely, so that we can insert things into various internal databases. Basically a global
  primary key for all knowledge.

- **ShortName**: Abstract type for a 'short name', i.e. the string to be displayed
  for a link, visible to users.

- **Label.Type**: Label type. Is a reference address, link or URI.

- **Unicode**: Misnamed. Really is a few special characters which need special code to be
  displayed properly. Unclear how much this should be used in examples, so maybe this should
  no longer be exported?

- **Symbol**: Abstract definition of symbol layout primitives. Enough information for
  renderers to display them.

- **Expr**: mathematical expressions. And also domain descriptions and real intervals.
  (the latter two should probably be split off on their own)

- **Expr/Precendence**: definition of precedence of various mathematical operators.
  Used for both parsing and pretty-printing

- **Expr/Extract**:function to extract names, and thence dependencies, from Expr.

- **Constraint**: Definition of a constraint on variables, and the reason 
  why this constraint exists. These constraints are either an interval enclosure or an
  enumeration.

- **ShortHands**: The long names of many symbols, and even some short ones (because of the
  tag) is inconvenient; provides some short-hand way of referring to them.

- **Classes/Document**: classes about documents (mostly citation right now)

- **Classes/Core**: The 'core' classes which abstract over HasUID, HasShortName,
  HasRefAddress, HasSymbol.

- **Symbol/Helpers**: helper routines to check if symbol exists, and to extract the 
  Implementation or Equational stage symbol.

- **RefProg**: Defines |Reference|.

- **Expr/Math**: constructors of Expr. 

- **UnitLang**:
  This defines the 'language of units', in two layers. First, as a 'laurent monomial'
  over basic symbols; and then as an extended type which takes into account synonyms
  scaling and shifting. This is the internal representation, which of course is only
  useful for printing. The combinators, one level up, for creating them, is more
  semantic.
  [It should really be done as an 8-tuple... but that can be done later]

- **Sentence**: Should really be called 'Sentence'. A 'language of sentences', mostly to
  accomodate symbols, concatenation, units, etc. And some helper functions as well.

- **Sentence/Extract**: Utility to extract the (unique) dependencies of Sentences on
  chunks, i.e. things that have UIDs.

- **NounPhrase/Core**: Noun phrases, pluralisation rules and capitalization rules. 
  Used to deal with terms and other things which are defined as 'noun phrases'
  that are used in contexts where they then need a plural or capitalization. Easier
  to go via rules than to define all variants upon creation.
  [Uses |Sentence|, which is really not right. But later routines use the lazy
   properties of Sentence, so refactoring this is complicated.]

- **Document.Core**: the various types that make a Document. Rather syntactic in nature.

- **NounPhrase**: Constructors for noun phrases, and actual computations of plurals
  and capitalization. And a Class for NounPhrase, which gives the main methods on
  them. Right now, there is a single instance though.

- **Derivation**: A list of Sentences. In theory,
  this really ought to be a real data-structure, that alternates between narrative and
  equations. Could even be equational reasoning, etc.

- **Data/Citation**: internal representation of the data in citations, and their
  smart constructors.

- **Chunk/Citation**: citations as a chunk (record, with accessors, etc)

- **Classes**: Defined a bunch of 'classy lenses'.  These can be understood as 
  our meta-model. NamedIdea, Idea, Definition, ConceptDomain, Concept, HasSymbol, etc.
  Really deserves a full write up.
  Sits 'on top' of all the important data-structures, and 'below' most of the
  actual chunks.
  (TODO: actually define the meaning of each of them)

- **Chunk/NamedIdea**: defines data-structures that hold NamedIdea and Idea, their
  constructors and instances.

- **Chunk/CommonIdea**: Data-structure for an Idea which also has an abbreviation.

- **Chunk/Concept/Core**: Defines 2 data-structures.
  - ConceptChunk, which is an idea, definition and (concept) domain
  - ConceptInstance, which is a ConceptChunk that also has a shortname

- **Chunk/Concept**: (smart) constructors for all of the above.

- **Development/Unit**: UnitDefn, the data-structure for the definition of
  new units, and lots of constructors.

- **Development**: Export features that are primarily meant for extending the
  system rather than for developing examples.

- **Chunk/Quantity**: A 'Quantity' is an idea which has a Space, a Symbol and
  perhaps a unit (definition). It is meant to represent a value.

- **Chunk/Unitary**: A Unitary is a Quantity that must have a unit.

- **Chunk/Eq**: Short for 'Equation' which means a Quantity which has a definition,
  both in words and through an equation. Called QDefinition.
  (Should not be exported, DataDefinition should be used instead)

- **Chunk/DataDefinition**: A DataDefinition is a QDefinition which is decorated
  with a variety of information. Should be the main structure used.

- **ChunkDB**: Contains a 'chunk database', for quantities, concepts, units and
  ideas.

- **Chunk/UnitaryConcept**: A join between a concept and a unitary, which is
  another way to say quantity+unit+definition+concept-domain.

- **Chunk/Theory**: A Theory is a collection of types, defined quantities,
  definitions and relations (and more - see the comments in the file).

- **Chunk/Relation**: A concept and a relation. Used (currently) by general definition.

- **Chunk/GenDefn**: General Definition. See wiki for current thoughts on that.
  A relation concept that may have units, but also a derivation, references, a label
  and notes.

- **Chunk/DefinedQuantity**: Very weirdly named, since the 'definition' here is that
  from Concept, i.e. just a Sentence. Basically join(Concept,Quantity).

- **Chunk/InstanceModel**: A 'relation concept' that has some stuff identified
  as inputs and outputs (and constraints on those). Like with GenDefn, should probably
  use a finer distinction that relation. See issue #1030.

- **Chunk/Unital**: DefinedQuantity guaranteed to have a unit.

- **Chunk/Constrained**: Either a Quantity or a DefinedQuantity (two types defined)
  that have constraints and maybe a default value.

- **Chunk/UncertainQuantity**: Either a Quantity or a DefinedQuantity that has
  some uncertainty.

- **Development/Sentence**: (bad name) short-cuts for building Sentences.
