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

- **ShortName**: Abstract type for a 'short name', i.e. the string to be displayed
  for a link, visible to users.

- **RefTypes**: (Should eventually disappear?). Currently defines different kinds of
  definitions (knowledge which belongs to documents), requirements and kinds of
  references. The 'kinds of reference' belongs here, but it shouldn't have a Show instance.
  And lots of the kinds of references are either layout-specific or document-specific, so
  should not be defined globally in either case. Also defines Reference, which pulls these
  together.

- **Label.Type**: Label type. Is a reference address, link or URI.

- **Label.Core**: Label structure. Has a type, shortname and a reference type (hack?).
  (probably should be abstract, but isn't yet)

- **Symbol**: Abstract definition of symbol layout primitives. Enough information for
  renderers to display them.

- **Stages**: (perhaps misnamed?). An indication of which 'stage' of the processing an
  entity belongs to. Mostly used to configure the display of 'variables', i.e. which
  symbol to choose.

- **Classes.Core**: The 'core' classes which abstract over HasUID, HasShortName,
  HasRefAddress, HasSymbol.

- **Unicode**: Misnamed. Really is a few special characters which need special code to be
  displayed properly. Unclear how much this should be used in examples, so maybe this should
  no longer be exported?

- **SymbolAlphabet**: The long names of many symbols, and even some short ones (because of the
  tag) is inconvenient; provides some short-hand way of referring to them.

- **Expr**: mathematical expressions. And also domain descriptions and real intervals.
  (the latter two should probably be split off on their own)

- **Expr/Precendence**: definition of precedence of various mathematical operators.
  Used for both parsing and pretty-printing

- **Expr/Extract**:function to extract names, and thence dependencies, from Expr.

- **Chunk/Constrained/Core**: Definition of a constraint on variables, and the reason 
  why this constraint exists. These constraints are either an interval enclosure or an
  enumeration.

- **Development/UnitLang**: First, the 'Development' sub-hierarchy indicates that this 
  should not be exported from Drasil.hs at all, just from Drasil.Development.
  This defines the 'language of units', in two layers. First, as a 'laurent monomial'
  over basic symbols; and then as an extended type which takes into account synonyms
  scaling and shifting. This is the internal representation, which of course is only
  useful for printing. The combinators, one level up, for creating them, is more
  semantic.

- **Spec**: Should really be called 'Sentence'. A 'language of sentences', mostly to
  accomodate symbols, concatenation, units, etc. And some helper functions as well.

- **Sentence/Extract**: Utility to extract the (unique) dependencies of Sentences on
  chunks, i.e. things that have UIDs.

- **NounPhrase/Core**: Noun phrases, pluralisation rules and capitalization rules. 
  Used to deal with terms and other things which are defined as 'noun phrases'
  that are used in contexts where they then need a plural or capitalization. Easier
  to go via rules than to define all variants upon creation.

- **NounPhrase**: Constructors for noun phrases, and actual computations of plurals
  and capitalization. And a Class for NounPhrase, which gives the main methods on
  them. Right now, there is a single instance though.

- **Derivation**: A list of Sentences. In theory,
  this really ought to be a real data-structure, that alternates between narrative and
  equations. Could even be equational reasoning, etc.

- **Classes**: Defined a bunch of 'classy lenses'.  These can be understood as 
  our meta-model. NamedIdea, Idea, Definition, ConceptDomain, Concept, HasSymbol, etc.
  Really deserves a full write up.
  Sits 'on top' of all the important data-structures, and 'below' most of the
  actual chunks.
  (TODO: actually define the meaning of each of them)

- **Sentence/EmbedSymbol**: defines 'ch' to embed a chunk with a symbol into a
  sentence.  Defined here because HasSymbol is in Classes rather than Classes/Core.

- **Label**: routines for making labels.

- **Expr/Math**: constructors of Expr. 

- **Symbol/Helpers**: helper routines to check if symbol exists, and to extract the 
  Implementation or Equational stage symbol.

- **Chunk/NamedIdea**: defines data-structures that hold NamedIdea and Idea, their
  constructors and instances.

- **Chunk/CommonIdea**: Data-structure for an Idea which also has an abbreviation.

- **Chunk/Concept/Core**: Defines 3 data-structures.
  - ConceptChunk, which is an idea, definition and (concept) domain
  - CommonConcept, which is a common idea, definition and (concept) domain
  - ConceptInstance, which is a ConceptChunk that also has a shortname

- **Chunk/Concept**: (smart) constructors for all of the above.

- **Development/Unit**: UnitDefn, the data-structure for the definition of
  new units, and lots of constructors.

- **Development**: Export features that are primarily meant for extending the
  system rather than for developing examples.

- **Chunk/Quantity**: A 'Quantity' is an idea which has a Space, a Symbol and
  perhaps a unit (definition). It is meant to represent a value.

- **Chunk/Unitary**: A Unitary is a Quantity that must have a unit.

- **Misc**: Some, well, miscellaneous routines. To make a table, to check that
  a String has not spaces (hard error otherwise) and a routine to sort by 
  (Implementation!) symbol. [The latter function could be moved to docLang, but
  it is still odd]

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
