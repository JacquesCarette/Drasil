To assist with progress on Drasil, we need to know the best practices for capturing and documenting (scientific and computing) knowledge.  We should also take advantage of any previous classifications and organizations of (scientific) knowledge.

# Glossary, Dictionary, Taxonomy and Ontology
We should have a good understanding of the definitions, and differences, between glossary, dictionary, taxonomy and ontology.  [The Data Maven](https://thedatamaven.net/2017/04/whats-the-difference-glossary-dictionary-taxonomy-ontology/) has an interesting take on the topic; another take is the paper [Clarity in the usage of terms Ontology, Taxonomy and Classification](https://pdfs.semanticscholar.org/b3e0/86fade8cbf248a3633e96b244611bbba26ce.pdf?_ga=2.165503295.1242279257.1537186598-259700611.1537186598). The headers below link to the appropriate page on Wikipedia (or W3C), and the definition is adapted from both those sources:

<dl>
  <dt><a href="https://en.wikipedia.org/wiki/Glossary">Glossary</a></dt>
  <dd>A glossary, also known as a vocabulary or clavis, is an alphabetical list of terms in a particular domain of knowledge with the definitions for those terms. Traditionally, a glossary appears at the end of a book and includes terms within that book that are either newly introduced, uncommon, or specialized.</dd>

  <dt><A href="https://en.wikipedia.org/wiki/Dictionary">Dictionary</a></dt>
  <dd>A dictionary is a collection of words in one or more specific languages, often arranged alphabetically, which may include information on definitions, usage, etymologies, pronunciations, translation, etc. It is a lexicographical product which shows inter-relationships among the data.

A broad distinction is made between general and specialized dictionaries. Specialized dictionaries include words in specialist fields, rather than a complete range of words in the language. Lexical items that describe concepts in specific fields are usually called terms instead of words, although there is no consensus whether lexicology and terminology are two different fields of study. Specialized dictionaries are supposed to be onomasiological, first identifying concepts and then establishing the terms used to designate them. 

There is also a contrast between prescriptive or descriptive dictionaries; the former reflect what is seen as correct use of the language while the latter reflect recorded actual use.</dd>

  <dt><a href="https://en.wikipedia.org/wiki/Taxonomy_(general)">Taxonomy</a></dt>
  <dd>Taxonomy is the practice and science of classification. A nice example: a
[taxonomy for static analysis](https://www.researchgate.net/publication/251940397_Taxonomy_of_static_code_analysis_tools).</dd>

  <dt><a href="https://en.wikipedia.org/wiki/Ontology">Ontology</a>&nbsp;(philosophy)</dt>
  <dd>Ontology is the philosophical study of being. More broadly, it studies concepts that directly relate to being, in particular becoming, existence, reality, as well as the basic categories of being and their relations. Traditionally listed as a part of the major branch of philosophy known as metaphysics, ontology often deals with questions concerning what entities exist or may be said to exist and how such entities may be grouped, related within a hierarchy, and subdivided according to similarities and differences.</dd>
  <dt><a href="https://www.w3.org/standards/semanticweb/ontology">Ontology</a> (computing)</dt>
  <dd>An ontology, is a formal naming and definitions of the types, properties and interrelationships of the entities that fundamentally exist for a particular domain of discourse. It is a practical application of philosophical ontology, with a taxonomy. An ontology compartmentalizes the variables needed for some set of computations and establishes the relationships between them. In the semantic web, ontologies can then be used for inference.</dd>
</dl>

# Lists of Ontologies

* [Where to publish and find ontologies? a survey of ontology libraries](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3293483/)
* [dbpedia - ontolology](https://wiki.dbpedia.org/services-resources/ontology)
* [Ontology list](http://info.slis.indiana.edu/~dingying/Teaching/S604/OntologyList.html) from a course [S604 - metadata & semantics](http://info.slis.indiana.edu/%7Edingying/S604Spring2011.html) by [Ying Ding](http://info.slis.indiana.edu/~dingying/) a professor of Cognitive Science at Indiana University.
* [List of ontologies](https://www.w3.org/wiki/Lists_of_ontologies) from W3C.
* The journal [Applied Ontology](https://www.iospress.nl/journal/applied-ontology/) appears to publish ontologies.

# Literature Review

(to do: give proper names to the links below)

## Possibly Relevant 
1. https://pdfs.semanticscholar.org/ecd5/fd6495da16f6b09a063c16029877151c9466.pdf
2. https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=6832486
3. https://ai.ia.agh.edu.pl/wiki/_media/pl:miw:2009:brankevaluationsikdd2005.pdf
4. https://pdfs.semanticscholar.org/7753/2e4ecc6ab2f0ff38922d5dc39413a370ad13.pdf
5. Natalya F Noy and Deborah L McGuinness.  Ontology development 101: A guide to creating your first
ontology, 2001
6. http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.41.7834&rep=rep1&type=pdf

## Research (Work In Progress)
### [VanRess2008](https://pdfs.semanticscholar.org/ecd5/fd6495da16f6b09a063c16029877151c9466.pdf) (Supplement to the link provided above)

Defines, compares, and discusses the three terms Taxonomy, Ontology, and Classification within the context of a formal paper. 

_**Classification**_
* Arrangement in groups or categories

_**Taxonomy**_
* Core property is that it possesses a hierarchical structure.

_**Ontology**_
* Provides a set of concepts from a certain domain that is well specified.
* [Gruber 1993] provides the definition “a specification of a conceptualization”

### [WupperAndMeijer1997](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.41.7834&rep=rep1&type=pdf)

Attempts to establish a set of fundamental definitions and notions to the fields of computer science and information technology. This is, in general, an attempt to clarify the relation of computer science and information technology with other scientific fields/disciplines. 

This is done with three separate frameworks: a formula, a diagram, and the tetrachotomy (_theory_, _method_, _language_, and _tool_)

### _**Formula**_

- Two fundamental notions which refer to the real world.

**Definition.** A _machine_ is a physical object which has been made to fulfill a well-defined purpose.

**Definition.** A _property_ is a physical phenomenon which can be ascribed to a physical object.

- Two supplementary notions which refer to the fundamental notions.

**Definition.** A _specification_ is a statement of properties, in some suitable language. It is said to _state_ a property to be obeyed or fulfilled.

**Definition.** A _schema_ is a description of the composition of part of which specifications are given. (i.e. to develop a solution)

- Problems accociated with the notions provided.

1. _adequacy_ is the problem of whether a machine has all desired properties.
2. _meaning_ is the problem of whether a specification states certain properties.
3. _acceptance_ is the problem of whether a machine fulfills a specification. 
4. _structure_ is the problem of whether a machine is an appropriate relisation of the schema.
5. _correctness_ is the problem of whether a schema satisfies a specification

### _**Diagram**_
Refer to the top of page 6 of http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.41.7834&rep=rep1&type=pdf

**Definition.** _To formalize_ to write specification that states the exact desired properties.

**Definition.** _To validate_ to establish that a specification states the desired properties
* Methods to ensure validation:
1. _insight_, when the specification is clear
2. _prototyping_, where the prototype is tested or measured
3. _reasoning_, where properties are mathematically proven

**Definition.** _To implement_ the creation of a machine which fulfills the given specification.

**Definition.** _To verify a machine against a specification_ to ensure that a machine fulfills its given specification.

**Definition.** _To realize_ the creation of a machine which is the realization of a given schema.

**Definition.** _To verify a machine against a schema_ to ensure that a machine is the realization of its given schema.

**Definition.** _To design_ finding a suitable schema which satisfies a given specification.

**Definition.** _To verify a schema against a specification_ to prove that a schema satisfies its given specification. 

### _**Tetrachotomy: a computer scientist's principal task**_

Separates complex results into the following classes:

**Definition.** A _theory_, is a collection of scientific laws/theorems.

**Definition.** A _method_, is a collection of notions, rules, and procedures which help to systematically find a solution for a given problem.

**Definition.** A _language_ or _formalism_, is a collection of terms with well-defined syntax and semantics.

**Definition.** A _tool_, is a computer system by which parts of the activities can be automated.