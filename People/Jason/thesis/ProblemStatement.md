Drasil has de-duplicated knowledge across SCS artifacts relevant to
specifications and code. Through codifying knowledge and collecting a coherent
set of information in a knowledge database, we are able to generate a wide
variety of software artifacts (e.g., OO programs [Java, C++, Python, Swift, and
C#] with guided usage via Makefiles, and requirements specifications [HTML and
TeX]). This codified knowledge was de-duplicated from an originating set of
artifacts via bottom-up gathering, however, we should be able to use the same
knowledge to generate more artifacts in different languages, flavours, and with
more options. However, each desired artifact language has its own way of
encoding information (such as mathematical expressions). This leaves us needing
to teach Drasil more about the targeted languages (and, at times, about the
existing codified knowledge) in order to reliably generate usable artifacts.
Mathematical expression and theory encoding becomes a key point of interest for
us because they are used in across the board (e.g., derivations, code,
constraints, definitions, etc.). Drasil relies on a single universal untyped
mathematical language to describe general mathematical knowledge (theories).
Unfortunately, this results in unreliable and brittle conversions of
mathematical knowledge into other forms because, we, lack information about
their structure (leading to inflexible conversions to other forms), don't
statically know when expressions are admissible in different contexts (e.g., in
code generation, derivations, etc.), and we don't know when are well-formed
(well-typed). As more theories are codified and typed, Drasils knowledge
database faces difficulties in scaling since it relies on a single unique map
for each type of knowledge, resulting in an ever-growing list of maps and a
tediously precise means of knowledge collection and reference.

Research questions:

1. Drasil has a language of simple mathematical expressions that are used in
   multiple contexts. But not all expressions are valid in all contexts. How do
   we 'fix' that? 

      (~Answered by the division of Expr into CodeExpr/Expr/ModelExpr)

2. Drasil's current encoding of "theories" are essentially black boxes. We would
   like to be able to use some structural information present in the short list
   of the 'kinds' of theories that show up in scientific computing. How do we
   codify that?

      (~Answered by the creation of ModelKinds)

3. How can we ensure that our language(s) of simple mathematical expressions
   encode 'valid' expressions?

      (~Answered by the typing of Expr)

4. Our current "typed" approach to collecting different kinds of data is hard to
   extend. How can we make it easier to extend?

      (~Answered by upgrading ChunkDB to accept almost any type that obeys
      certain requirements)
