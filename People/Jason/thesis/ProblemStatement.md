Drasil has de-duplicated mathematical expressions across software artifacts
relevant to specifications and code. As part of Drasils aim to de-duplicate
everything across scientific software artifacts (primarily that which is related
to specifications and code), efficient and reliable mathematical knowledge
communication becomes important. Specifically, de-duplication requires us to
form a means of discussing the intersection of the duplication while retaining
an understanding of why it appeared in different contexts, how it appears, and
what role it takes on in different contexts. As Drasil currently relies on a
single universal mathematical language to describe mathematical knowledge,
issues arise in its usage, primarily focused around how and when it is usable in
different contexts. Additionally, as more knowledge is de-duplicated and
formalized, our stored memory needs to be capable of scaling up to allow for all
kinds of new types.


Research questions:
1. How does context affect what mathematical terms we discuss?
   (~Answered by the division of Expr into CodeExpr/Expr/ModelExpr)
2. How can we rationalize mathematical theories/knowledge in Drasil? How can we codify theories?
   (~Answered by the creation of ModelKinds)
3. How can we ensure our mathematical expressions are syntactically and semantically 'valid' constructions?
   (~Answered by the typing of Expr)
4. How can we allow our limited-accepting knowledge-base to one that can accept almost anything?
   (~Answered by upgrading ChunkDB to accept almost any type that obeys certain requirements)
