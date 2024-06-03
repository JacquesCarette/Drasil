This is a question-driven exploration of future work on Drasil.

1. What artifacts occur in **software projects**?
    - it would be useful to know the relative frequency, as a very crude means of prioritization
        - what kinds of software projects? **A:** for the purposes of artifacts, 'any'. The more popular the project, the higher weight it can be given.
        - where to find software projects? **A:** github (and gitlab and ...)
        - how many projects in sample? **A:** as many as automated tools will support
        - projects may be missing important artifacts/ infrequent important artifacts, even in large sample? **A:** how *important* can an artifact be if it's rare? As an artifact, it can't be. Note that the information that ought to be in such artifacts (example: requirements) really could be missing.
        - does frequency in the sample truly reflect best practice? **A**: absolutely not. It reflects common practice only.
        - what can we do with this information? **A**: get a sample of artifacts used in common practice, and reverse-engineer from them the information that they contain.
2. What information is contained in those artifacts?
    - more precisely, how can the information be classified into different kinds?
        - relationships between classifications? is this information useful? **A**: the definition of the kinds would be tailored to be useful, by construction. So yes, it is entirely possible to created kinds that may be 'explanatory' from a human perspective, but be useless from the point of view of **generate everything**. 
        - frequency of information between artifacts? which classifications? information shared between classifications? **A**: like the above, any classification would be for the purpose of making generation easier.
3. How would one encode those different kinds information (so as to be re-usable)?
    - ex: math expressions, sentences, semi-structured sentences, semi-structured sets of these, 'theories', etc.
        - best practice? **A**: As this is currently not done (from the perspective of generation) outside of code generation, there isn't really much 'best practice' that exist. All current practice is experimental.
        - other practices? novel ways of encoding such information? **A**: some people use ontologies. Others use DSLs. Some config files are kind of related. Some people embed this information directly in generators. There's a hodge podge of stuff.
        - optimize? **A**: way too early to think of that. It needs to 'work', as a binary optimization problem...
4. What is the **provenance** of that information?
    - provenance will reveal meta-artifacts that may not typically be in projects, but exist nonetheless
        - how to trace? **A**: don't know
        - categorize provenance? **A**: don't know
        - categorize meta-artifacts? **A**: don't know
        - what to look for? **A**: don't know
        - how to formalize such information in the projects? **A**: don't know
        - how can this help in future projects? **A**: don't know
5. When taking all the information in a set of of artifacts as a coherent whole,
    - what is the joint information?
    - what is the unique information?
        - how connected? [Need more information about the question to answer]
        - what can we learn from these connections? [ditto]
6. How does one transform encoded information into the form it appears in final artifacts?
    - i.e. this is a reverse operation to the encoding step(s) above
        - final artifact form? **A**: this is defined by each of the artifacts. Ex: latex SRS, ASCII README.md, plain (unicode) text Java, etc
        - how is it encoded? decoded? **A**: answering this question is core to the research itself
        - what are the current best practices? **A**: roughly speaking, this is no current practice, beyond Drasil and org-mode, and even then, it isn't really well specified.
        - novel ways? **A**: yes please!
7. How to store a 'database' of such information so that it can be reused?
        - how categorized? links? relationships?  what can we learn? how can we take advantage? can we extract more information? **A**: these are good generic questions, but too generic to be meaningfully answered. A decent answer will depend on the exact details of the answer to some of the questions above.
8. How does one specify a set of transformations that makes up a 'recipe' from information to artifacts?
        - relationship between transformations? type of data? **A**: roughly speaking, same as above: good generic question, but not easily answerable on its own.
9. How does one make that set of transformation modular?
10. Observation: a software system (like Drasil) is itself a software project - could Drasil be encoded in Drasil?
    - inspired by compilers, micro-passes is probably a good way to go
        - where are we? **A**: a working prototype that contains *de facto* partial answers to many of the above questions, embedded in Drasil itself.
        - what are the current goals? **A**: understand Drasil's own design, at a high level, enough to document it all.
        - where to start? **A**: read the original vision paper; read the draft white paper; read the wiki articles describing the current knowledge; read the code itself
        - what do we need to know? **A**: don't knows. Partial answers to some of the questions above will help.

Stuff to add to above:
- new backends (Matlab, C, Rust, ...)
- testing as a cross-cutting approach, i.e. the information that is contained in a test case (input, calling a function/feature, expected behaviour/output, how to check that expected behaviour did happen / expected output was produced, how to understand and report unexpected behaviour) is related to information in many other artifacts (thus the cross-cutting)
- version control of software as a generic activity
- workflow, idealized (Drasil) process
- very different domains, i.e. software is made up of many different domains, all of which need a common encoding
- embedded computational knowledge, i.e. a description (pseudo-code) of 'algorithms' done in such a way that a Drasil recipe can weave together domain information and computational knowledge into something like high-level GOOL, from which actual code can be generated
- best practices (of software engineering), to guide us to what to aim for
- common practice (of software engineering, to guide us to what parts people find most useful *today*
- [generalized definition of developer productivity](https://arxiv.org/abs/2009.14015)