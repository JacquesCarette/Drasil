A guide to Drasil, through its files. If Drasil was done in a literate style, i.e.
Drasil-in-Drasil, then this information would be there, somewhere.

Drasil is divided into package:
- drasil-lang: the language of basic Drasil knowledge
- drasil-code: representation of output programming languages
- drasil-printers: representation of output rendering languages, and renderers
- drasil data: actual Drasil knowledge, represented in the Drasil language
- drasil-docLang: the language of documents
- drasil-gen: the actual top-level generators, which interprets recipes and generates everything
- drasil-example: our current examples

So we can in turn dig into each package in more depth.

* drasil-lang

We will go through each of the files, in topological-sort order.
