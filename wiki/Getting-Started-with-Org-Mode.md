# Org Mode

Given that we may expand Drasil in the future so that it can generate org mode files for emaccs, we will paste some relevant "getting started" links and information here.  With org mode we can write our documentation using a "markdown notation like" notation and we can mix the documentation with code as a literate document.  The org mode document (mixing documentation and code) can be exported to multiple formats, including html, LaTeX and code.

## Emacs links

[Getting Started with Emacs](https://lucidmanager.org/productivity/getting-started-with-emacs/)

[Emacs Cheat Sheet](https://www.gnu.org/software/emacs/refcards/pdf/refcard.pdf)

[Musas: A Life Configuring Emacs](https://alhassy.github.io/emacs.d/)

[Elisp Cheat Sheet](https://github.com/alhassy/ElispCheatSheet/blob/master/CheatSheet.pdf)


## Org Mode links

[Org Mode Beginning At the Basics](https://orgmode.org/worg/org-tutorials/org4beginners.html)

[Literate Programming with Org Mode](http://howardism.org/Technical/Emacs/literate-programming-tutorial.html)

[Use Emacs Org mode to easily create LaTeX documents](https://github.com/JacquesCarette/Drasil/wiki/Getting-Started-with-Org-Mode/_edit)

[Images in LaTeX export](https://orgmode.org/manual/Images-in-LaTeX-export.html)

[Footnotes in Org Mode](https://orgmode.org/manual/Creating-Footnotes.html)

[Org Mode With Babel to Include Code Blocks](https://orgmode.org/worg/org-contrib/babel/intro.html)

[Org Mode Cheat Sheet](https://devhints.io/org-mode)

## Projectile example

To learn org mode, and to provide an illustrative example, the [Jupyter Notebook version](https://github.com/smiths/caseStudies/blob/master/CaseStudies/projectile/projectileLesson/ProjectileLesson.ipynb) of the projectile motion lesson has been converted into an [org mode version](https://github.com/smiths/caseStudies/blob/master/CaseStudies/projectile/projectileLesson/orgModeVersion/projMotLesson.org).

## Technical Notes

- I've had good success using [GNU Emacs](https://www.gnu.org/software/emacs/) on my Mac.  The configuration files in this case is: `~/.emacs.d/init.el`.
- Emacs is sensitive to file extensions, so make sure that the name of your document ends in `.org.`  Alternatively, to make Emacs understand that this is an Org document, add the following to the top of your document: `MY PROJECT -*- mode: org -*-`.
- Footnotes are ended by two consecutive empty lines.
- The shell for a GUI application on a Mac in non-interactive mode may have different environment variables from the interactive shell.  You may need to set the environment variables again for the non-interactive shell.

## Thoughts on org mode compared to Jupyter lab
- org mode supports multiple languages easily, while Jupyter requires extensions.
- org mode worked naturally with LaTeX, including equations, cross-references, figure captions and footnotes.  Jupyter requires more work and feels more fragile.
- org mode allows for any LaTeX code to be added verbatim.
- Jupyter can export LaTeX, pdf, html, text, but the quality (in my experience) was better with org mode.
- tangle feels like an afterthought for Jupyter
- org mode allows blocks to be introduced in the order best for a human reader, while Jupyter puts code in the order needed by the machine.
- emacs is harder to work with than Jupyter - emacs is rewarding, but it is also requires a significant commitment to learn

## Thoughts on org mode compared to "traditional" weave and tangle (Knuth)
- tools exist to support multiple languages, as extensions to Knuth's work, but multiple languages feels more natural for emacs
- tools exist to support exporting to html, but don't feel as well supported as emacs org mode
- both Knuth style tools and org mode allow for true literate programming where the code blocks are documented in the order that is best for the human reader, not for the machine.
- code blocks can be refined via a step-wise refinement (code blocks can be set so that they are not evaluated, and not part of the tangle process)
- because org mode is not a LaTeX document, it could be awkward to incorporate the LaTeX code needed for some specialized LaTeX tasks
- Given the large developer community for emacs, org mode, and babel, I cannot see a compelling reason to use one of the less-supported and less versatile tools

## Thoughts on org mode compared to Drasil
- org mode does not seem to scale well to large programs
 - the org mode author needs to impose the structure
 - the same information might be repeated in many places
- Drasil could generate org mode files (and Jupyter notebooks)
- Drasil has an implied structure from the existing knowledge and recipes, not every project needs to invent its own structure
- In literate programs the same equation is written in tex and in code.  In Drasil, the equation is only written once.
- although org mode will allow documentation of requirements, design, and test cases, the focus is usually on documenting the code.  Drasil allows for a natural separation into the various artifacts of interest
- our Drasil generated web-pages do not have equation numbers or links, unlike the org mode generated pages.  I'm not sure if our web-pages do not have numbers because we chose not to have them, or because we couldn't get it to work.
- org mode is just manipulating text - it does not know what the text means, other than simple document information, like section, list, bold, etc.  Drasil, on the other hand, knows what much of the text means.  For instance, Drasil knows the meaning of equations, definitions, symbols, etc.  With the extra information, Drasil is able to generate much more meaningful documents.

## Final thoughts
- Jupyter notebooks and org mode documents seem well suited to presenting the use of high-level functions, functions that are close to the domain knowledge.  In these cases the code and the theory can be interspersed without the details of the code getting in the way.  Example uses include
  - lessons
  - user manuals
- as good as org mode is, it has not caught on in a significant way, likely because emacs is such a big commitment
- as valuable as literature programming is for reproducible research, it hasn't caught on in a significant way, maybe because the tools are too much work?  (repeating knowledge in code and in documentation is a definite drawback)
- literate programming is not well suited to change.  If a design needs significant changes, they can be difficult to make because of the intermixing of documentation and code.  Literate programming is better for a stable design.
- the current projectile example is written in Jupyter and org mode - what if something has to change?  The change will have to be made manually in both documents.  If Drasil generated Jupyter and org mode, the change would have to be made in one spot

Another option for a scientific and technical publishing system is [Quarto](https://quarto.org/).  Quarto seems to have better support for equations, citations, cross-references (etc), than Jupyter.  It doesn't appear to have the flexibility of org mode.  In particular, it may not have a way to extract just the code, as done in literate programming.