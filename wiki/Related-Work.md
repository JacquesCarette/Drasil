There is a lot of work that's related, for various versions of 'related'. Right now, this page will be unclassified, but that also really ought to be done.

Table of Contents:
- [[Draco|Related-Work#draco]]
- [[DMS|Related-Work#dms]]
- [[GLisp|Related-Work#glisp]]
- [[Software Product Lines|Related-Work#software-product-lines]]
- [[Specware|Related-Work#Specware]]
- [[Literate Programming|Related-Work#literate-programming]]
- [[org-mode|Related-Work#org-mode]]
- [[Notebooks|Related-Work#notebooks]]
- [[Concept Centric Development|Related-Work#concept-centric]]

The work above is 'related' in the sense that there is some overlap of goals. Other work is better classified as [inspiring our design](Inspiration).

## Draco

### Description

Jim Neighbors' [Draco](https://www.semanticdesigns.com/Company/Publications/Draco_Approach_to_Software_1983.pdf) is an early 80s project that had very similar goals to Drasil (see also the later [Draco: A Method for Engineering Reusable Software Systems](https://homepages.cwi.nl/~storm/teaching/reader/Neighbors89.pdf). The abstract says "In particular, we are concerned with the reuse of analysis and design information in addition to programming language code", and "The goal of the work on Draco has been to increase the productivity of software specialists in the construction of *similar* systems" (where the emphasis was in the original).

Informative is the whole section: **What does Draco Do?**, reproduced here in its entirety:
1. Draso accepts a definition of a problem domain as a high-level domain-specific language which we call a *domain language*. Both the syntax and semantics of the domain language must be described.
2. Once a domain language has been described, Draco can accept a description of a software system to be constructed as a statement or a program in the domain language.
3. Once a complete domain language has been given then Draco can refine the statement into an executable statement under human guidance.

To detail some of the items above:
- a domain description is composed of a *parser*, a *pretty-printer*, *transformations*, *components* and *procedures*,
- the point of these is to help focus the refinement activity
- a lot of the work is around domain specific source-to-source program transformations and how to specify these

Some of the items form the conclusions are also worth repeating here:
- "We have found that the reuse of analysis and design is much more powerful than the reuse of code. [...] Code is very tricky to reuse. Many of the analysis, design and implementation decisions which went into its construction are absent from the code itself. Our experience of refinement histories more than 10 times the size of the final code speaks for a large amount of information missing in just the code itself."

More information and papers are listed on [Bayfront Technologies' web page about Draco](http://www.bayfronttechnologies.com/l02draco.htm) where Bayfront is Neighbor's consulting company and on his [DBLP page](https://dblp.org/pid/35/6560.html).

### Analysis

This has been quite influential in some circles (cited 688 times, with a related paper cited 234 times).

Pros:
- Domain Analysis
- Good degree of automation

Cons:
- still much too focused on code
- does not include documentation
- adding knowledge (a whole domain) is much too high a bar
- adding a domain requires way too much information/work

Context forces:
- long time ago, lots of generative technologies did not exist
- understanding that this kind of transformation-based system is easiest (by a lot) done in a host language that is functional was not yet pervasive
- system not open source, so could not be leveraged by others (instead, consulting company, which made sense at the time)

Fundamentally Draco **aims too high** by trying to **automate the hard / creative work**. By looking at **all** artifacts and only aiming to automate the **straightforward** parts (using modern technology), Drasil is different.

### Interesting factoids

- Jim Neighbors coined the term **domain analysis**.

## DMS

### Description

Semantic Design's [DMS page](http://www.semdesigns.com/Products/DMS/index.html) provides a good high-level (marketing-oriented) summary. While DMS was [widely presented in 1992](https://dl.acm.org/doi/pdf/10.1145/129852.129859), based on a 1990 PhD of the author, Ira Baxter, it continued to evolve for a number of years (eg [paper at IWPSE 2002](https://dl.acm.org/doi/pdf/10.1145/512035.512047?casa_token=eBlPfmJ6YNsAAAAA:q4Uw_jSAJptobAdYMK7r-ws4dQjcK7D5qGJnx_6A_htXROF9YTbGtj5xx0lgIw9Zk2WeMfuoBGDsuQ)) but has not been talked about much since.

The general vision is centered around the concept of **design maintenance**. In its own words *for large software systems, the design information that rationalizes its implementation given its specification is the fundamental artifact to capture and modify*.

DMS fundamentally assumes a fairly high level of formality, i..e that a specification exists, that there might be multiple implementation in multiple languages, that the implementation(s) are derived from the specification *in a justified manner*. DMS follows the Draco model.

### Analysis

While the DMS description assumes high levels of formality, it seems that the infrastructure has been used for a variety of tasks (see the IWPSE paper) that are not so well specified. In some sense, it appears that the author's expertise in program transformation and program analysis is what is being leveraged, rather than DMS *the system*.

DMS does have *domains*, but all its implemented domains appear to be programming languages. 

DMS, as originally envisioned, aims for even more automation, and more formality, than Draco. It also focuses very hard on code, even though it 'encodes' it differently, because it tries to deal with 'design maintenance' at its core. So it is very much a transformational system. Details are a little hard to see, as it is closed source.

## GLisp

### Description

[GLISP: A Lisp-based Programming System with Data Abstraction](https://www.cs.utexas.edu/users/novak/aimag83.html) presents a language "above" Lisp, that compiles to Lisp, that is also related. One of the new ideas are *object descriptions*, which is its (declarative) *knowledge base*. Generic programs make use of these description to add procedural knowledge. Various kinds of knowledge can be added to objects, such as what are termed *properties* (basically: static information that can be computed at compile time), *adjective* (predicates) and messages that can be sent to the object (a crude form of program).  The compilation process of GLisp is fairly straightforward (from today's perspective). Its use of generic functions is quite nice, as is the crude partial evaluator.

GLisp goes further and provides a graphical development environment as well.

Part of the modelling goes through hierarchical inheritance (see section 6 of [Data Abstraction in GLisp](https://dl.acm.org/doi/pdf/10.1145/872728.806863) ). The modelling is very ad hoc, in the same way that early OO modeling was as well.

Most notable as its worked examples in physics, which are well shown in [Generating Programs from Connections of Physical Models](https://www.cs.utexas.edu/~novak/caia94.pdf). Amusingly, the first example is a simple variant of Projectile. The other examples are nice too, and could quite likely be borrowed for Drasil.

### Analysis

Interestingly, the later work admits that CAS like Mathematica exist and are alike, but dismiss it for inadequate programming support. Its references are quite full of work that try to do model-based development (decades before the name) in the physics domain - definitely a good mine here.

Compared to Draco and DMS, GLisp is much less systematic, as well as less ambitious. In the physics domain though, it appears to thus achieve more. Like all the rest, the focus is on code.

## Software Product Lines

This isn't an overview of all the SPL work, but centered on Don Batory's work. Some unrelated but excellent reads where he is (co-)author:
- [A Theory of Modularity for Automated Software Development (Keynote)](https://www.cs.utexas.edu/ftp/predator/15Modularity.pdf)
- [Structured Document Algebra in Action](https://www.cs.utexas.edu/ftp/predator/15SDA.pdf) (SPA)
- [Interfaces are Key](https://www.cs.utexas.edu/ftp/predator/13SEHPCCSE.pdf)
- [Scalable Software Libraries](https://dl.acm.org/doi/pdf/10.1145/167049.167078) from 1993. 

### Description

A good introduction is the recent (2013) textbook [Feature-oriented software product lines](https://link.springer.com/content/pdf/10.1007/978-3-642-37521-7.pdf). Its Chapter 2 is particularly worth reading.

SPLs do leverage a lot of the previous work. But again, focus too much on code.

### Analysis

Fundamentally it seems that, no matter the definition used (though some would quibble), a *feature* as a *unit of knowledge* is much too large. Feature diagrams provide a *particular view* of a system's requirements. Its "intersection" with domain analysis is quite unclear. Like code, it is fundamentally biased on "doing stuff" rather than on "what stuff means".

Of the other ideas:
- SPA is 'too crude', even if it's a step in a solid direction, tackling a real problem. The approach is still very operational instead of being more denotational.
- 'Interfaces are Key' are more about lessons learned while doing dense linear algebra software (including the title lesson). Just good stuff throughout.
- 'Scalable software libraries' is great for 1993, but other than the goal, the rest is essentially obsolete.

Basically, some of the work is flawed (!!!) because it tries to be useful on a short-term basis, and thus is mired in (at the time of writing the papers) tools, development practices and programming languages. Commendable indeed, but we're trying to see what we can do if we try for long-term.

## Specware

What is really included here is both [SpecWare](https://www.kestrel.edu/research/specware/) and its extension [DesignWare](https://www.kestrel.edu/people/smith/pub/ctcs.pdf)

### Description

From its own web page (i.e. everything below is a direct quote):
Specware is
* a **design tool**, because it can represent and manipulate designs for complex systems
* a **(higher order) logic**, because it can describe concepts in a formal language with rules of deduction
* a **specification language**, for expressing functional programs as well as safety and security constraints
* a **transformation system and library**, for deriving high assurance software
* a **database**, because it can store and manipulate collections of concepts, facts, and relationships

Specware can
* be used to develop **domain theories**
* be used to develop **code from specifications, with machine-checkable proofs of correctness**
* be used to develop **specifications from code**

### Analysis

SpecWare contains a huge wealth of ideas that we'd like to borrow.

Pros:
- takes specification seriously
- based on successive refinement
- backed by real theory
- has been used to do 'real things'
- can generate code in multiple languages
- has proofs of correctness
- is **self-specified** !!

Cons:
- still doesn't take documentation seriously enough
- puts its theory "up front", which is a real drawback
- doesn't generate all the artifacts

Context forces:
- was proprietary for a long time
- only works on mac/linux. Tied in to Emacs.

## Literate Programming

### Description

The ideas are well described on the [Literate Programming](https://en.wikipedia.org/wiki/Literate_programming) Wikipedia page. There are worthwhile later adaptions such as [Entangled](https://ieeexplore.ieee.org/abstract/document/10254816?casa_token=UnJ02KX9RPMAAAAA:HAG2tZy-VwX8Nf9oNabFn3gj0-PQ2mqo_qHHrfItd8HNsa0yPIkLMzjEjGFhte1L-EfFo4x7) that should be delved into as well.

### Analysis

The strengths are:
- takes documentation seriously
- does not 'obey' the underlying PL's idea of order to explain things
- produces pretty results
- does allow re-use of some chunks

The weaknesses are:
- in a way, still focuses on code! While small-scale design can be explained this way, things like requirements and tests are not as first-class
- documents an implementation, hard to do families and vary the language
- does not facilitate re-use of ideas

## org-mode

### Description

As wikipedia says, [org-mode](https://en.wikipedia.org/wiki/Org-mode) is "a document editing, formatting, and organizing mode, designed for [notes](https://en.wikipedia.org/wiki/Note-taking), [planning](https://en.wikipedia.org/wiki/Project_planning), and [authoring](https://en.wikipedia.org/wiki/Markup_language) within the [free software](https://en.wikipedia.org/wiki/Free_software) [text editor](https://en.wikipedia.org/wiki/Text_editor) [Emacs](https://en.wikipedia.org/wiki/Emacs)", but also "Almost orthogonally, Org Mode has functionalities aimed at executing code in various external languages; these functionalities form org-babel."  For Drasil, it is really org-babel which is more interesting. It allows *literate programming* as well as *reproducible research*.

### Analysis

org-mode has all the pros of literate programming (via [Babel](https://orgmode.org/worg/org-contrib/babel/index.html), nicely described in [this paper](https://www.jstatsoft.org/index.php/jss/article/view/v046i03/557) ) and removes some of the cons. More specifically

Pros:
- all of those of Literate Programming
- allows mixing of multiple language and ideas (so includes testing, for example)
- does not focus on code
- includes various things around process (to-do lists), linking, and personal wiki.

Cons:
- tied in to emacs
- does not facilitate re-use of ideas

## Notebooks

### Description

[Notebook interfaces](https://en.wikipedia.org/wiki/Notebook_interface) are quite old (late 1980s) but didn't really make much of an impression outside of the realm of mathematics until Jupyter came along. The basic idea is to have a GUI approach to an interactive document that can mix many things together. 

### Analysis

A naive look might it seem like it's a mixture of literate programming and org-mode. Unfortunately, the concept of 'chunk' doesn't really exist, so the explanations much still revolve around the computational kernel's idea of how to structure code. 

The obvious strength there is being able to bundle up everything in one place, which is why many champions of reproducible science like them so much. Unfortunately notebooks don't scale very well, nor do they enable easy reuse. There are other issues as well which aren't inherent to notebooks per se, but to the fact that most kernels are stateful.

## Concept centric

This is laid out in the paper [Concept-centric Software Development](https://arxiv.org/abs/2304.14975). While it has much going to it, it is too software-focused. We prefer things more along the lines of [Domain Driven Design](https://en.wikipedia.org/wiki/Domain-driven_design), though that too is flawed. [Domain Specific Modelling](https://en.wikipedia.org/wiki/Domain-specific_modeling) is likely closer to the mark.