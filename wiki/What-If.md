# The "story of Drasil"

We will tell the "story of Drasil" as a conversation between someone who
understands what it is (**S**), and someone new to it (**N**).


**S**: Begin with this simple question:

    What if everything that is currently human-written in current software
    development was instead generated?

**N**: What do you mean exactly by *everything*?

**S**: Every single artifact that one finds
inside a repository dedicated to a single software project. Both
the practical ones currently present in the majority of projects, and
also the idealized ones that our textbooks say should be there but all too
often are not. A non-exhaustive list:

- requirements documentation
- design documentation
- theory manual
- user manual
- CI infrastructure code
- tests
- READMEs
- build infrastructure
- (documented) code

As a means to have a collective word for all of the above, we will
label all of these **softifacts**.

**N**: And what do you mean by *generating*? Like templates?

**S** Not like templates. Like a program that uses a more semantic source for
all the softifacts, selects the appropriate information for each piece, and
writes each out in its *native format*. The basic idea is that each
softifact contains a quite particular *view* of the total information behind
a single software product, and so the generation process is one of
filtering and translation.

**N** Like Modelling then?

**S** Much closer to that. Typical MDE approaches are very concerned with a
particular slice out of the softifacts, and assume the rest will still be
hand-written.

**N** This brings up so many more questions! I'm not even sure what order
to ask them all in. Maybe I can bombard you with my questions?

**S** Sure, go ahead. I'll then try to pick some kind of order that might
make sense.

**N** Ok, here we go:
1. Does this even make sense?
2. Is this feasible?
3. What would development then look like?
4. This sounds radical -- is it?
5. You almost make it sound like a silver bullet! Is it?
6. What do you mean by "more semantic source"?

**S** All excellent questions. Let me address the simplest one first
*this sounds radical -- is it?*

If you mean, is the development process under such an assumption very
different than current development processes? Then the answer is a 
resounding yes. More details on that later.

If you mean, is this a radically new idea, then the answer is 'no'.
It is instead the mashing together of a large number of pre-existing ideas
coupled with now much more mature technologies than when these ideas were
first brought forth.

We borrow ideas from literate programming, org-mode, Draco, DMS, GLisp,
Software Product Lines, Specware, Jupyter-like notebooks, concept-centric
development, Kolmogorov Complexity, biform theories, universal algebra,
concept lattices, ontology, open science, sustainable science
and model-driven engineering. We've also been inspired by some work from
the Viewpoints Institute and the Kestrel Institute. And, of course,
heavily leverage advances in programming languages, both on the theory and
on the practical side.

It is still a *new* idea, in the sense that the particular assembly of
borrowed ideas **is** new. And we also had to improve on the ideas that we
borrowed.

**N** Wow, that's a lot!

**S** Yes, put like that, it does seem like that, doesn't it? 

However, it really isn't that complicated. This is because the ideas we
borrow 'fit' quite well with each other. In fact, sometimes it turns out
to be *the exact same idea* in multiple guises! But we have to 'borrow' it
multiple times to see that for ourselves.

**N** Can you give me an example of a borrowed idea?

**S** Of course!  Let's pick *literate programming* as an example.

Buried inside the technical details of literate programming are several
key ideas:
1. That explaining a piece of code to a human forces you to think more
deeply about your code, resulting in better code.
2. That explaining code to a human does not naturally follow the same
order of the eventual program. In other words, the "explainable chunks" of
a piece of code may not correspond at all to what the programming language
would consider a program.
3. That co-locating code and explanations helps hugely in keeping these 
synchronized.

===
Below here are notes for an older version.

## Does this even make sense?

(lots of duplication)

## Is it even feasible?

(yes, we have a prototype)

## How does it work?

(a basic idea of the workflow)

- theories, basic ontology
- meta-language of OO languages

## Is this a new idea?

(not really - but previous work didn't really succeed)

## So you have a silver bullet then?

(Heck no! This has some pre-conditions for it to work.)

## Well-understood?

(i.e. where theories of domain, typical design decisions and even
architecture are all well documented in textbooks, and have been
time tested for a long time.)

## And then it's worth it?

Nope. Long-lived is another component.

===

Notes for stuff to remember:
- Assumptions
  1. code generation
  2. organizing things in (flexiformal, triform) theories
  3. product families
  4. DSLs
- current "linguistics" of writing software sub-optimal
- not aiming for a silver bullet
- context for success for this (well-understood, amortization)
- meta-theory (theory of theories) needed for lib. dev
- still need a viable dev processto make this work
