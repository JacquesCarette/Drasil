# The "story of Drasil"

We will tell the "story of Drasil" as a conversation between someone who
understands what it is (**S**), and someone new to it (**N**). Headers are
included to help with navigation but are external to the story itself.


## The question

**S**: Begin with this simple question:

    What if everything that is currently human-written in current software
    development was instead generated?

## Explaining some terms

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

## More questions

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

## Is this new?

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

## Literate programming

**N** Can you give me an example of a borrowed idea?

**S** Of course!  Let's pick *literate programming* as an example.

Buried inside the technical details of literate programming are several
key ideas:
1. That explaining a piece of code to a human forces you to think more
deeply about your code, resulting in better code.
2. That explaining code to a human does not naturally follow the same
order of the eventual program. In other words, the "explainable chunks" of
a piece of code may not correspond to what the programming language
would consider a program.
3. That co-locating code and explanations helps hugely in keeping these 
synchronized.
4. The language of explanations for a lot of code mixes natural language
and mathematics -- and ASCII is a horrible way to do that. Explanations
ought to be pretty.

**N** Those are certainly very interesting ideas indeed. But I'm not
quite sure how they connect to "generate everything"?

**S** This is because the connection is not immediately visible in the
key ideas, but emerges more easily from the processes of literate
programming. In particular, the processes of *weaving* and *tangling*.
Weaving is the where a human-readable document is obtained from the literate
code, while tangling produces computer code as expected by the underlying
programming language. This works on the same 'source' thus maintaining
consistency *by construction*.

The resulting artifacts, i.e. the document and the code, are generated
from the one source. The 'code' that is produced is reconceptualized as
a *web of concepts* that needs to be tangled for the code to emerge.

**N** Ah, I see now where the 'generation' comes from. But literate
programming only supports two kinds of artifacts, right?

**S** That's right, and that's why these ideas need to be generalized.

**N** So are those ideas above the ones you use?

**S** No. Those ideas are what inspired us. We can abstract further and
get the following ideas:

1. There is immense value in human-level explanations of artifacts.
2. *Reusable knowledge* doesn't easily fit the confines of a 
programming language.
3. That synchronization between different guises of the same fundamental
concept (i.e. a human-level explanation and a rendering of the executable
part as code) is a huge problem; if these are always seen as an integral
whole, then the synchronization problem disappears.
4. There is power in usual the 'right language' for any given job at hand.

It's important to note that these ideas can't be seen as direct descendants
of literate programming: they weave in ideas from other sources too!

**N** That's getting quite abstract, and I'm kind of losing the concrete
consequences of these ideas.

**S** Yes, sorry, I got a little over-enthusiastic there, and jumped to some
conclusions that I'm not ready to justify!

**N** Ok, let me store that away for now, for future pondering.

Maybe you could try tackling a different question from my list?

## Does this even make sense?

**S** Sure! Let's try "Does this even make sense?". Let's start with a
thought experiment: If you took a (large) repository full of code and documents,
zipped and *compressed* it all, what would happen?

**N** Well, it would get smaller.

**S** Yes, but by how much?

**N** Hmm, I don't know!

**S** Ok, here's one data point: the 
[Hutter Prize](https://en.wikipedia.org/wiki/Hutter_Prize)
involves compressing 1Gb of a particular snapshot of Wikipedia, as an
experiment in approximating [Kolmogorov Complexity](https://en.wikipedia.org/wiki/Kolmogorov_complexity).

The 1Gb of data can currently be compressed to 112,578,322 bytes. But in some
sense, that's not so impressive, as that is compression of
*structured data*. 

**N** Nice.  But how does that lead to an answer to my question?

**S** Let's go back to our repository.  Why would it compress? Beyond the
trivial fact that ASCII is information-sparse that is?

**N** Oh, I actually know this one! This is because all the artifacts in
question are actually on a single topic (what the application is about) and
so there is naturally a lot of repetition in between artifacts.

**S** Right. Much more impressive is the kind of compression you can
achieve on 'knowledge'.

Here the best example is from the (now defunct)
[Viewpoints Research Institute](http://www.vpri.org/)'s
[STEPS project](https://tinlizzie.org/VPRIPapers/rn2006002_nsfprop.pdf)
which was aiming for a 1000x to 10,000x compression. (The
[final report](https://tinlizzie.org/VPRIPapers/tr2012001_steps.pdf) gives
ideas of ho well they did.) Roughly speaking: the "information content"
of a full-stack system is quite small indeed.

The lesson is that there are cases when the 'raw knowledge' contained
in a particular application is quite small, *when viewed properly*. One of
the key ideas in Kay et al.'s approach is **linguistic**: create the 'right'
DSL to express the ideas of each domain, and then find a way to weave these
together to the meaning of the whole. While parts of the work of Viewpoints
prefers 'dynamic' solutions (Kay is co-inventor of Smalltalk, after all), in
many areas, there is also code generation already present.

And that's where part of the answer lies: there are places where we know
this has been done for some of the artifacts.

In other words, maybe we should consider artifacts in a repository
as a particular *view* on some pre-existing knowledge, and then the creation
of all software artifacts would consist of instructions on how to
weave and tangle that into the appropriate format.

**N** Oh, I think I'm beginning to see how this might fit together.
So you're saying there is so much duplication that this would make writing
software much more compact if this underlying knowledge was captured?

**S** Yes, that's right. But of course, it's not quite that simple, is it?

**N** Hmm, I guess that a lot of people complain about things like
'requirement churn' and so on, and that won't magically disappear, will it?

**S** Indeed. Key here is an observation that I first saw Dines Bjorner
make: that for some applications, there is a (very large!) base of knowledge
that does not change, and a small layer 'on top' of desired functionality
that changes extremely rapidly.

**N** Oh, that makes sense. So I guess it's important to be in a situation
where that's the case?

**S** Probably. It's unclear what the respective costs of each activity is,
so it's unclear where the compromises lie.

**N** But there seem to be compromises?

**S** Yes indeed. But maybe that's diving too deep for now, and instead 
we should take a look at some of your other questions now?

## Is it feasible?

**N** That's probably a good idea. This story you're telling me, it seems
really nice, but could it even work?

**S** Yes, absolutely. We've had glimpses that it could for a long time
(literate programming, org-mode, Draco, MDE and more). But the 'everything'
part, especially when it comes to *documentation*, had never been done
before. We have a prototype (Drasil) that works on smaller examples.

**N** Why didn't you say so earlier? Everyone should be doing this!

**S** The word 'prototype' is an important one here. Because it is a 
'first', we already know that there are many things that are not right
with the current code. And lots of missing features. It's not ready for wide
use.

**N** Oh, sorry. But when it's ready, this is going to revolutionize
how software is done!

**S** Again, no. But the explanation of why this is so might not quite
make sense yet. I think that first, I should explain "How it works".

**N** Oh yes please! I'm surprised I didn't notice that this has not even
been covered yet. I guess I was quite wrapped up in this fascinating
exploration.

## How does it work?

**S** Thank you.  So I'm going to describe what we call the
**Idealized Development Process** (or IDP for short) that we envision.
By 'development process' we mean something akin to other models like
Waterfall, Spiral, Agile, V and so forth. There are different activities
done in different phases, each of which produces artifacts. And, in
general, there are different people who will perform these activities.

So what would development of an application using the IDP be, assuming
some future full-featured version of Drasil, and a large library of
captured knowledge?

A developper would:
1. Figure out the 'context' in which their application exists,
2. Gather from the Drasil library all the domain theories that fit
that context,
3. Refine and describe (in Drasil) the requirements for the application,
including thinking about potential future changes,
4. Instantiate the existing domain theories to their context of use,
most significantly to the *domain assumptions* that they have,
5. Make application architecture choices,
6. Instantiate the right generators for these choices,
7. Make further application-level design choices,
8. Generate the full artifacts (more choices available here),
9. Compile the resulting software; run the generated tests,
10. Audit that what was generated corresponds to what was needed.
11. Go back to any step where the choices where unsatisfactory, fix and
repeat.

**N** That resembles Waterfall, but with more theory?

**S** Granted, written that way, it does look like that.

What's different is that all of the work in all the steps is never lost
or disconnected. Every part of the process involves writing some
'instructions' down that are used by latter parts of the process.

And that this can be done incrementally and repeatedly.

**N** So the reason that I'm not quite understanding the difference is
that all the steps are *formal*?

**S** Yes, that's right.  Every step involves either 
writing down specific 'knowledge' or gathering specific 'knowledge' for
an existing library. 

## Linguistics

**N** Are you saying that there are a whole bunch of different 'languages'
involved, for each of the different kinds of activities?

**S** Yes, that's exactly right! This is one of the important ideas that
emerges from much of the work that has influenced us. There are many
'domain of knowledge' and each one of them ends up defining a vocabulary
(and grammar and formation rules and ...).

Some domains of knowledge are obvious, such as solid-body physics for
simulation a whole host of 'motion', or the mathematical domain of
differential equations, as model for many situations of interest. Others
are less so: software construction itself is a 'domain of knowledge'.
Capturing that allows a system to execute constructions and reason about 
them too. So if you manage to capture various parts of science and
engineering, including capturing the processes around building software
for science and engineering applications, this can itself become part of
your linguistic infrastructure.

**N** Is this where this intersects with MDE?

**S** In part, yes. Definitely the knowledge capture of application domain
reflects the better parts of MDE, when they focus on first specifying the
problem space (some older MDE work is very solution-space focused, which is
the exact opposite of what we want.) Parts of MDE also ties itself in
knots over modelling, meta-modelling, meta-meta-modelling and so forth.

================

What's different here is that act of capturing domain knowledge is
incorporated as an explicit activity in the process. This is akin to
considering the addition of new features to an open source library by the
development team of a closed-source piece of software (that uses that library)
as an integral part of the development. 

We can divide the IDP into groups of activities:
1. Infrastructure development
2. Theory development
3. Domain-specific theory development
4. Requirements capture and encoding
5. Theory instantiation and design weaving
6. Artifact generation
7. Using the produced software

As Infrastructure development is akin to 'compiler writing', which is
not normally seen as integral, we'll skip that.






## How does it work?

(a basic idea of the workflow)

- theories, basic ontology
- meta-language of OO languages

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
