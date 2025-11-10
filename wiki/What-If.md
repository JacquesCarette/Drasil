# The "story of Drasil"

We will tell the "story of Drasil" as a conversation between someone who
understands what it is (**S**, who can be thought of as Socrates), and someone
new to it (**N**). Headers are included to help with navigation but are
external to the story itself.

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

**N**: And what do you mean by *generating*? Like templates?

**S** Sort of. Many templates are simplistic fill-in-the-hole, and
that's not what is meant here. What we mean is turning a semantic
description of all of the artifacts, and then *rendering it* it in
one (or more) programming languages. We see each artifact that is generated
as a particular *view* onto the semantic information behind a single
software product, so the generation process is more one of filtering and
translation.

In other words, we want to *compute* the results. 

**N** Like with Generative AI, i.e. LLMs (Large Language Models)?

**S** Quite a bit closer, actually. Now LLMs compute a 'plausible'
reply to a prompt, based on a huge amount of data. We don't do that at
all: no training, no data needed.

You could say we're closer to genetic programming, in the sense that there
you specify constraints for a solution and a 'solution space vocabulary', 
and generate tentative solutions programmatically. We too have those
constraints for a solution (the specification). But we ask the user to
*design* the solution as well -- at a very high level. We ask for just
enough information so that producing the artifacts given that information
is purely mechanical.  This is more like a correct-by-construction
methodology.

Note that LLMs require you to give quite a lot of information in the
prompt to have a hope of being (partly) successful. We're asking for
similar information, but in Drasil, there is no room for fuzziness.

**N** Like Model-Driven Engineering then?

**S** Much closer to that. Typical Model-Driven Engineering (MDE) approaches
are very concerned with a particular slice out of the artifacts, and assume the
rest will still be hand-written.

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
4. There is power in using the 'right language' for any given job at hand.

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
so there is naturally a lot of repetition in between artifacts. For
example, whenever we do solid body physics, things like force and 
acceleration and velocity will appear everywhere.

**S** Right. Much more impressive is the kind of compression you can
achieve on 'knowledge' that can take on different forms across
artifacts (such expressions in vector notation in the theory and loops
for computing those same expressions in code).

Here the best example is from the (now defunct)
[Viewpoints Research Institute](http://www.vpri.org/)'s
[STEPS project](https://tinlizzie.org/VPRIPapers/rn2006002_nsfprop.pdf)
which was aiming for a 1000x to 10,000x compression. (The
[final report](https://tinlizzie.org/VPRIPapers/tr2012001_steps.pdf) gives
ideas of how well they did.) Roughly speaking: the "information content"
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
So yes, it is feasible.

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
**Idealized Development Process** that we envision.
By 'development process' we mean something akin to other models like
Waterfall, Spiral, Agile, V and so forth. There are different activities
done in different phases, each of which produces artifacts. And, in
general, there are different people who will perform these activities.
Because Waterfall and Spiral are also idealized development processes,
we're going to dub ours the **Idealized 
Theory-based Development** process or ITD for short.

So what would development of an application using the ITD process be, assuming
some future full-featured version of Drasil, and a large library of
captured knowledge?

A developer would:
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
that all the steps are things that the system has knowledge of?
Even the documentation?

**S** Yes, that's right.  Every step involves either 
writing down specific 'knowledge' or gathering specific 'knowledge' from
an existing library. It can be explanations meant for humans or lead
to code or both.

**N** What if the library doesn't contain the theories needed?

**S** This is the thorny part of the 'getting started' process. 
Domain experts will need to work with Drasil experts to write those domain
theories, creating a vocabulary that captures the core knowledge of
critical domains.

**N** So there are people other than developers involved?

**S** In the most general case, yes. And right now, yes. There is a need
for infrastructure builders, meta-theory builders and theory builders,
who need to continue to bring up the system to where most of the work
would be done by developers.

## Linguistics

**N** Coming back to the knowledge capture part, are you implying that there
are a whole bunch of different 'languages' involved, for each of the different
kinds of activities?

**S** Yes, that's exactly right! This is one of the important ideas that
emerges from much of the work that has influenced us. There are many
'domains of knowledge' and each one of them ends up defining a vocabulary
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

## MDE and DDD

**N** Is this where this intersects with MDE?

**S** In part, yes. Definitely the knowledge capture of application domain
reflects the better parts of MDE, when they focus on first specifying the
problem space (some older MDE work is very solution-space focused, which is
the exact opposite of what we want.) Parts of MDE also ties itself in
knots over modelling, meta-modelling, meta-meta-modelling and so forth,
and this is not something that seems so problematic in our situation.
Not that we're immune to such issues, more that there has yet been any
reason to go 'that' meta.

**N** So this is more like Domain-Driven Design?

**S** We do intersect some, but DDD veers off into weird specifics very
early on (it seems to be oriented towards particular kinds of software
and assumes OO quite a lot). I'd say we're closer to the original ideas
of Draco. But that's where terminology like 'domain knowledge' came from!

But we do agree with DDD on the fundamental importance of domains.

## Activities

**N** I'm still having a hard time seeing what a developer would actually
do when using the ITD process.

**S** One point of difference with classical development is that the act of
capturing domain knowledge is incorporated as an explicit activity in the
process. This is akin to considering the addition of new features to an open
source library by the development team of a closed-source piece of software
(that uses that library) as an integral part of the development. And in
the same way, at some point the library is good enough and not so much work
is done on it.

We can divide the ITD process into groups of activities:
1. Infrastructure development
2. Theory development
3. Domain-specific theory development
4. Requirements capture and encoding
5. Theory instantiation and design weaving
6. Artifact generation and audit
7. Using the produced software

As Infrastructure development is akin to 'compiler writing', which is
not normally seen as integral, we'll skip that.

Usual development skips activities 2 and 3 and leave them as tacit
knowledge in developer's heads. Activities 4 and 5 are done, of course,
but usually somewhat informally, as the artifacts produced by those
activities are *not reusable*. But these are the activities that generate
the most value! Traditionally a huge amount of effort is spend on
activity 6 (where we lump in many activities that are often separated
out into individual items) where here we see this as mostly automated.

It is important to note that many different people are involved in
these activities. It is not expected that a single person would ever span
them all. A more nuanced description of the activities would split things
up also by that role.

**N** Yes, I see how that's quite different. Though I must admit that
this remains somewhat vague. I don't quite see what you would *do*.


## How does it work, with an example

**S** You're right to point out that I've been explaining things at much
too high a level, and I should try to be much more concrete. I'm going to
use a simple (aka toy) example to illustrate things.

Suppose you need to write software to compute where a projectile is
going to land, based on a fixed launcher. Let's call this 'Projectile'.

This problem is one where the fundamental rules come from Physics. So you
grab the parts of physics that are in the library, and look for things
that have to do with rigid bodies, kinematics, movement, forces, and collisions.
An informal description of "compute the landing distance of a projectile
launched at a particular angle with a certain initial speed given
flat ground. Assume no friction and the only force is gravity."

All of this can be translated into semi-formal statements, which will all
correspond to things that already exist in the library. The only
domain-specific theory development you might need to do is if your
projectile and launcher have specific properties that are not generic.
Maybe it's important that your projectile is purple and that your point
of launch is not at ground level, for example. You end up capturing such
that information in a "twinned" manner, that captures both natural language
descriptions and the mathematical equivalent. This is where things cannot
become de-synchronized: the knowledge capture tools fundamentally don't
allow for that. Some information comes in different bundles (pictures,
external libraries, etc). We emphasize the natural language + mathematical
because they are the richest ones that we can best leverage.

Then you decide how the software to do this should be structured. As there
are a number of things that can vary (for example, launch angle), you have
to decide whether these are read from a file or will be parameters. Are you
building a library of functions for this purpose, or an application? These
are architectural choices that you need to make. In this case, there is
also the choice of using a closed-form or computing the answer using
numerical methods, and the latter would involve algorithmic choices.

Then you need to decide further things, like which programming language
will the code be produce in? Will the code produced by highly documented
or quite raw? Overall, how much user documentation will there be?

Then, just like with literate programming, you write a description of
each of the artifacts that you want. The same way that you 'weave' and
'tangle' in literate programming, here is there is a different language
for every artifact. You need to explicitly say how to slot in the information
that you have (extracted from the 'web of theories') into the artifacts.
Part of this process, just like it does for literate programming, involves
choosing an *order* for things. Solving problems like projectile motion
involves a sequence of computation steps, whether in the mathematics or
in the code that implements the mathematics.

**N** I think I see. I guess I'd want to see some of these things that
you write to make it resonate even more.

**S** Absolutely. But perhaps that can wait for another day?

**N** Sure. This sounds quite revolutionary to me. Why did you say it's
not?

## Silver-bullet?

**S** Ah yes, the hunt for the 'silver bullet', which doesn't exist.

If you look at the developer steps and the activities, a lot of them
involve 'knowledge capture' in the form of 'theories'. Underlying this is
the implicit assumption that this knowledge exists and is already
essential formal. Otherwise the effort to do this kind of work is
enormous, and might far outweigh the effort of just writing all the
artifacts in the traditional way.

So we need to analyze the overall effort required under both the more
traditional ways of doing things, and under the ITD. The ITD spends
a lot of time and effort on knowledge capture, and that has to somehow
be amortized over the project's lifetime. If a project's lifetime is not
known to be 'long enough', then the short amortization period will not
make such an investment worthwhile. It is crucially important that the
investment in knowledge capture be seen as a long-term investment.

An analogy: this is why people invest in "standard libraries" for the
ecosystems that corresponds to various programming languages. The richer
the library, the faster a programmer can write code that 'does stuff'.
But here the "standard library" is one that isn't code, but theories.
We can also see this in action for systems that are already theory-based,
like the mathlib for Lean 4 or the AFP for Isabelle/HOL.

As amortization of costs is very important, let me be quite specific
about how we see this playing out:

1. start-up costs for wholly new projects (i.e. those whose knowledge
has yet to be captured) will be *very* high.
2. Drasil-based projects can handle change much better. Not only is it
easy to understand what needs to change, the repercussions can be
computed.
3. Because of that, ongoing maintenance costs should be considerably
lower than traditional development methods.

So when is this all worth it? We think the conditions are:

1. The underlying theory of an application is **well-understood**,
2. The theory is not changing much and is likely widely applicable,
3. The project is planned, from the start, to have a **long lifetime**,
4. The project will require non-trivial maintainance over its lifetime,

then investing in the ITD may well be worth it.

Note that it is not necessary for there to be a single underlying theory
that applies. There is the concept of 'validity frame' which allows a
variety of applicable theories to co-exist (like Newtonian and Relativistic
Physics). So a change-of-theory is fine, as well as it was known from the
start that this was a possibility.

## Well-understood?

**N** What do you mean by 'well understood'?

**S** We mean that there are standard textbooks that explain the
applicable theory (like the physics of projectiles). But also that
the typical design decisions and software architecture(s) for writing
software in that domain are also well documented, preferably in textbooks,
and have been under use for a long time.

In other words, the science and engineering of both the problem space
and the solution space is amply documented, i.e. well understood.

## Long lifetime?

**N** And what about 'long lifetime'?

**S** This is basically a proxy for a project thas a number of characteristics:
- where it makes sense to think hard up-front about what is the underlying
  theory that drives the software,
- where documentation is seen as crucial as a way to instill confidence
  that the software does the right thing, and solves the right problem,
- where it is known that maintenance is likely to involvement making
  various non-trivial changes to the design as the requirements come in,
- where the project will live long enough that there will be staff turnover,
  and so documentation capturing all the decisions along the way will be
  needed.

**N** Ah, I see. Yes, indeed, quite a lot of projects would not meet
these criteria!  In fact, would there be any?

## Domains of Interest

**S** Yes, plenty. For example, take the software inside a space probe
like the Pioneer and Voyager spacecrafts. But also inside things like
nuclear power stations or many medical devices.

There is also quite a lot of 'research software' where the surface
requirements change all the time, but the deep theory is essentially fixed.
The ITD process really lets you do "what if" experiments in those settings
quite easily.

But of course, pretty much all of the software where Agile would be
appropriate are domains where ITD would not be.

**N** Oh yes, that makes a lot of sense.

## Why now?

Hmm, you mentioned a number of projects that were related (Draco, DMS, GLisp,
SpecWare). But I didn't really
hear about any of them. Did they fail? And what's different now?

**S** Good point.

It's not so much that they failed, but that the tools part of the projects
did not survive. Many of the projects had a substantial intellectural
impact on many subsequent projects.

The main reason for the tools to fall to the wayside is that the maintenance
burden was too high compared to the amount of take-up by outsiders in a
reasonable time frame. And a lot of that had three underlying reasons:
1\) poor technology, 2) overly ambitious aims, and 3) lack of theoretical
foundations. Any one of them is not a fatal flaw, but the combination of all
of them was too much. In some sense, a fourth reason is that some of them
were just too far ahead of the state-of-the-art, and so used 'clunky'
means to get the job done.

**N** So you think that's changed?

**S** Yes, absolutely. There have been tremendous advances in
meta-programming, statically typed programming, tooling for DSLs and
generally a much better understanding of how to create languages.
Our project tries to mitigate the 'overly ambitious' by trying to do
the minimal infrastructure necessary to get things to work, and then
refactor when necessary. We're also very example-driven when it comes to
assessing the need for infrastructure. In other words, our own internal
development is really not following our own process -- basically because
we're continually inventing, the very opposite of 'well understood'.
Lastly, the theory underlying languages (programming and specification alike,
but also natural languages as well) has also improved dramatically.

Basically enough has changed between then and now that it feels worthwhile
learning the lessons of the older projects (i.e. both their good ideas and
their mistakes) to try again. Of course we're also in a good position to
learn from many projects.

**N** Well, I think that covers it.

**S** You forget one of your questions about "semantic sources".

**N** Oh yeah!

## Semantic sources

**S** You might have noticed that I called it 'Idealized Process for
Theory-based Development' and talked about theories a lot, but never quite
explained that.

**N** Now that you mention it...

**S** I was equally vague with 'knowledge', wasn't I? The point is that
whatever information we have, it needs to be organized. So we use two
different mechanisms: theories and ontologies.

Theories in the mathematical sense (defining types, function and
relation symbols, obeying some axioms) but also in the more general
scientific sense (such as Newton's First Law, and the vocabulary of
modeling). Because we're doing various scientific theories, various items
such as units of measure are important to us. But also what emerges are
"systems of quantities" and other meta-theories useful for arranging
theories of the real world.

Furthermore we also mix in many concepts that occur in ontologies. We're
forced to invent a lot of stuff here, because these various ideas have
never been put together before.

## Conclusion

**N** And now that's it?

**S** Let's say yes for now. Really we should more systematically go through
the 'lessons learned' from all the other things that I mentioned, as well as
do a synthesis of these into a more general set of "good ideas worth
pursuing" and "pitfalls to avoid".

And I should really show you a concrete example in full.
