# Idealized Process

See at the end for older notes on this topic.

## Thoughts on the Idealized Process

So I (Jacques) have been reading (on DDD and 'meaning' in software and other
similar things), and this has helped me see what the steps could be. I will
give an enumerated list of steps but, just like Parnas' Rational Process,
these are not meant to be taken literally. In practice, there will always be
iteration.

I will first list all the steps, then give some commentary on them. So the
first list is meant to be succinct and, because of that, may well be
ambiguous. Convention: steps in _italics_ are optional. A step with a
**bolded** verb means that this is (currently) an "on paper" step.
See the commentary below which defines some of the terminology in more detail.

1. **Gather** all relevant theories needed to describe the problem domain.
2. Choose (or implement), from Drasil's database all theories gathered in step (1)
3. _Generate a Theory Manual_.
4. Weave a context(s) out of the above theories for problem description.
5. Describe the problem that needs to be solved, using the woven contexts.
6. _Generate a Problem Description Document_
7. **Gather** together the relevant theories to describe the tools (domain
specific and software/computing) involved
in the solution, as well as the theories for describing the properties
necessary for a good solution.
8. Choose (or implement), from Drasil's database, all theories gathered in step (7).
9. _Generate a Solution Landscape Document_
10. Describe the functional requirements for a solution, and the properties of a good solution
11. Create an SRS, that weaves together the problem description of step (5) and the information in step (10)
12. _Generate an SRS_
13. Refine the SRS so that it is implementable.
14. _Generate a Design Document_
15. _Prove that the designed solution meets its requirement_
16. _Generate the proof document_
17. Refine the implementation.
18. _Generate a software dossier_

## Commentary

See the [Terminology](Glossary) section of the Terminology page for the
definition of certain terms used below.

- Pretty much all 'Generate' steps will have _options_ that control how it
looks as well as various details specific to it.
- Many items are implicit/tacit above. Things like 'problem frames' are not mentioned at all. These will arise when checking the adequacy of the contexts.
- The subtlety between natural language generation and code generation is also omnipresent yet implicit.
- A "Theory Manual" is a background document that gives all the theories
necessary for the problem statement and solution (including the software
aspects of the solution) to 'make sense'.
- The relevant theories are arranged in a *network*. We need to weave them
together (and specialize them as well) to create a *context* in which to work.
That context, on top of being a giant reservoir of vocabulary, also will
contain ``variables'' representing not-yet-known quantities of interest.
- It is not a 'bug' if theories that eventually turn out to be irrelevant
are gathered/included. Such 'irrelevant' theories help support exploration.
- a "Problem Description Document" is akin to a pre-software SRS. One way
to think of it is as an SRS that has nothing to do with the first 'S', aka
Software. 
- The "Solution Landscape Document" lays out the *software theories* that
could be used to solve the problem. Not that step (1) did not involve the
solution space at all, so step (7) is needed for that, and this document
is part of that. The point of 'landscape' is that known but discarded solutions
may be documented here.
- Generate steps are explicit in the above as each of those also involve a 
lot of choices, and that is where those come in.
- The steps are not meant to be linear, and the in-practice process will
certainly be spiral/iterative, i.e. latter steps will always illuminate
what *ought* to have been done in previous steps, and then lead to fixes.
- The steps are not all manditory (ex: 15-16 do not exist right now).
- The steps are not sequential, even in the ideal case, i.e. one can definitely
jump to 17 from 13.
- Basically: every single step above encapsulates many pages worth of description
- 16 of the 18 steps will generate one or more DSL that is well-suited to that
step.
- For example, step (18) abstracts over all of GOOL+GProc completely! Similarly, step (12) abstracts over the recipe language for describing a particular style of SRS as well as the pretty-printer backends for HTML and LaTeX and Markdown.

## Meta-Requirements

Here we should describe all that is necessary for each step of the above
process to be achievable.

## ToDo

- [ ] Double check with what we've written before (papers, notes, things on wiki)
that steps are not missing.

### Meeting on May 13, 2024 (Smith, Carette and Balaci)

![IMG_0229](https://github.com/JacquesCarette/Drasil/assets/1422000/00834939-55b6-488e-8576-0f4d1909086a)
