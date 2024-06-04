For anyone getting started with developing for Drasil, the following thread discusses good candidate first/early issues:
[Starting issues](https://github.com/JacquesCarette/Drasil/issues/2096).

The projects listed below are intended to improve Drasil; the scope of these 
projects is generally larger than a single issue 
([Drasil Issue Tracker](https://github.com/JacquesCarette/Drasil/issues)).
Where a good issues should generally be close-able with less than a week of
effort, the projects listed here will likely take longer.  Moreover, not all of
the project details have been worked out, so the path to closure for each
project still needs to be determined.  Each project will likely be completed by
decomposing it into a series of issues.

All of the projects are larger than a single issue, but beyond that
characterization there is considerable variability in their scope. Some are
suitable for a summer student research project, while others would be more
appropriate for an MEng, Masters or PhD project.

The information given for each project is just a starting point.  All of the
potential projects require further thought and refinement.  An initial version
of several of the potential projects is given in the
[SE4SC Repo](https://gitlab.cas.mcmaster.ca/SEforSC/se4sc/-/tree/git-svn/Planning/PotGradStdProj).
The SE4SC repo (not public) provides some additional brainstormed ideas.

## Incorporate Pandoc into DocLang

Pandoc is a Haskell library for converting from one markup format to another.
Instructions on using Pandoc are available at:
[Pandoc Web-page for Users](https://pandoc.org/), while the code itself is
maintained in the following repo:
[Pandoc GitHub Repo](https://github.com/jgm/pandoc). From the Pandoc GitHub
page: > Pandoc has a modular design: it consists of a set of readers, which
parse text in a given format and produce a native representation of the document
(an abstract syntax tree or AST), and a set of writers, which convert this
native representation into a target format.

The AST for the documentation representation should be compared to the Drasil
representation of a document.  We could use the comparison to improve DocLang,
or maybe even replace DocLang with Pandoc.  The readers and writers may also be
helpful for conversion between different document formats.  The [data types](https://hackage.haskell.org/package/pandoc-types-1.22.1/docs/src/Text.Pandoc.Definition.html#Pandoc) that shows up
in pandoc overlap with some of the things we do in our printers.

## Scientific Knowledge Ontology

The organization of the code for Drasil has been refactored as patterns
emerge. In particular, when opportunities for reuse are observed, the code is
changed to facilitate this.  We are implicitly capturing scientific information
and the relationship between different theories, definitions, etc.  If we could
make this information explicit, it could facilitate future additions of
knowledge to Drasil.  An ontology of scientific knowledge would be useful in its
own right, especially one that is backed up by being (at least partially)
formalized.  An informal ontology for scientific knowledge is currently
implicitly available in the organization of books, papers, journals, curricula
etc, but we are not aware of a formal model of scientific knowledge.

Capturing scientific knowledge will require categorizing concepts and
determining their properties and the relations between them.  As knowledge is
gained errors and inconsistencies in SCS can be avoided at earlier and earlier
stages.  For instance, as an ontology of physics knowledge takes form, Drasil
will know the concept of the length of a beam makes sense, but the "length" of
water, does not.  Similarly, Drasil will be able to generate warnings, or error
messages, when a variable representing length is assigned a negative value, or
when Poisson's ratio is outside of the admissible range [0, 0.5], or when a
fluid mechanics theory is used outside the laminar flow range (Reynolds number
less than 2010 for flow in a pipe).

Some sample ontologies that might be relevant include:

- [Eng Math Knowledge from Stanford](http://www-ksl.stanford.edu/knowledge-sharing/papers/engmath.html)
- [Physics related](https://physh.aps.org/browse?facetIds=Research%2520Areas)

An overview of ontologies is given on
[Wiki Page on Ontology Related Definitions](https://github.com/JacquesCarette/Drasil/wiki/Glossary,-Taxonomy,-Ontology)

Developing our own ontology can be driven by the Drasil examples.  As the
information is organized for reuse, an ontology should naturally arise.  For
instance, the SSP example has the local concept of stress, but this really is a
concept that applies for any example in continuum mechanics.  However the
concept of effective stress should be reserved for continuum mechanics problems
where the material is granular, such as for a soil.  This example is discussed
as part of a
[pull request](https://github.com/JacquesCarette/Drasil/pull/2160#discussion_r434575544).

## Scientific Project Related Knowledge Ontology

This ontology is based on scientific project knowledge that Drasil already
captures.  It is not the scientific knowledge related to the domain knowledge
(discussed in another potential project); it is the things like people,
documents, expressions, references, etc.  The plan is to encode the knowledge
using [OWL](https://en.wikipedia.org/wiki/Web_Ontology_Language), where OWL
stands for the Web Ontology Language (OWL).  OWL languages have a formal
semantics and are built on the Resource Description Framework (RDF).

The initial steps are:
1. collect all of the objects that Drasil directly can talk about
   [there are actually very few!]
2. collect all the classes of objects that Drasil knows about [there are more]
3. determine all the properties that Drasil can encode

These are all in the Drasil code (in multiple packages).

Some useful links include:
- https://en.wikipedia.org/wiki/Upper_ontology
- http://dublincore.org/ and https://en.wikipedia.org/wiki/Dublin_Core
- https://www.w3.org/wiki/Lists_of_ontologies
- http://info.slis.indiana.edu/~dingying/Teaching/S604/OntologyList.html

We will look for ontologies that are close to what we can already talk about,
and then point out the differences between the prior work and Drasil.

## Generation of test cases based on data constraints and properties of a correct solution

Test cases can be generated from the typical inputs, data constraints, and the
properties of a correct solution.  Generating test cases from the data
constraints will involve improving the constraint representation, as partially
introduced in [#1220](https://github.com/JacquesCarette/Drasil/issues/1220).

The test case generation facility should also be incorporated into the Travis
continuous integration system, so that the generated code for the case studies
is automatically tested with each build.  Currently we test that the generated
code compiles, but we do not test to see if it passes any test cases.

## Add 3D Model of the Aorta to Drasil

Cardiovascular diseases are a leading cause of death globally.  Lives could
potentially be saved if we had a means of early detection of different diseases.
Automatic construction of a 3D model of the aorta from CT scans would help
researchers and clinicians.  Currently 3D aorta reconstruction is done with a
mix of manual and automatic tools.  Improving the automation would save
significant time, which could mean shorter visits to the doctor, and possibly
fewer visits.

A tool for automatic reconstruction does not currently exist.  An interesting
case study of Drasil would be to build this tool, and the associated
documetation, using Drasil facilities.  The existing Drasil examples were
created a priori and translated into Drasil.

## Generate Jupyter notebooks - probably starting with generating simple physics experiments

[Jupyter](https://jupyter.org/) notebooks are commonly used to present
"worksheets" that present the theory, code and computational results together.
This is just a different view of the same information that is already in Drasil.
Showing that we can generated Jupyter notebooks would highlight the flexibility
of Drasil.  It would also highlight the kind of knowledge that we can
manipulate.

The examples for the Jupyter notebook could start with simple physics examples,
possible borrowing ideas from
[Learn You a Physics for Great Good](https://dslsofmath.github.io/BScProj2018/index.html)

Many potential simple physics problems are given at: [My Physics Lab](https://www.myphysicslab.com/).
Another great place for Physics is [Dyna-Kinematics](https://github.com/diegomacario/Dyna-Kinematics),
which also has very pretty animations. Would make a good showcase.

## Generate Markdown

[Markdown](https://en.wikipedia.org/wiki/Markdown) is a small markup language for creating formatted
text.  It is easier for humans to write, read and edit than html.  Jupyter notebooks often use markdown
for their text, rather than more complicated html code.  Drasil generating Markdown should be relatively
easy and it would give another nice example.  We could incorporate it into the generated Jupyter notebooks.
Moreover, Markdown would give us a nice example of an internal program family, since there are several
variants including the GitHub Flavoured Markdown and Markdown Extra.

## Automatic Check for Completeness, Correctness, and Consistency

If information is missing, Drasil should inform the user.  The following
information can be checked:

- Necessary information is provided, or explicitly indicated as not
  applicable.  Necessary information could include properties of a correct
  solution.  The properties of a correct solution are easy to pass neglect at
  the early stages, but attention to this detail can definitely pay dividends
  down the road.
- The number of inputs is sufficient to find the output of a given
  equation.  
- Every "chunk" is used at least once.  If a theoretical model, for
  instance, is never referenced elsewhere in the documentation, then it is
  likely irrelevant for the given problem.  As another example, all assumptions
  should be invoked somewhere, or they shouldn't be in the documentation.
- It seems likely that every instance model should be invoked by at least
  one requirement.  Automatic generation of the traceability between
  requirements and instance models will help determine whether this is a
  realistic check.
- Add "sanity checkers" to review the Drasil code. These checkers should
  prevent "silly" mistakes. For instance, if there is a min and a max specified
  for data constraints, the min should be less than (or equal to?) the max.
- Check for words that are usually indicative of a problem with a specification
  document. The first three come from Ron Patton in the 2nd edition of _Software
  Testing_ while the last one comes from Dr. Smith.
  - **Potentially unrealistic:** always, every, all, none, every, certainly,
    therefore, clearly, obviously, evidently
  - **Potentially vague:** some, sometimes, often, usually, ordinarily,
    customarily, most, mostly, good, high-quality, fast, quickly, cheap,
    inexpensive, efficient, small, stable
  - **Potentially incomplete:** etc., and so forth, and so on, such as, handled,
    processed, rejected, skipped, eliminated, if . . . then . . . (without "else"
    or "otherwise")
  - **Potentially nonatomic:** and (in requirements)

It should be possible to turn the checks off, since there could be
cases where the user wants to ignore the warnings.

## Complete and Fix Incomplete Case Studies

The following Drasil case studies do not generate code: Game Physics, SSP and
SWHS.  These examples should be completed.  The SRS for Game Physics also needs
to be carefully reviewed.  As it is right now, the inputs and outputs for the
game physics library are not complete, or consistent.

To get SSP working will mainly require hooking it into an external library for
optimization.  The same pattern as for using external libraries with noPCM can
be used with SSP, but with optimization libraries.

When revising the game physics example, the following issues are worth considering:

- [Boundary case of collision where velocity and surface normal are pependicular](https://github.com/JacquesCarette/Drasil/issues/2256)
- [TM:NewtonSecLawRotMot should reference DD:torque](https://github.com/JacquesCarette/Drasil/issues/2258)

## Add New Case Studies to Drasil

The following projects could be added to Drasil.  They are suggested for one or
more of the following reasons: they would be of interest to potential students,
they are in an area not covered by the current Drasil examples, they are more
ambitious than the current Drasil examples:

- machine learning
- discrete probability density function
- family of data fitting algorithms
- family of finite element analysis programs
- family of convex hull algorithms

Drasil currently focuses on physics based examples.  Adding general purpose
research software tools would be helpful, since they provide a bridge between
the physics problems and how the problems are solved numerically.  For instance,
the fitting used in GlassBR and in SFS (Software for Solidification (not in
Drasil)) could be made much more generic.  We could have a family of fitting
algorithms that could be used in any situation where fitting is required.  A
proper commonality analysis of this domain could potentially show the potential
design decisions that bridge between the requirements and the design.  In the
SFS example many different fitting routines were tried.  If the experiments
could have been done easily via a declarative specification, considerable time
would have been saved.  If the experiments are combined with automated testing
and "properties of a correct solution" the human involvement could be reduced,
so that we have partially automated algorithm selection.

## Add A Recipe to Drasil for Generating Papers

Drasil can currently generate requirements documentation and code.  We should
be able to write recipes for writing physics based papers.  Since we have 
started examples on projectile motion, we should be able to generate a
physics based paper like the following:

[Projectile Trajectory of Penguin's Faeces and Rectal Pressure Revisited](https://arxiv.org/abs/2007.00926)

(The paper title might sound like a joke, but it is actually a real world
application of the use of physics.)  :smile:

## Add Support for External Libraries to Drasil

Drasil currently supports one task as implemented by an external library: solving 
an initial value problem (IVP) ordinary differential equation (ODE) for a linear ODE.
Adding external library support could start with improving ODEs. From a conversation 
with Brooks: "All of the ODE variations (coupled, BVPs, higher order) are not yet working, 
but are close. ... Any of these would require encoding the 
appropriate library as data, as well as some minor infrastructure changes to have the 
right data structures in the right places (for example, the `ODEInfo` we currently use 
is tailored specifically to IVPs, so we would need a new data structure for BVPs, 
or re-work `ODEInfo` to be general enough to work for both cases). Notably, when I 
designed `ODEInfo` I had ODE systems in mind, so for example `ODEInfo` supports a list 
of ODE equations instead of just one, though all of the example library encodings I 
created were for single ODEs. Thus, coupled ODEs are likely even closer than the 
other variations you mentioned."  After the ODE variations, support for solving a linear
system of equations (Ax = b) seems like a good candidate, since linear systems
come up often.

## A Sound Type System for Physical  Quantities, Units, and Measurements

To prevent Drasil users from building expressions that have inconsistent units, a
proper type system could be added for the units.  An [example](https://www.isa-afp.org/entries/Physical_Quantities.html) 
is available for Isabelle.  Since scientists and engineers are not always as careful
as they should be with units, an option should be available to issue warnings rather than
type errors. 

## A domain knowledge and recipes for verifying a cruise control system using Simulink

We should be able to add knowledge and recipes to Drasil so that we can reproduce an existing
case study on [Verifying a Cruise Control System using Simulink and SpaceEx](https://arxiv.org/abs/2101.00102).

## Drasil in Drasil

We should do what we preach (also known as [eating your own dog food](https://en.wikipedia.org/wiki/Eating_your_own_dog_food)).
So we should be able to write down the requirements, specification, design, etc, of Drasil inside Drasil. Right now, the
biggest first impediment to that is that our only backend is GOOL, which specifically targets OO languages. We'd have to have a
Haskell backend as well. A Haskell backend is not really all that difficult, at least if it is done in parallel with GOOL.
Having it as part of GOOL seems infeasible, as GOOL is on-purpose OO-specific. There are of course parts of the expression
language which could be re-used, but it's not clear if that's worth it.

It is, of course, not that simple. The next, even larger, problem is that our specification language really does not
let us talk about representations (i.e. what would eventually become data-structures). So that would be needed too.

Luckily, the whole project could be done incrementally, meaning that various pieces of Drasil could be generated, and plugged in
to the hand-written code.  drasil-lang is probably "best understood", for example.  drasil-docLang is kind of at the opposite end. 
drasil-data is close to drasil-lang in being understood.

Such a task would most likely entail de-embedding drasil-example (and drasil-data). Meaning that we'd need to have an external syntax for describing the examples. That would definitely be a good thing. Same with the fundamental knowledge in drasil-data, it should get an external
syntax.

Most importantly, this would require an analysis of all of our own softifacts, classification of what knowledge is in them,
how to capture that knowledge, and what recipes to reproduce those softifacts would look like. That would be some of the most interesting
parts of the work. Some of the meta-structure of the code in drasil-lang, i.e. what's a data-structure, what is a class, would
require some very careful thinking. The wonderful thing about that is that all that information is **design information**, something
that we don't have enough of right now.

## Check Drasil and Generated Projects with Checklists

We should eventually check Drasil against existing checklists.  Using checklists we may find some ways to improve our artifacts and infrastructure.  For instance, I don’t think we explicitly mention known bugs (or in our case, explicitly state the current scope/limitations of Drasil).  I also don’t think we have test coverage metric.  Maybe Haskell tell us what lines of Haskell are exercised when we generate all of our examples?  If there are files that aren’t used, that would be useful information.

Checklists can also be applied to our generated programs.  We could look to see what is existing from our complete examples, like GlassBR.  This might give us some "low hanging" fruit that will improve the relevance of our examples.  For instance, we could likely easily add an AUTHORS file and similar artifacts.  It would be more work to add a design document, but GlassBR does have an assumed design that we could roughly document.

If we go through the exercise of “grading” Drasil and one of our generated case studies, we could get a nice “to do” list.

Here are some good checklists to start with, in rough order of preference:

[CLARIAH](https://github.com/CLARIAH/software-quality-guidelines/blob/v1.0/softwareguidelines.pdf)

[Software Sustainability Institute Form](https://docs.google.com/forms/d/e/1FAIpQLSf0ccsVdN-nXJCHLluJ-hANZlp8rDKgprJa0oTYiLZSDxh3DA/viewform)

[Eurise checklist](https://github.com/eurise-network/technical-reference/blob/v0.1/quality/software-checklist.rst)

[Scottish Covid Response Modelling Software Checklist](https://github.com/ScottishCovidResponse/modelling-software-checklist/blob/main/software-checklist.md)

[DLR Software Engineering Guidelines](https://zenodo.org/record/1344612#.YdXihRPMJm8)

We also have a list of more [checklists](https://gitlab.cas.mcmaster.ca/SEforSC/se4sc/-/wikis/Advice-and-Checklists-for-Repos-(including-a-list-of-recommended-artifacts)).

The common artifacts recommended by the different software development guidelines are summarized in Table 3 of the "[Digging Deeper](https://github.com/smiths/AIMSS/blob/master/StateOfPractice/Papers/ICCS-2022-SOP_MethodologyPlusExamples/DigDeep.pdf)" paper.

This project can be split into sub-projects:

- grade Drasil
- grade Drasil output
- analyze/prioritize each of the above; priority should weight both ease of implementation and ‘impact’
- create tasks to fix the highest priority items

## Develop a Strategy for Working with Shared External Libraries

Drasil currently uses external libraries, like scipy, for their ODE solvers.  As Drasil development continues we'll be adding more external libraries.  Currently we keep copies of the external libraries we are using as part of our repo.  This isn't practical in the long-term, given the amount of space the libraries will consume.  The current approach is made even worse because the copies of external libraries aren't currently shared between examples.  We have sorted out that problem with symbolic links (#2980), but this is not an elegant solution.  The question remains on how we should handle dependencies.  Should we use a package manager (like nix, homebrew (for Macs), etc)?  Should we just generate instructions for the user and let them install the dependencies?  Should we generate files for the common `.configure`, `make build`, `make install` chain of commands? 

## Improve Automation of Formatting Long Equations in LaTeX Generated pdfs

In the Drasil generated LaTeX code long equations can run off the edge of the page.  For instance, the equation is cut off in the [double pendulum example for IM:calOfAngularAcceleration1](https://jacquescarette.github.io/Drasil/examples/dblpendulum/SRS/srs/DblPendulum_SRS.pdf)):

![LongEquation](https://user-images.githubusercontent.com/1422000/186962397-7ae7ac6f-69fe-4c9c-b450-20f8053c209f.png)

Ideally we would like to generate code that is automatically formatted to fit on the page.  As discussed in #718, full automation is likely too difficult with the information Drasil currently has access to.  Drasil would need to know more, like the page width, font size etc.  Rather than full automation, we can aim for providing the user with access to options that more aggressively break equations across lines.  This topic, and some ideas for how to split equations across lines are discussed in #718.

## Generate Graphs in Drasil

Drasil could be taught generate graphs (bar charts, line charts, scatter plots etc) from available data.  The data could be entered into Drasil directly, or it could be an output of calculations.  To do this we would need to understand the vocabulary of graphs.  A starting point could be the prior work on [infographics](https://modeling-languages.com/model-driven-production-of-data-centric-infographics/).

## Visualizing our Case Studies

As with any scientific problem, visualizing is always helpful, intriguing, and encouraging to those pursuing science. For our case studies that produce code, we've already gone through the work of understanding how the calculation works. As an alternative "view" of our body of knowledge gathered to generate "solving code", we can similarly generate web apps that simulate the same problems using visual information. In particular, generating graphs and diagrams can be helpful in understanding how abstract theories work and how they concretely appear.

For example, [myPhysicsLab](https://www.myphysicslab.com/) and [JavaLab](https://javalab.org/en/) are both web projects with similar objectives to this idea: simulating physics experiments.

[Elm](https://elm-lang.org/) is a functional language, similar to Haskell, and follows a specific [Model-View-Update](https://guide.elm-lang.org/architecture/) architecture, and a potential target language for our simulation software artifacts. Regarding the MVU architecture, we should be able to create a "model" that contains the notable symbols we have in our case studies, a "view" that displays the options for the simulations and the simulation according to our "model", and an "update" module that updates the "model" according to changes in the options (and over time). The "update" and "model" components should largely be based on our existing "code" generation. Elm does not necessarily need to be our target language for simulation software, but it is a user-friendly and commonly used language.

## Investigate / Add Hackage Dimensional Package
Moved here from Issue [#1182](https://github.com/JacquesCarette/Drasil/issues/1182). 

Currently, Drasil has a home-growm 'units' handling module. This project would investigate if we can switch to a third-party solution.

[dimensional](https://github.com/bjornbm/dimensional) is a potential third-party package.

* Does the package have a notion of Expr? If so, investigate to see compatibility. 
* Investigate how to interface package
* Add interface

The issue can be reopened and assigned once the project is taken on.





