# Contributor's Guide

Welcome to the Contributor's Guide! This wiki page contains a gentle introduction to working with Drasil, general guidelines, and advice for contributing to the Drasil research project.

Once you've read through this page, you may also take our [Contributor's Test](https://github.com/JacquesCarette/Drasil/blob/main/doc/Contributor's%20Test/ContributorTest.pdf) to gauge your understanding of git and other Drasil-related topics (**highly recommended** for future contributors).

If you have any questions, please file an issue or reach out to the principal investigators ([Dr. Carette](https://github.com/JacquesCarette) and [Dr. Smith](https://github.com/smiths)). We are always looking to expand this page to be a useful guide for all contributors. Any feedback or suggestions are welcome; just file an issue :)

**If you are a summer research student**, please also read the [Letter to Future Summer Students](Letter-to-Summer-Students), written by past summer researchers, in addition to the [onboarding](https://github.com/JacquesCarette/Drasil/blob/main/doc/OnBoarding/onBoarding.pdf) document.

<details>
<summary>

## Table of Contents

</summary>

1. [Contributor's Guide](#contributors-guide)
   1. [Table of Contents](#table-of-contents)
   2. [Preparing Your Workspace](#preparing-your-workspace)
   3. [Haskell](#haskell)
   4. [Workflow](#workflow)
      1. [What it *means* to Develop Drasil](#what-it-means-to-develop-drasil)
      2. [What it *looks like* to Develop Drasil](#what-it-looks-like-to-develop-drasil)
   5. [Guidelines](#guidelines)
      1. [Issue Tracking](#issue-tracking)
      2. [Coding Style](#coding-style)
      3. [Git Best Practices](#git-best-practices)
      4. [Editing this Wiki](#editing-this-wiki)
2. [Where to Begin](#where-to-begin)

</details>

## Preparing Your Workspace

Preparing your workspace for Drasil development requires more than a few installation steps and dependencies (because of the wide breadth of software Drasil generates). As such, we have a page dedicated to [setting up your workspace](New-Workspace-Setup). The remainder of this document will assume you have a functioning development environment suitable for Drasil.

## Haskell

Drasil is developed in [Haskell](https://www.haskell.org/). Rather than an Object Oriented language like C++, Java, Python, etc., Haskell is purely a functional programming language. All this really means is that if you like math, you are in luck! Creating a program in a functional language like Haskell is very similar to creating a series of mathematical functions that take an input and give the desired output.

If you are completely unfamiliar with Haskell or functional programming, it is highly recommended that you pause and learn a bit of Haskell first. We have found the [Learn You a Haskell](https://learnyouahaskell.github.io/) and [Craft of Functional Programming](https://simonjohnthompson.github.io/craft3e/craft3e.pdf) books to be good introductory materials.

## Workflow

This section serves as an overview of the complete [Workflow](Workflow) article. It is recommended that you read that wiki page as well. In this section, we will discuss what it [means](#what-it-means-to-develop-drasil) and [looks like](#what-it-looks-like-to-develop-drasil) to develop Drasil.

### What it *means* to Develop Drasil

Drasil is a software generation framework. As such, developing Drasil means developing a tool for generating software. This is like developing a compiler, except the target output is not executable code, but a complete software project with human-readable comments, documentation, build scripts, and whatever else you might find in a git repo. Drasil's development has been driven by a series of [physics-focused software projects](https://jacquescarette.github.io/Drasil/#Sec:Examples) (i.e., scientific software artifacts), but the Drasil approach is suitable for any well-understood domain.

Developing Drasil can mean one of two things (or both):

1. Altering *how* Drasil is able to generate the things it currently generates (i.e., modifying the internal process).
2. Altering *what* Drasil supports generating and currently generates (i.e., broadening or refining the kinds of software Drasil generates).

For example, this might mean:

1. Refactoring Drasil's domain-specific languages (e.g., re-writing how information is fed into Drasil and where).
2. Adding support for generating more kinds of artifacts (e.g., new programming languages, data serialization languages, kinds of documents) or changing what Drasil exports (e.g., changing ODE libraries, how tuples are represented, how files are organized, etc.).

As a research project (which Drasil is), developing Drasil means studying the principal research question:

> [What if everything that is currently human-written in current software development was instead generated?](What-If)

### What it *looks like* to Develop Drasil

As mentioned earlier, Drasil's development is driven by its case studies, whether it be refining our target series of software artifacts, adding features, or altering Drasil's internal process for generating software. Your primary activity will be developing Haskell code for generating other kinds of software, and then testing said other kinds of software with their respective tooling (e.g., compilers, viewers, interpreters, etc.).

Our development workflow is very similar to most other git-version-controlled, team-based software projects:

1. *Choose a task to work on.* We use [`git`](https://git-scm.com/) and [GitHub](https://github.com/JacquesCarette/Drasil) to organize and submit work to the Drasil research project. If you are new to these tools, please review our [Git2Know for Drasil](Git2Know-for-Drasil) wiki page for a breakdown of branching and pull requests.
   * If you are new to Drasil, you might find it difficult to decide what to work on. Choosing from one of our [`newcomers`-designated tickets](https://github.com/JacquesCarette/Drasil/issues?q=is%3Aissue%20state%3Aopen%20label%3Anewcomers) is good to get the ball rolling. Once you've seen a bit more of Drasil, you'll eventually feel comfortable choosing your own work.
   * If you are familiar with Drasil, you'll likely know what you want to work on. That being said, if you don't, we have no shortage of [open tickets](https://github.com/JacquesCarette/Drasil/issues)!
2. *Work on said task.* This will involve editing Drasil's Haskell codebase and compiling and using it through a [`Makefile`](Makefile). If you are unfamiliar with what `Makefile`s are, you should learn a little bit about [what it is](https://opensource.com/article/18/8/what-how-makefile).
   * If you are working on a `newcomers`-designated ticket, the ticket should come with a concrete work plan. If it doesn't, please ask the author of the ticket.
   * Otherwise, you will need to study Drasil's source code (using our wiki as a supplementary resource) and figure out what needs to be done, just as in any other software project.
3. *Commit your work and file a PR.* To contribute your work upstream (i.e., to the main repository), you will need to commit your code to your own branch/fork and file a pull request (PR) targeting Drasil's `main` branch. A reviewer (likely one of the principal investigators) will audit your work and provide feedback/request changes whenever necessary until your work can be accepted and merged into the mainline Drasil repo.
   
   In the [Workflow](Workflow) article, there is a section detailing the [GitHub Workflow](Workflow#github-workflow) including branches, pull requests, and merging to `main`. There are also some notes about [Continuous Integration (CI)](Workflow#continuous-integration-ci---github-actions--builds--tests) as a system for automating checks on GitHub to make sure Drasil compiles after any changes.

Unlike typical software projects, however, Drasil's generated software artifacts are checked into the main repo (under the [`code/stable/`](https://github.com/JacquesCarette/Drasil/tree/main/code) folder) and used as a reference/expectation for what future revisions of Drasil should be generating. This is not to say that these “stable” artifacts are flawless, only that they are what we expect, which can change. If and when you modify Drasil such that the artifacts it generates no longer match Drail, you should make sure to [update the `stable` folder](Workflow#updating-stable-folder) (that is, to reflect changes in generated artifacts).

## Guidelines

In this section, we will discuss the guidelines regarding contributing to Drasil, including [conduct](#conduct), [issue tracking](#issue-tracking), [coding style](#coding-style), [git best practices](#git-best-practices), and [editing this wiki](#editing-this-wiki).

### Conduct

Learning how to conduct yourself is crucial to working well in any team-based setting. Please refer to our [Code of Conduct](https://github.com/JacquesCarette/Drasil/blob/main/CODE_OF_CONDUCT.md) for more information.

### Issue Tracking

* **Avoid duplication.** Before filing, please search existing issues to avoid duplicates.
* **Have a clear, narrow scope.** Your issue should have a clear, narrow scope and objective. Ideally, this coincides with a task that _should be_ resolvable within a reasonable amount of time (e.g., a week).
* **Be focused.** Avoid introducing unrelated/tangential issues within your issue. Keep discussion focused on your main goal. Instead, file complementary issues and reference them using `#IssueNumber`.
* **Write a meaningful title.** Use descriptive titles. You want your issue to be discoverable and readable. A well-written title helps prime readers for what to expect as well.
* **Write clearly and concisely.** Avoid [flowery language](https://www.lib.sfu.ca/about/branches-depts/slc/incommon/flowery-language). Be direct and [frontload your writing](https://digitalcommunications.wp.st-andrews.ac.uk/2017/03/15/web-writing-basics-frontloading/). Avoid writing paragraphs upon paragraphs that make reading a long, burdensome task.
* **Give context.** Discuss key background information to your issue. Link to related background issues/PRs. However, if your issue relies on 2 or more background comments/discussions/issues/PRs, please copy and paste key excerpts in addition to referencing them.
* **Use [GitHub-Flavoured Markdown](https://github.github.com/gfm/) to its fullest.** In addition to [basic markdown formatting](https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax), you should use rich features whenever appropriate, including:
    * [Linking to related issues, PRs, comments, commits](https://docs.github.com/en/get-started/writing-on-github/working-with-advanced-formatting/autolinked-references-and-urls). This eases navigating and helps others find your issues.
    * [Including screenshots or files](https://docs.github.com/en/get-started/writing-on-github/working-with-advanced-formatting/attaching-files). You can include excerpts of PDFs or other files, especially annotated excerpts, when discussing generated artifacts. When sending images, you can/should also add highlighting to help make areas stand out to the reader.
    * [Code permalinks](https://docs.github.com/en/get-started/writing-on-github/working-with-advanced-formatting/creating-a-permanent-link-to-a-code-snippet). Instead of copy/pasting code from the codebase, you should use permanent links to specific sections of code from the codebase.
    * [Highlighted code](https://docs.github.com/en/get-started/writing-on-github/working-with-advanced-formatting/creating-and-highlighting-code-blocks#syntax-highlighting). When recommending a solution (or otherwise inserting multiline code), mentioning the language of the code allows for syntax highlighting. Click on `Edit` to see the source code for this Haskell excerpt below:
    ```haskell
    var :: String
    var = "Hello"
    ```
    * [Pretty printing Mathematical expressions with LaTeX](https://docs.github.com/en/get-started/writing-on-github/working-with-advanced-formatting/writing-mathematical-expressions).
    * [Diagrams](https://docs.github.com/en/get-started/writing-on-github/working-with-advanced-formatting/creating-diagrams).
    * [Collapsible sections](https://docs.github.com/en/get-started/writing-on-github/working-with-advanced-formatting/organizing-information-with-collapsed-sections). This is particularly helpful when attaching long logs to issues!
* **Closing protocol.** When closing an issue, please provide rationale and relevant links to other issues, PRs, or specific commit hashes. For instance, when something is considered fixed, please give a pointer to the fix, so that others can inspect your fixes.

### Coding Style

- Use spaces instead of tabs. Preferably 2 spaces per indent level. Ex:
    ```Haskell
    top level
      indent level 1
        indent level 2
    ```
- Regarding variable names:
  - Use camel case. Ex: `someFunctionFoo`
  - Make them meaningful. Ex: `htTransCladFuel` as opposed to `hG`.
  - Try not to make them ridiculously long either - e.g. the package/module where something comes from shouldn't be repeated in a name.
  - If something is **heavily used**, then a shorter name might be warranted.
  - If you find that structure in naming variables (e.g., numbering, systematic
    concatenation, related naming, etc.), there might be missing
    connections/chunks we can make between the related variables. In this case,
    it may be beneficial to file an issue discussing your findings, and asking
    if anything can be done to remedy it. See [Issue 1848](https://github.com/JacquesCarette/Drasil/issues/1848) for an example ticket and relevant discussion.
  - See this [page](https://kowainik.github.io/posts/naming-conventions) for a broader discussion of variable names for Haskell libraries in general.
- One blank line between top-level definitions. No blank lines between type signatures and function definitions. Please don't add too many blank lines, 1 to 2 are enough!
- Surround binary operators with a single space on either side. Ex: `x ^. term`
- Do not break operations across lines, unless they are sentence combinators (`:+:`, `+:+`, `(sC)`, etc.). Ex: 
    ```Haskell
      S "The" +:+ (phrase $ heatTrans ^. term) +:+ 
      S "is the"
      ...
    ```
- Try to not make 'long' lines (i.e., lines should not be more than 80 characters wide)
- Bumping up version numbers in the `package.yaml` files:
    - The version number should be bumped up only by +0.0.1 in most circumstances.
    - It should be bumped up whenever a major change is (or multiple minor changes are) made to interfaces, such as any change to `Language/Drasil.hs`, or a change to the signature of any function that is exported there.
    - Please then also bump up the dependencies of the other packages, so that they will pick up the new version.
- Leave a single trailing newline character at the end of each file.
- We use Haddock to generate code documentation. Haddock has a specific format to change Haskell's comments into a Haddock document. For example, the syntax `-- |`. The [website](https://haskell-haddock.readthedocs.io/latest/markup.html#markup), gives examples and instructions.

### Git Best Practices

To maintain a clean, understandable project history and improve collaboration, please follow these guidelines when contributing code:
- Structure commits and pull requests thoughtfully
  - Group related changes into the same commit making them easier to track. Ask yourself if the changes you are committing serve the same coherent purpose. If so, try to commit them all together.
  - Do not group unrelated items in a single pull request. Smaller pull requests are much more manageable and highly preferred
  - Pull requests, unless trivial, should get at least 2 reviews before being merged
  - Even if GitHub gives you permission to, do not merge your own pull requests
- Clear, concise commit messages
  - When committing changes, avoid using `auto` commits and provide meaningful messages that assist reviewers in understanding what was changed and why. Ex.,
    - Good:
      1. Website: Fix typos in homepage text
      2. README.md: Add a link to the Well-Understood paper
    - Avoid:
      1. Fix stuff
      2. Update
      3. more edits
  - Of course, these messages should line up with what was actually changed
- *Acknowledge your co-authors!* Each commit declares who submitted the work. For example, [130e76](https://github.com/JacquesCarette/Drasil/commit/130e76ed4246989765cf1b7440ccadb7f226df2b.patch) shows that Jason wrote the commit (see the “From” field). Normally, we assume that Jason wrote this code himself as well. For situations where this isn't true, such as in [8deb7c](https://github.com/JacquesCarette/Drasil/commit/8deb7caf8442b90a1fa56f1926e2d44a378230a4.patch), we should note each contributor by appending a new line in the format "Co-Authored-By: X <X's email>" to the end of the commit message. If multiple authors exist, you should append one new line in that format for each contributor.
- Properly syncing with `main`
  - Syncing with `main` regularly can be a very good thing due to the existance of stable artifacts checked into the repo, when done correctly
    1. Avoid using `merge`, it results in many merge commits making the commit history very "noisy"
    2. It's better to use `rebase` as the commit history is cleaner (no merge commits). Use this as much as you like
  - Syncing with `main` is required when a merge conflict occurs in order for a PR to be accepted. It is also a particularly good idea to sync when a PR author notices someone merged a feature branch that alters `stable/`. This avoids CI errors that happen when `stable/` differs from the updated code.
- Regarding squashing commits
  - For PRs with long and/or "messy" commit histories, multiple commits should try to be squashed into a more consice set. This should be done especially in scenarios where you have a longer commit history that gradually works on the same piece of code. Ex., Let's say you have the following commits:
    1. Create evaluator for `Expr`
    2. Create pretty printer for `Expr`
    3. `Expr` Evaluator: Fix evaluation of list of numbers (one was accidentally ignored)
    4. `Expr` Pretty Printer: Remove excessive parentheses around literals
  - It would be a good idea to squash (1) and (3) together, and also (2) and (4). These pairs of commits both work on the same piece of code, and could easily be part of the same commit with no confusion.
  - These are some helpful resources related to commit squashing
    - Lots of the information in this was gathered from a [comment by @balacij](https://github.com/JacquesCarette/Drasil/pull/4065#issuecomment-2885125379), more details about squashing commits can be found there
    -  For a good example to understand the value of squashing [this comment](https://github.com/JacquesCarette/Drasil/pull/3705#issuecomment-2148869719) provides some good feedback in the context of a PR
    - **Important:** This [blog post](https://dev.to/the_real_stacie/git-are-you-an-over-committer-squash-those-commits-2klk) contains useful information on how to squash commits

### Editing This Wiki

We do not use the same web-based workflow that most GitHub repositories follow. Rather, we carry a copy of our wiki _in_ our repo. Specifically, in the `./wiki/` folder. **To edit this wiki, please use the standard "commit and PR" workflow we follow, as with everything else in the repo.** The benefit of this approach is that we can review wiki changes through the PR workflow and tie them to tickets filed about the wiki.

# Where to Begin

Make sure you fully read through the above [Contributor's Guide](#contributors-guide) section before following the steps here. This includes compiling Drasil after setting up your [workspace](#setting-up-your-workspace) and following the [Quick Start instructions](#getting-started), learning about [issue tracking](#issue-tracking), [coding style](#coding-style), and the Drasil development [workflow](#workflow).

To get an idea of what Drasil can do, have a look through the [Drasil website](https://jacquescarette.github.io/Drasil/). The website contains links to Software Requirement Specification (SRS) documents, both in an HTML and PDF format. There are also some links to generated code along with a case studies table to demonstrate the variety of choices that can be made by the user at the time of code generation. There are links to documentation within Drasil, as well as a section analyzing Drasil (we'll get to these last two sections later). All the examples, example code, and the website itself are artifacts generated by Drasil. You can see their source code in the `drasil-example` and `drasil-website` folders respectively. 

Jumping more into what Drasil actually is, you may want to take a look through some [papers](Drasil-Papers-and-Documents) written by the Drasil team (especially the first three). These papers contain the conceptual foundations of Drasil and will help form some of your problem solving and design decisions. Although Drasil has grown a lot since the time these papers were published, the core ideas still remain. Check out [Terminology](Terminology) for explanations of the names we use for various Drasil-related ideas. For more of the programming and implementation side of Drasil, you may want to check out our documentation. In order of importance, [Information Encoding](Information-Encoding), [Chunks](Chunks), [Lenses](Lenses), [Recipes](Recipes), [Expr](Expr), [Combinators](Combinator-Documentation), and [References](Reference-Design-and-Documentation) may prove helpful as you work with Drasil.

When working on the code, you will often encounter unexpected behaviour and foreign terminology. Learning to [debug](Debugging-in-Drasil) will be crucial. However, we also have some documentation to help you, such as for assistance using [`grep`]((Debugging-in-Drasil#grep-Summary)), [code reference material](https://jacquescarette.github.io/Drasil/docs/full/index.html), and an [explanation of our folder layout](Folder-Layout).

The [analysis section](https://jacquescarette.github.io/Drasil/#Sec:Analysis) of the Drasil website contains tables and graphs that document the structure of Drasil. The linked graphs show the reliance between types, classes, class instances, and modules inside the Drasil framework. This will likely become more important as you progress with learning Drasil, so don't worry too much about it now.

If you'd like to create your own project with Drasil, you should have a look at our [Creating Your Project in Drasil](Creating-Your-Project-in-Drasil) wiki page.

Finally, our wiki covers many topics. Searching the navigation pane on the right-hand side of this document may have answers to other questions you might have. If not, please always feel free to file an issue. We are always happy to help! :)

Happy hacking!
