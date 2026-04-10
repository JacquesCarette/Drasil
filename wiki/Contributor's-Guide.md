# Introduction

Welcome to the Contributor's Guide! This wiki page contains a gentle introduction to working with Drasil, general guidelines and advice for contributing to the Drasil research project.

If you have any questions, please feel free to either file an issue or reach out to the principal investigators ([Dr. Carette](https://github.com/JacquesCarette) and [Dr. Smith](https://github.com/smiths)). We are always looking to expand this page to be a useful guide for all contributors. Any feedback or suggestions are welcome; just file an issue :)

If you are a summer research student, please also read the [Letter to Future Summer Students](Letter-to-Summer-Students.md), written by past summer researchers.

<details>
<summary>

## Table of Contents

</summary>

1. [Introduction](#introduction)
   1. [Table of Contents](#table-of-contents)
2. [Contributor's Guide](#contributors-guide)
   1. [Preparing Your Workspace](#preparing-your-workspace)
   2. [Haskell](#haskell)
   3. [Workflow](#workflow)
      1. [What it *means* to Develop Drasil](#what-it-means-to-develop-drasil)
      2. [What it *looks like* to Develop Drasil](#what-it-looks-like-to-develop-drasil)
   4. [Guidelines](#guidelines)
      1. [Issue Tracking](#issue-tracking)
      2. [Coding Style](#coding-style)
      3. [Git Best Practices](#git-best-practices)
   5. [Editing this Wiki](#editing-this-wiki)
3. [In-Depth Guide to Drasil](#in-depth-guide-to-drasil)
4. [Note to Future Summer Research Students](#note-to-future-summer-research-students)

</details>

# Contributor's Guide

Once you've read through this page, you can take this [Contributor's Test](https://github.com/JacquesCarette/Drasil/blob/main/doc/Contributor's%20Test/ContributorTest.pdf) to gauge your understanding of git and other Drasil-related topics (**highly recommended** for future contributors).

## Preparing Your Workspace

Preparing your workspace for Drasil development requires more than a few installation step and dependencies (because of the wide breadth of software Drasil generates). As such, we have a page dedicated to [setting up your workspace](New-Workspace-Setup). The remainder of this document will assume you have a functioning development environment suitable for Drasil.

## Haskell

Drasil is developed in [Haskell](https://www.haskell.org/). If you are unfamiliar with Haskell, it is highly recommended that you pause and learn a bit of Haskell first. We have found the [Learn You a Haskell](https://learnyouahaskell.github.io/) and [Craft of Functional Programming](https://simonjohnthompson.github.io/craft3e/craft3e.pdf) books to be good introductory materials.

## Workflow

This section serves as a short version of the complete [Workflow](Workflow) article. It is recommended that you read that wiki page as well. In this section, we will discuss what it [means](#what-it-means-to-develop-drasil) and [looks like](#what-it-looks-like-to-develop-drasil) to develop Drasil.

### What it *means* to Develop Drasil

First, recall that Drasil is a software generation framework. As such, developing Drasil means developing a tool for generating software. This is like developing a compiler, except the target output is not executable code, but a complete software project with human-readable comments, documentation, build scripts, and whatever else you might find in a git repo. Drasil's development is navigated through a series of [physics-focused software projects](https://jacquescarette.github.io/Drasil/#Sec:Examples) (i.e., scientific software artifacts).

Developing Drasil can mean one of two things (or both):

1. Altering *how* Drasil is able to generate the things it currently generates (i.e., modifying the internal process).
2. Altering *what* Drasil supports generating and currently generates (i.e., broadening or refining the kinds of software Drasil generates).

For example, this might mean:

1. Refactoring Drasil's domain-specific languages (e.g., re-writing how information is fed into Drasil and where).
2. Adding support for generating more kinds of artifacts (e.g., new programming languages, data serialization languages, kinds of documents) or changing what Drasil exports (e.g., changing ODE libraries, how tuples are represented, how files are organized, etc.).

As a research project (which Drasil is), developing Drasil means studying the principal research question:

> [What if everything that is currently human-written in current software
development was instead generated?](What-If)

### What it *looks like* to Develop Drasil

As mentioned earlier, Drasil's development is guided through case study, whether it be refining our target series of software artifacts, adding features, or altering Drasil's internal process for generating software. Your primary activity will be developing Haskell code for generating other kinds of software, and then testing said other kinds of software with their respective tooling (e.g., compilers, viewers, interpreters, etc.).

At a high-level, our development framework is very similar to most other team-based software projects:

1. *Choose a task to work on.*
   * If you're new to Drasil, you might find it difficult to decide what to work on. Choosing from one of our [`newcomers`-designated tickets](https://github.com/JacquesCarette/Drasil/issues?q=is%3Aissue%20state%3Aopen%20label%3Anewcomers) is good to get the ball rolling.  Once you've seen a bit more of Drasil, you'll eventually feel comfortable choosing your own work.
   * If you're already familiar with Drasil, you'll likely know what you'd like to work on already. That being said, if you don't, that's okay, we have a myriad of [open tickets](https://github.com/JacquesCarette/Drasil/issues).
2. *Work on said task.* This will involve editing Haskell code.
   * If you're working on a `newcomers`-designated ticket, the ticket should have a good explanation of what work needs to be done and how it should be done. If it doesn't, you should ask the author of the ticket.
   * Otherwise, you will need to study Drasil's source code yourself (using our wiki as a supplementary resource) and figure out what needs to be done, just as in any other software project.
3. *Commit your work and file a PR.* Similar to other open-source projects, whenever you have work you want to contribute upstream (i.e., to the main repository), you will need to commit your code to your own branch/fork of Drasil and file a pull request (PR) from your branch/fork to Drasil's `main` branch. A reviewer (likely one of the principal investigators) will audit your work and provide feedback/request changes whenever necessary until your work can be accepted and merged into the mainline Drasil repo.
   
   In the [Workflow](Workflow) article, there is a section detailing the [GitHub Workflow](Workflow#github-workflow) including branches, pull requests, and merging to `main`. There are also some notes about [Continuous Integration (CI)](Workflow#continuous-integration-ci---github-actions--builds--tests) as a system for automating checks on GitHub to make sure Drasil compiles after any changes.

However, Drasil is also not exactly like most other software projects. With Drasil, generated case-study artifacts are checked into the main repo (under the [`code/stable/`](https://github.com/JacquesCarette/Drasil/tree/main/code) folder) and used as a reference/expectation for what future revisions of Drasil should be generating as well. This is not to say that these “stable” artifacts are flawless, only that they are what we expect, which can change. If and when you modify Drasil such that the artifacts it generates no longer match Drail, you should make sure to [update the `stable` folder](Workflow#updating-stable-folder) (that is, to reflect changes in generated artifacts).

## Guidelines

### Issue Tracking

- Please include enough information in your issue so that the reader can respond to the issue without having to track down multiple sources. Some tips on how to do that include the following: 
    - Include excerpts of PDF/HTML documentation, especially annotated excerpts, when referring to output (desired or generated).
    - Highlighting specific portions of such screenshots helps text/discrepancies stand out to the reader.
    - Linking to related issues, pull requests, comments, and commit hashes is also really helpful for easy navigation through related and significant content and discussion in the repo.
    - If there are certain code segments you want to link to an issue, instead of copy-pasting it, permalink is another option. Please see [Creating a Permanent Link to a Code Snippet](https://help.github.com/articles/creating-a-permanent-link-to-a-code-snippet/) for more information.
    - Familiarizing yourself with working with markdown will be useful in allowing text in your issues to be easier to read. For instance, when recommending a solution (or otherwise inserting multiline code), mentioning the language of the code allows for syntax highlighting. Click on `Edit` to see the source code for this Haskell excerpt below:
    ``` Haskell
    var :: String
    var = "Hello"
    ```
- Create issues that are useful for the present, and for the future, but following these guidelines:
  - Search the other issues to see if your issue has already been covered.
  - Use a meaningful title for your issue (this is what people will be reading when they search the issues in the future).
  - Do not introduce new issues within an existing issue.  We should keep the discussion focused so that it relates to the current issue.  Once an issue is resolved, it should be closed.  Related issues should be created as separate issues, rather than being incorporated into the discussion. (Related issues can be linked to the current issue via a hashtag `#` followed by the number of the other issue.)
  - Make your issue as specific as possible.  Ideally, closing an issue should be possible with a few days of effort.  This doesn't always happen, but it is something to strive for.
- When **closing** an issue, please provide rationale and relevant links to other issues, Pull Requests (PRs), or specific commit hashes. For instance, when something is considered fixed, please give a pointer to the fix, so that others can inspect your fixes.
- In GitHub, Markdown can be used to annotate Issues, Pull Requests, Comments, Wiki pages, and any other documents. Here is a useful [Markdown Cheatsheet](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet).

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

### Editing this Wiki

We do not use the same web-based workflow that most GitHub repositories follow. Rather, we carry a copy of our wiki _in_ our repo. Specifically, in the `./wiki/` folder. **To edit this wiki, please use the standard "commit and PR" workflow we follow, as with everything else in the repo.** The benefit of this approach is that we can review wiki changes through the PR workflow and tie them to tickets filed about the wiki.

# In-Depth Guide to Drasil

Make sure you fully read through the above [Contributor's Guide](#contributors-guide) section before following the steps here. This includes compiling Drasil after setting up your [workspace](#setting-up-your-workspace) and following the [Quick Start instructions](#getting-started), learning about [issue tracking](#issue-tracking), [coding style](#coding-style), the [workflow](#workflow) of Drasil, and an [important note for Windows users](#important-notes-windows-users-read-this).

To get an idea of what Drasil can do, have a look through the [Drasil website](https://jacquescarette.github.io/Drasil/). The website contains links to Software Requirement Specification (SRS) documents, both in an HTML and PDF format. There are also some links to generated code along with a case studies table to demonstrate the variety of choices that can be made by the user at the time of code generation. There are links to documentation within Drasil, as well as a section analyzing Drasil (we'll get to these last two sections later). All of the examples, example code, and the website itself are artifacts generated by Drasil. You can see their source code in the `drasil-example` and `drasil-website` folders respectively. 

One of the most important things in working with Drasil is having a solid foundation for using GitHub. Read through [Git2Know](Git2Know-for-Drasil) to learn more about branching, pull requests, merging, and anything else git or Github related. Also take a look at the [Makefile documentation](Makefile), since that will be the primary way you compile and use Drasil. More optionally, you may want to take a brief look through [Creating Your Project in Drasil](Creating-Your-Project-in-Drasil), and some [tips for debugging](Debugging-in-Drasil).

Drasil is primarily built upon the programming language [Haskell](https://www.haskell.org/). Rather than an Object Oriented language like C++, Java, Python, etc., Haskell is purely a functional programming language. All this really means is that if you like math, you are in luck! Creating a program in a functional language like Haskell is very similar to creating a series of mathematical functions that take an input and give the desired output. You will definitely want to read this great introduction to Haskell for more details: [Learn You a Haskell for Great Good](http://learnyouahaskell.com/). There are also some great sources for learning LaTeX in the [New Workspace Setup](New-Workspace-Setup#latex) that will be worth taking a look at.

Jumping more into what Drasil actually is, you may want to take a look through some [papers](Drasil-Papers-and-Documents) written by the Drasil team (especially the first three). These papers contain the conceptual foundations of Drasil and will help form some of your problem solving and design decisions. Although Drasil has grown a lot since the time these papers were published, the core ideas still remain. Check out [Terminology](Terminology) for explanations of the names we use for various Drasil-related ideas. For more of the programming and implementation side of Drasil, you may want to check out our documentation. In order of importance, [Information Encoding](Information-Encoding), [Chunks](Chunks), [Lenses](Lenses), [Recipes](Recipes), [Expr](Expr), [Combinators](Combinator-Documentation), and [References](Reference-Design-and-Documentation) may prove helpful as you work with Drasil.

When working inside the code, you will often come across foreign functions defined in Drasil. To learn more about these kinds of functions, you can either take a look at the [Haddock Documentation](https://jacquescarette.github.io/Drasil/docs/full/index.html) or use `grep` (as defined in [Debugging](Debugging-in-Drasil)). The Haddock documentation also contains a list of most functions in Drasil, which can be found in the [index](https://jacquescarette.github.io/Drasil/docs/full/doc-index-All.html) tab. You'll also want to take a look at [Folder Layout](Folder-Layout) as it details what each folder means, as well as which ones are generated and should not be changed manually.

The [analysis section](https://jacquescarette.github.io/Drasil/#Sec:Analysis) of the Drasil website contains tables and graphs that document the structure of Drasil. The linked graphs show the reliance between types, classes, class instances, and modules inside the Drasil framework. This will likely become more important as you progress with learning Drasil, so don't worry too much about it now.
