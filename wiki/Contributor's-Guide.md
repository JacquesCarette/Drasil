# Introduction
Welcome to the Contributor's Guide! This wiki lays out some guidelines and advice for contributing to Drasil. Take a look at these pages to get a feel for how Drasil's contributions should work. This page specifically is meant to be a gentle introduction to working with Drasil. For more information, check out the rest of the articles under the Getting Started tab. If you have any questions, feel free to reach out to either [Dr. Carette](https://github.com/JacquesCarette) or [Dr. Smith](https://github.com/smiths), the supervisors.

We're looking to expand this page to be a useful guide for all contributors. If you have any requests or suggestions, please contact us.

If you are new to GitHub, a summer student, or want more in-depth instructions, please read the [articles](#in-depth-guide-to-drasil) listed below in addition to the contributor's guide.

## Table of Contents
- [Introduction](#introduction)
  - [Table of Contents](#table-of-contents)
- [Contributor's Guide](#contributors-guide)
  - [Getting Started](#getting-started)
  - [Setting Up Your Workspace](#setting-up-your-workspace)
  - [Issue Tracking](#issue-tracking)
  - [Coding Style](#coding-style)
  - [Workflow](#workflow)
  - [Git Best Practices](#git-best-practices)
  - [Editing this Wiki](#editing-this-wiki)
  - [Important Notes (Windows Users read this!)](#important-notes-windows-users-read-this)
- [In-Depth Guide to Drasil](#in-depth-guide-to-drasil)
- [Note to Future Summer Research Students](#note-to-future-summer-research-students)

# Contributor's Guide

## Getting Started
- Follow the README's [Quick Start](https://github.com/JacquesCarette/Drasil#quick-start) instructions to see what Drasil can do.
- Once you've looked at all of the contents of the Contributor's Guide, take this [Contributor's Test](https://github.com/JacquesCarette/Drasil/blob/main/doc/Contributor's%20Test/ContributorTest.pdf) to gauge your understanding of git and other Drasil-related topics (**highly recommended** for future contributors). Use this to learn the skills and knowledge needed for working with the software development infrastructure around Drasil.

## Setting Up Your Workspace
- View our [New Workspace Setup](New-Workspace-Setup) for all the steps and dependencies required to run Drasil. To get all of the features of Drasil, you will need Git to get this repository locally, Haskell (stack) to compile Drasil's code, LaTeX to compile some generated artifacts, VSCode (or any IDE) for working on Drasil, Inkscape for traceability graphs, and Graphviz also for generating traceability graphs.

## Issue Tracking
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

## Coding Style
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

## Workflow
This section will serve as a roadmap for the [Workflow](Workflow) article. Please view it for more details and specifics regarding the workflow of Drasil. The below paragraph will detail the sections of that article for easy reference. This information is also included on the self-examined [Contributor's Test](https://github.com/JacquesCarette/Drasil/blob/main/doc/Contributor's%20Test/ContributorTest.pdf) to check all the required skills and knowledge for working with Drasil.

In the [Workflow](Workflow) article, there is a section detailing the [GitHub Workflow](https://github.com/JacquesCarette/Drasil/wiki/Workflow#github-workflow) including branches, pull requests, and merging to `main`. There are also some notes about [Continuous Integration (CI)](https://github.com/JacquesCarette/Drasil/wiki/Workflow#continuous-integration-ci---github-actions--builds--tests) as a system for automating checks on GitHub to make sure Drasil compiles after any changes. In addition, the article details the process of [making changes](https://github.com/JacquesCarette/Drasil/wiki/Workflow#making-changes) to the Drasil repo, and [updating the stable folder](https://github.com/JacquesCarette/Drasil/wiki/Workflow#updating-stable-folder) to reflect any changes in generated artifacts. 

Note: If changes are made that intentionally modify the generated documentation/code, it is imperative to [update the stable folder](https://github.com/JacquesCarette/Drasil/wiki/Workflow#updating-stable-folder) in order to prevent any CI errors.

## Git Best Practices
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

## Editing this Wiki
We do not use the same web-based workflow that most GitHub repositories follow. Rather, we carry a copy of our wiki _in_ our repo. Specifically, in the `./wiki/` folder. **To edit this wiki, please use the standard "commit and PR" workflow we follow, as with everything else in the repo.** The benefit of this approach is that we can review wiki changes through the PR workflow and tie them to tickets filed about the wiki.

## Important Notes (Windows Users read this!)
- If you are using the Git Bash App (on Windows), you will need to enable UTF-8 Encoding. See **Unicode Support (Important)** under **Windows** in [New Workspace Setup](https://github.com/JacquesCarette/Drasil/wiki/New-Workspace-Setup#windows) for details on how to do this. This is required because unicode symbols are in the Drasil source code. This is how Drasil handles Greek letters.

# In-Depth Guide to Drasil
Make sure you fully read through the above [Contributor's Guide](#contributors-guide) section before following the steps here. This includes compiling Drasil after setting up your [workspace](#setting-up-your-workspace) and following the [Quick Start instructions](#getting-started), learning about [issue tracking](#issue-tracking), [coding style](#coding-style), the [workflow](#workflow) of Drasil, and an [important note for Windows users](#important-notes-windows-users-read-this).

To get an idea of what Drasil can do, have a look through the [Drasil website](https://jacquescarette.github.io/Drasil/). The website contains links to Software Requirement Specification (SRS) documents, both in an HTML and PDF format. There are also some links to generated code along with a case studies table to demonstrate the variety of choices that can be made by the user at the time of code generation. There are links to documentation within Drasil, as well as a section analyzing Drasil (we'll get to these last two sections later). All of the examples, example code, and the website itself are artifacts generated by Drasil. You can see their source code in the `drasil-example` and `drasil-website` folders respectively. 

One of the most important things in working with Drasil is having a solid foundation for using GitHub. Read through [Git2Know](Git2Know-for-Drasil) to learn more about branching, pull requests, merging, and anything else git or Github related. Also take a look at the [Makefile documentation](Makefile), since that will be the primary way you compile and use Drasil. More optionally, you may want to take a brief look through [Creating Your Project in Drasil](Creating-Your-Project-in-Drasil), and some [tips for debugging](Debugging-in-Drasil).

Drasil is primarily built upon the programming language [Haskell](https://www.haskell.org/). Rather than an Object Oriented language like C++, Java, Python, etc.), Haskell is purely a functional programming language. All this really means is that if you like math, you are in luck! Creating a program in a functional language like Haskell is very similar to creating a series of mathematical functions that take an input and give the desired output. You will definitely want to read this great introduction to Haskell for more details: [Learn You a Haskell for Great Good](http://learnyouahaskell.com/). There are also some great sources for learning LaTeX in the [New Workspace Setup](New-Workspace-Setup#latex) that will be worth taking a look at.

Jumping more into what Drasil actually is, you may want to take a look through some [papers](Drasil-Papers-and-Documents) written by the Drasil team (especially the first three). These papers contain the conceptual foundations of Drasil and will help form some of your problem solving and design decisions. Although Drasil has grown a lot since the time these papers were published, the core ideas still remain. For more of the programming and implementation side of Drasil, you may want to check out our documentation. In order of importance, [Information Encoding](Information-Encoding), [Chunks](Chunks), [Lenses](Lenses), [Recipes](Recipes), [Expr](Expr), [Combinators](Combinator-Documentation), and [References](Reference-Design-and-Documentation) may prove helpful as you work with Drasil.

When working inside the code, you will often come across foreign functions defined in Drasil. To learn more about these kinds of functions, you can either take a look at the [Haddock Documentation](https://jacquescarette.github.io/Drasil/docs/full/index.html) or use `grep` (as defined in [Debugging](Debugging-in-Drasil)). The Haddock documentation also contains a list of most functions in Drasil, which can be found in the [index](https://jacquescarette.github.io/Drasil/docs/full/doc-index-All.html) tab. You'll also want to take a look at [Folder Layout](Folder-Layout) as it details what each folder means, as well as which ones are generated and should not be changed manually.

The [analysis section](https://jacquescarette.github.io/Drasil/#Sec:Analysis) of the Drasil website contains tables and graphs that document the structure of Drasil. The linked graphs show the reliance between types, classes, class instances, and modules inside the Drasil framework. This will likely become more important as you progress with learning Drasil, so don't worry too much about it now.

# Note to Future Summer Research Students
On behalf of the Drasil team, we would like to extend a warm welcome to all future summer students. For the past few years, Drasil has been a home for many Undergraduate Researchers, especially those in first year. This contributor's guide is a product of all of the students working on Drasil, leaving advice, instructions, and tips whenever possible to help any future students who decide to work here. We personally had a great experience working on this project, and would like to leave some things we learned along the way to ensure that future students have a great experience as well. To future students, when you have finished your work on this repo, feel free to add your bits of advice at the bottom of this letter, and sign it with the year you worked here. We think it would be amazing to see all of the helpful tips and lessons passed on to new students, especially the accumulation of advice to advance this project even further.

One of the first lessons we learned here was to **never be afraid** to ask for help. Often, especially when starting, the sheer volume of Drasil's code base can overwhelm students who are new to programming in general. Thankfully, Dr. Carette, Dr. Smith, graduate students, and any other members of the Drasil team were always willing to help. If you ever get stuck on a tricky bit of code or a complex concept, drop a message in the Issue tracker. There will always be someone ready to help, so long as you ask for it. You can also tag people in issues (by using @Username) or assign issues to others and mark it with the `question` label. There are a lot of conceptual ideas within Drasil that might be hard to grasp, so we definitely recommend asking one of the professors or more experienced students about it.

When solving a problem, it is also a really good habit to come up with a solid design plan so that other contributor's can check your work. There are a lot of issues involving the design of Drasil, so communicating all steps of the problem-solving process helps to avoid conflicts within the code. Especially for more complex issues, make sure to clearly state your problem, possible solution designs, and then possible implementation of a solution.

Make this work your own! Some of the most satisfying work comes from solving problems that you found within Drasil. Actively seeking out issues that interest you helps you to learn more about the problem areas of Drasil, and gives you plenty of choice for things to work on. Treat it as you would a heavily-weighted school assignment, where you constantly look for possible improvements at all times. This also helps in learning more about software and programming in general. For example, I personally did not know the first thing about [lensing](Lenses), or that the concept even existed, but now I am able to use them comfortably because some of the issues I found required me to learn about them. Creating your own issues brings about a new kind of motivation to solve them and in my opinion made for a very enjoyable work experience. Furthermore, if you tend to gravitate to a certain kind of problem, try and find other problems similar to it! Drasil is a massive repo and its almost impossible to solve more than a few different kinds of issues at once. You can find more math-related issues like working with `Expr`, information related ones that deal with `Chunks`, or even documentation related ones (like writing this Wiki page). As an added bonus, working on similar things means you can have a central idea for a possible poster when the Annual Summer Undergraduate Research Poster Showcase rolls around in August. Actively progressing Drasil is really what contributions are all about.

The internet is your friend. Back on the topic of seeking help, you will also find yourself scrolling through various forums, documentation, and papers to understand the correct way to solve a problem. Fortunately, there are a wide variety of articles available, the closest being the Wiki. Side note: if you don't find what you are looking for in this wiki and feel that it should be added, feel free to do so, especially if it is Drasil related! Learning how to use the internet to effectively search for a certain topic is one skill that will definitely be used throughout the work term. There are plenty of free and independent resources available, so if something doesn't work out, try looking online. [Stack overflow](https://stackoverflow.com/) (and various other forums), [Hoogle](https://hoogle.haskell.org/), [Hackage](https://hackage.haskell.org/), and [Learn you a Haskell](http://learnyouahaskell.com/) will be super helpful throughout your work term.

<!-- Add any more advice here and remove this comment when done -->

Sincerely, the students of Summer 2021.
