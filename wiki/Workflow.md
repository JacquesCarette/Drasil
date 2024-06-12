# Table of Content
- [Table of Content](#table-of-content)
- [Desired Workflow In Drasil](#desired-workflow-in-drasil)
  - [Git Workflow](#git-workflow)
    - [Branches](#branches)
      - [Creating a new branch](#creating-a-new-branch)
      - [Pruning branches](#pruning-branches)
      - [Cherry-picking](#cherry-picking)
      - [Deleting commits](#deleting-commits)
    - [Pull Requests](#pull-requests)
    - [Merging Pull Requests](#merging-pull-requests)
  - [Continuous Integration (CI) - GitHub Actions / Builds & Tests](#continuous-integration-ci---github-actions--builds--tests)
  - [Making Changes](#making-changes)
    - [Renaming Functions](#renaming-functions)
    - [Updating Functions](#updating-functions)
    - [Implementation Details](#implementation-details)
  - [Updating Stable Folder](#updating-stable-folder)
    - [Important Tips](#important-tips)

# Desired Workflow In Drasil
## Git Workflow
### Branches
#### Creating a new branch
One of the most common first tasks when starting new work is creating a new branch to house your work. Generally, you will always be branching off of the `master` branch. However, at times, others will be working on similar or related things which may have code conflicts with your soon-to-be work. As such, when you would like to start new work, you should first check if anyone is doing related and/or conflicting work. If there is existing work that heavily changes what you would be writing, you should discuss with the maintainers of that conflicting work and the project maintainers to decide if you should be branching off of `master` or off their branch.
 
When you've decided which branch to start writing your code on top of, the commands you will run are as follows:
```bash
git checkout <master/yourPreferredBaseBranch>
git pull
git checkout -b <yourNewBranchName>
```
(note: please omit the `<>`)

This ensures your new branch is created from an up-to-date base branch. Please see [`git2know`](https://github.com/JacquesCarette/Drasil/wiki/Git2Know-for-Drasil#starting-a-new-branch) for more details.
- For changes that are unlikely to break anything, it's ok to have a "long-lived" branch (2-3 days) that accumulate commits. Once you're done, you then submit a pull request. To make sure your code will pass the Pull Request tests performed, you should run `make pr_ready` and resolve any errors (if any) locally first.
- For changes that are likely to conflict, make small edits, update stable, commit, then [merge master](https://github.com/JacquesCarette/Drasil/wiki/Git2Know-for-Drasil#merge-conflicts) to your branch. Make sure to separate non-cohesive changes in different commits, in order to document what changed where. Constantly keep your branch up to date with master to minimize conflicts.

#### Pruning branches
`git remote prune origin` deletes the refs to the branches that don't exist on the remote; by adding the `--dry-run` flag, one can view all the branches that will be pruned before the action is completed.

#### Cherry-picking
Ensure you are on the branch you want to apply the commit to using `git checkout` and execute `git cherry-pick <commit-hash>`, where `commit-hash` is the commit you'd like to move to the new branch. Please see this [Stack Overflow article](https://stackoverflow.com/questions/34027850/how-to-pull-request-a-specific-commit) for more details.

#### Deleting commits
From your local machine, use `git reset --hard x` where `x` is the commit hash of the commit you want to restore your local repo to (i.e. everything after commit `x` will be removed).

### Pull Requests
- Continuously keep your branch up to date with `master` to keep merge conflicts to a minimum, especially before starting a new branch. This can be done by using `git pull` on the `master` branch, or `git pull origin master` if you'd like to make sure your working branch is mergeable into master.
- Wait until something is "complete" in a branch before making a [pull request](https://github.com/JacquesCarette/Drasil/wiki/Git2Know-for-Drasil#pull-requests) (PR). It's fine to have lots of work that is incomplete in a branch (even multiple branches), but PRs should be done when it is expected that they can be incorporated "right away" into master. Before making a PR, you should check `make pr_ready` to ensure that it will pass the automated tests necessary to get your code merged in right away.
- If you would like to draft your code, please convert your PRs to Draft PRs before creating them so that maintainers know that the code is not yet ready for merging, and is pending more work/discussion.
- Keep PRs as small as possible.
- You should avoid submitting a pull request then keep adding commits to it. You should be doing that work locally instead, and submit a pull request when it is ready to be merged.
- Remember to also link related issues, so that others can know what is motivating the pull request. However, **do not** manually link issues to pull requests; rather, in the PR description, write (at minimum, the required keywords, replacing #ID with the issue ID):
    - "Contributes to issue #ID" or "contributes to #ID" → links issue to PR, but doesn't close issue when PR is merged (required keywords: #ID).
    - "Closes issue #ID" or "Closes #ID" → links issue to PR, and closes issue when PR is merged (required keywords: closes, #ID).
- I am working on an issue that requires multiple pull requests to complete, some of which are dependent on others. I have created one branch that I intend to use for my multiple pull requests for the same issue.
    - This can be done; however, it is problematic, as outlined in [this comment](https://github.com/JacquesCarette/Drasil/pull/2157#issuecomment-637079762); instead, follow the advice given in the [same comment](https://github.com/JacquesCarette/Drasil/pull/2157#issuecomment-637079762). In summary, create a new branch for each PR to avoid confusion.
- I am working on an issue that has resulted in a long-lived branch with many accumulated commits; now this branch is out-of-date with the master branch. How do I update my branch to be up-to-date with the master branch without losing my commits?
    1. First, ensure that you are on your out-of-date branch:
       - Change your working directory to `./Drasil/`. For info on how to do this, refer to the second step in the [Quick Start Guide](https://github.com/JacquesCarette/Drasil#quick-start).
       - Run the command `git branch` to check that you are on the correct branch (current branch should be highlighted).
       - Run the command `git switch branchName` (replacing `branchName` with your out-of-date branch name) to switch onto the correct branch.

    2. Has the out-of-date branch ever been pushed beforehand? For more info on pushing commits, see step 3 of this [wiki article](https://github.com/JacquesCarette/Drasil/wiki/Git2Know-for-Drasil#commits).
       - If yes, run the following commands:
       	```
       git fetch
       git merge master
       ```
       - If no, run the following commands:
        ```
       git fetch
       git rebase origin/master
       ```
    3. Running these commands should make your current branch up-to-date with the master branch.
- Please see this wiki article on [PRs](https://github.com/JacquesCarette/Drasil/wiki/Git2Know-for-Drasil#pull-requests) for more details.

### Merging Pull Requests
- **(Who should do it)** Please allow one of the supervisors to do the actual merge.
- **(Dependent changes)** If your work requires the changes made to a branch that has not yet been merged into `master` (i.e. when creating multiple pull requests that are dependent on one another), refer to [this comment](https://github.com/JacquesCarette/Drasil/pull/2157#issuecomment-637079762) and [this comment](https://github.com/JacquesCarette/Drasil/pull/2157#issuecomment-637108852) for further clarification regarding how it should be done.
- **(Independent changes)** Independent changes should each get their own branch and pull request.

## Continuous Integration (CI) - GitHub Actions / Builds & Tests
We use GitHub Actions as a means of testing PRs to ensure that changes do not break our stable `master` branch.
- When you create a PR, GitHub Actions will automatically create a build for your branch at time of PR creation, and will also create a build/test for each commit you created on your source branch after PR creation. It might result in a "build failure", but don't worry (!), you can grab the logs and see what went awry. To ensure your code won't fail the CI tests, you should run `make pr_ready` and resolve any automatically found errors before creating your pull request.
- Alternatively, you may include `[workflow-trigger]` anywhere in a commit message to your source branch to have the GitHub Actions build & test script run on your source branch without having a PR around to automatically test it. 
- If you would like to manually run or re-run a workflow script, please visit the ["Actions"](https://github.com/JacquesCarette/Drasil/actions) tab's workflow dispatch area.
- Workflow runs are logged under the "Actions" tab. You may view your workflow run logs there for information about why your build succeeded/failed. These workflow runs will also be noted next to your commits in commit histories on GitHub and on related PRs.

## Making Changes
**All** changes made to the files in this repo should be "physics knowledge preserving". We have had issues where the easiest solution to get some particular knowledge into the DSLs for Drasil was to simplify the knowledge by removing assumptions, sources, derivations and links to other definitions (to name a few examples). There is some "filler" information in the manual documentation, but almost everything in there is there for a reason. It might not always be a good reason, but changes that remove physics should not be made unilaterally. A discussion is necessary in all cases, which is best documented in an issue tracker.

### Renaming Functions
When renaming a function, it is best to find all instances of the function in the `code` directory and its subdirectories. Rename all instances and make sure that the [`make`](Makefile) rule compiles.

### Updating Functions
When updating a function's implementation, a good idea is to rename all original instances to something that is easily searchable, such as `oldFunction`. Then slowly integrate your new function by replacing old instances with new ones. Follow these steps:
1. Implement a few replacements
2. Check to make sure nothing is broken
3. Make any necessary updates to stable (as long as they are correct).

### Implementation Details
When coming up with an implementation of a feature/fix, the implementation details, as much as possible, should be hidden from the user. For example, instead of having a singular constructor that can take a `Nothing` value for an intentionally empty list or a `(Just xs)` for the list `xs`, (which lets the user input `(Just [])` which could be intentionally empty or not), it's a better idea to implement two constructors, one that doesn't take a list (and passes in `[]` behind the scenes), and one that takes a list and throws an error if its empty. This prevents the user from making a low-level mistake, when it should be enforced by Drasil.

## Updating Stable Folder
The SRS artifacts generated by Drasil will appear in the `build` folder. For each example folder within `build`, there are `TraceyGraphs`, `SRS/PDF`, and `SRS/HTML` subfolders. Some examples also have a `src` subfolder containing the generated source code for the example. After a successful compilation and execution of Drasil (by running `make`), the files in each example subfolder are updated to reflect any new changes made to the code. To ensure that we do not have any unwanted changes, we compare each newly generated `build` folder against its previous version. The previous version's files can be found in the `stable` folder, and mirrors the layout of the `build` folder. To avoid any errors or 'breakage' of `stable`, we need to manually copy changed files into the corresponding subfolders in the `stable` folder, replacing the old files. This is to ensure that `stable` has the latest version of the build files. Note that you should always check whether your `build` version of the SRS is correct before overwriting the files in `stable`. Here's how to go about this:
- Find any existing problems in the logs generated by comparing the differences between the `build` folder and the `stable` folder:
    1. Run `make` in your terminal.
    2. Look for `diff --strip-trailing-cr -r -X ../.gitignore -x '*.txt' stable/<FileName> build/<FileName>/ > "logs/FileName"`
    3. View the log files in the `logs` folder. Any non-empty files will display the discrepancies between the stable & build files.
- After checking to make sure the changes in `build` are correct and wanted, place a copy of the specific build file (the one the logs showed there was a problem with) into the stable folder, overwriting the existing one in stable. Commit and push these changes before a PR. This will help avoid Continuous Integration (CI) build failures or errors.
- The above can also be done by using `make stabilize`, or for a specific example `X`, `make X_stabilize`.
 
### Important Tips
- Remember, you can do multiple [`git add`](https://github.com/JacquesCarette/Drasil/wiki/Git2Know-for-Drasil#commits) (staging changed files) before doing a single commit
- This is especially good for pull requests involving changes to multiple files (e.g. Haskell scripts and stable folder files); this will help avoid CI build failures or errors (see [Updating Stable](#updating-stable-folder-files) for more information).

