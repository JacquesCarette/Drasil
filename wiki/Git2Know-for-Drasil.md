Last Updated: July 15th, 2021

# Contents
- [Working with `git`](#working-with-git)
    - [Cloning A Repository](#cloning-a-repository)
    - [Local & remote Branch](#Local-&-remote-Branch)
    - [Commits](#commits)
    - [Undo Commits](#oops-how-do-i-undo-a-commit)
    - [Pull Requests](#pull-requests)
    - [Merge Conflicts](#merge-conflicts)
- Terminal Knowledge
    - [For Mac OS](#mac-os-terminal-need2know)
    - [For Windows](#windowsgit-bash-terminal-need2know)
- Additional `git` Info
    - [Starting New Branch](#starting-a-new-branch)
    - [Renaming Branch](#renaming-branch)
    - [Other Commands](#other-commands)
    - [Helpful VS Code Shortcuts](#helpful-vs-code-shortcuts)
- [Things to Avoid](#things-to-avoid)
- [Issues/Errors in `git`](Debugging-in-Drasil)

# Working with `git`
`git` is a software for tracking changes in any set of files, usually used for coordinating work among programmers collaboratively developing source code during software development (Credit: [Wikipedia](https://en.wikipedia.org/wiki/Git)). This guide will walk through some common steps that contributors will need to know involving `git`.


## Cloning A Repository

You can create a “local” version of a remote repository to work in by cloning it:
```
git clone <http link to repo>
```
This will create a new folder (a “clone” of the repo) in the directory you are working in. The newly cloned repository will become the place where you can modify and run code.

## Local & remote Branch

A local branch is a branch that only the local user (in this case, you) can have access to/ see/ edit. It exists only on your computer.
A remote branch is a branch from a remote location (it's  origin). 
It is also possible to push your local branch to the remote server, so other people with access to the remote server can track it.



## Commits

Commits are different versions of the repository. They are basically “screen grabs of different stages of the code as you progress”. A new commit updates a repository with changes made that differ from the last commit.

Making a commit to a repository
1. Stage commit. This will set up your files so that they may be committed to the repository. Any files that are unstaged (even if they have changes) will not be included in the commit. To stage a file, use:
    ```
    git add <relative path to file>
    ```
    To stage all modified files: 
    ```
    git add -u
    ```
    If you want to check which files are currently staged for a commit, use:
    ```
    git status
    ```
2. Creating a commit. This step will actually apply your changes to your local repository. Use the following function to commit your changes (after they have been staged):
    ```bash
    git commit -m "meaningful message to communicate changes made"
    ```
3. Pushing commits to a remote repository. This will create or update an identical branch to the repository you initially `cloned` from. For others to see the changes you made, push them to remote repository by using:
    ```
    git push
    ```
    If it is your first time pushing a new branch to the remote repository, you may have to use the following command instead:
    ```
    git push -u origin yourBranchName
    ```
4. Pulling from the remote repository. When others make changes, `git pull` syncs your local version with any changes made to the remote repository. 


## Oops, how do I undo a commit?!? 
One of the following commands may help if you find you made some changes to a commit that you want to revert.
- `git log <branchName> --oneline` (view log of all commits made and their hashes) 
- `git checkout <#commitHash>` (view specific commit in detail) 
- `git checkout <branchName>` (back to branch to undo commit) 
 ### It’s on my local repo! 
- `git reset –hard <#commitHash>` (reset commit history) 
 ### I pushed it to the remote repo! 
- `git revert <#commitHash>` (creates inverse commit to commit #commitHash)  


## Pull Requests 
This is a common process for fetching the latest data from the remote repository, making some changes, and then contributing back to the remote repository. A pull request is a method for telling the owners/maintainers of a remote repo that you wish to add your changes.
1. Make sure your master branch is up to date by switching to it (using `git switch master`) and using `git pull` 
2. From there, create a new branch using: `git checkout -b <branch-name>` 
3. Make the needed changes in that branch.
4. Specific to this repo, check that the changes you made do not break the program by navigating to the `code` folder (using `cd code`) and running the `make` command. If there are errors, you may need to check the [Debugging page](https://github.com/JacquesCarette/Drasil/wiki/Debugging-in-Drasil). Your pull request will not be accepted if there are any errors in the code.
5. Create commit as previously explained in the [section above](#commits). 
6. Push branch to master: `git push -u origin <branch-name>` if the branch is new, otherwise use `git push`.
7. Go to the [home page](https://github.com/JacquesCarette/Drasil) of the remote repo you are working on and click “Create Pull Request” for the branch you just pushed.  
8. Add a title and appropriate description. 
9. If your changes were made in response to an issue, add one of the following comments: 
    - `Closes #IssueNumber` (this option will close the issue when your changes are merged). 
    - `Contributes to #IssueNumber` (this option will not close the issue, but will still link it to the PR).


## Merge Conflicts
Merge conflicts will likely appear if more than one person is working on a repo (which is quite often). A merge conflict occurs when git cannot figure out which parts of code to include and which parts to leave out. Many times, this happens when two different changes are made to the same line of code at the same time. Unfortunately, there is no automatic fix for merge conflicts. It must be manually resolved by one of the developers or contributors.

To merge a branch, follow these steps:
1. Perform `git pull` on master.
2. Use `git switch` or `git checkout -b` to get into the desired branch.
3. Call `git merge master` to merge any new changes from master into your current branch. If your terminal does not complain, that means the merge was successful and you do not have to continue this procedure. Otherwise, continue.
4. Your terminal should now tell you if there are any merge conflicts. If so, follow the link to the file with conflicts. The files where the merge conflict happened will appear with the following syntax: 

    ```
    <<<<<<< 
    current changes (changes you just made on your branch)
    (If you’re in VSCode this will be highlighted green.) 
    ======= 
    incoming changes (new changes from master)
    (If you’re in VSCode this will be highlighted blue.) 
    >>>>>>>
    ```

5. At this point, you can now look through and edit the code by hand to combine the current changes and the changes you made to resolve the conflict. After editing, you can chose to accept either “incoming changes”, “current changes”, or just manually delete the `<`,`>`, and `=` symbols. 
6. Continue as if you were making a new commit: `git add .`, `git add *`, or `git add -u`.
7. `git commit -m “merge master to branchName”`.
8. `git push` to send your changes to the remote repo.


## Mac OS terminal need2know
The terms and commands listed below are all useful in navigating and using the Mac OS terminal.

- Working with the terminal
    - `pwd` - Prints the current working directory/file path.
    - `history`- Show commands run in terminal session
    - `clear`- Clears terminal.
    - `git init`- initializes an empty git repository (locally) in the selected file. 
- Navigation
    - `cd` or `cd~` - Go to home directory.
    - `cd <folder name>` - Go to given folder. 
    - `cd ..` - Move down once to parent folder. 
    - `cd ../..` - Move down twice. `cd../../..` moves thrice, and so on.
    - `ls`- Lists all files in the current folder.
- Working with files
    - `mkdir <folder name>` - Makes directory with a given folder name.
    - `open`- Opens a given file.
    - `cat`- Display contents of particular file. Ex. `cat /Desktop/new-folder/new-file` will display the contents of `new-file` in a `new-folder` on the `Desktop`.
   

### Example code for opening a new project: 

```
cd ~/ Desktop 
mkdir myproject
cd myproject/
git init
```


## Windows/git bash terminal need2know 

Most of the commands using git bash are similar to that of Unix style operating systems (Mac OS and Linux). Git bash is a different terminal than the default PowerShell command line from Windows and will need to be downloaded from [the web](https://git-scm.com/downloads). However, not all Unix commands work in the Windows git bash shell. It is only meant as a light wrapper to interact using git. For example, `open` and `rev` do not work. If a certain command does not work, it may not be available for Windows OS, and you may need to use the [Windows Subsystem for Linux](https://docs.microsoft.com/en-us/windows/wsl/install-win10).


## Additional `git` information

### Starting a new branch
Follow these steps:
- `git checkout master`
- `git pull`
- `git checkout -b <yourNewBranchName>`

### Renaming a branch you already pushed to master
Follow these steps:
- `git branch -m new-name` (must currently be on that branch) or `git branch -m old-name new-name` (if you’re not on that branch) 
- `git push origin old-name new-name` 
- `git push origin -u new-name`

### Other commands
- Add file `.gitignore` to the repository to tell git what file paths to ignore. Mac OS users might have to unhide files beginning with a “.”. 
- `git log --follow <file>` - Follow the history of changes of a file, looking for potential areas where a file might have been moved around from a location.
- `git status` - Displays paths with differences between the current repo and last commit. Shows all files with changes made after last commit/push and if they are staged for a commit.
- `git branch` - Shows a list of all the current branches in your local repo. 
- `git cherry-pick <commit-hash>` - Move a specific commit to a new branch.
- `git pull` - Updates your local branch with any changes made in the remote branch.
- `git checkout <branchName>` - Same as `git switch <branchName>`.
- `git push --set-upstream origin <new branch name>` or `git push -u <new branch name>` - For pushing your local branch if a corresponding branch does not already exist on your origin repo.  
- `git reset --hard origin/master` - Run on master branch to reset to upstream master. 
- (Specific to this repo), `make clean`- Runs `make` from scratch by stack clean in all directories. 
- `git clone <link to repo>` - Clones the repo for use and modification locally. 
- `git branch -d <local-branch>` - Delete a branch. 
- `git add <file-name extension>` - Stages a specific modified file.
- `git add .` or `git add *` - Stage all modified files. 
- `git add -u` - Looks at all the already tracked files and stages the changes to those files if they are different or if they have been removed.
- `grep "item" -r * --include *.hs` or `grep "item" -rw * --include *.hs` - Recursively find all instances in of the item you are looking for in files that end with `.hs`. The second one will search for occurrences of only the full word. You can also add a --color flag before the first * to make finding even easier. 
- put `*.hs` in `""` if you are using Mac OS.


### [Helpful VS Code Shortcuts](https://github.com/JacquesCarette/Drasil/wiki/New-Workspace-Setup#general-helpful-vscode-shortcuts) 


## Things to avoid 

- Don’t write lines more than 80 characters long. 
- Don’t add too many blank lines (no more than 1 or 2).

## Avoidable Issues 
I’ve made changes to the code, but the program says there’s nothing to commit? 
- might seem obvious but don’t forget to save your changes! `CTRL+S`
- or, if you are trying to change the capitalization of a file, see this [Stack Overflow page](https://stackoverflow.com/questions/10523849/changing-capitalization-of-filenames-in-git)