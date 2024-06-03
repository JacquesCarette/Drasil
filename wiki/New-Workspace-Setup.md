# System Prerequisites

Collaboratively working on any software, you generally need a few things; a compiler, shell + generally available system tools, version control system, and an IDE. In addition to this, we often construct TeX documents alongside our software, requiring you also have a working LaTeX installation. This guide will give you a simple workspace recommendation for installing Stack (a Haskell development toolkit), VSCode (an IDE), and a basic LaTeX toolkit. Windows users will need to either use Windows WSL (preferred), or install `Cygwin` and `Git` if they don't already have them so that they can have a good BASH shell, common dev utilities, and a Git client. It is [**recommended**](https://github.com/JacquesCarette/Drasil/issues/2913#issuecomment-987300398) for Windows users to install the following tools using Windows WSL instead of native Windows installations.

In order, you should be installing:
1. [Basic Development Tools](#basic-development-tools)
2. [Stack](#stack)
3. [VSCode](#vscode) + [Extensions](#extensions)
4. [LaTeX](#latex)
5. [Inkscape](#inkscape)
6. [Graphviz](#graphviz)

Optionally, some functions will be limited without:
1. [Doxygen](#doxygen)

**Notes**: 
1. It is [**recommended**](https://github.com/JacquesCarette/Drasil/issues/2913#issuecomment-987300398) for Windows users to install the following tools using Linux on Windows/Windows WSL. If you choose to install using Windows WSL, you can safely ignore all Windows installation notes (except for that regarding Unicode support), and follow the instructions for Linux machines with the *apt*-package manager (e.g., Debian, Ubuntu, etc). From experience, installations with Windows WSL over native installations provide better tools, quicker installs, and an overall smoother experience.
2. If you use the `nix` package manager and/or the `NixOS` operating system, you may alternatively use the `shell.nix` ad-hoc development environment provided, however, you should still install [VSCode](#vscode) + [Extensions](#extensions) and [install the `lmodern` and `lmmath` fonts](https://nixos.wiki/wiki/Fonts).

## Basic Development Tools

We commonly use [`GNU Make`](https://www.gnu.org/software/make/) to simplify many of our common compilation and testing steps for Drasil. Additionally, we use `git` for source code management.

Git is a version control system for collaboratively building software with others. Using git, you may distribute and contribute software source code, with a full version history of the source project. We use git for collaborating development efforts on Drasil. In addition to the bountiful publicly available [documentation](https://guides.github.com/introduction/git-handbook/) on using `git`, we also have our own primer [Git2Know](https://github.com/JacquesCarette/Drasil/wiki/Git2Know-for-Drasil) document.
 
If you're using a Mac or Linux based computer, you will likely already have a terminal. Mac users may press `Apple Logo + SPACE` and search for "Terminal" to use it. Linux users may use their terminal and shell of their choice, but should use something that is compatible with BASH or ZSH.

### Installation

Please follow the instructions related to your operating system.

<details>

<summary><h4>Windows</h4></summary>

If you're on Windows, you will need some sort of bash shell, a git client, and common development tools (e.g., GNU Make, rm, sort, rev, etc).

[Git Bash](https://git-scm.com/downloads) is a custom terminal for Windows users packaged with `git` and other various generally used *nix system tools. The installation is process is fairly simple; you will need to download their [Git Bash installer](https://git-scm.com/downloads) and run it. The installer will give you a series of steps, please go through them, leaving all options shown default, unless you specifically know what each option does. Once it's been installed, you can open it by pressing the four-flagged Windows key and searching for "Git Bash", clicking on the "Git Bash" application showing up (this will open up a black terminal window which you can use). From there, you should be able to run "git --version" and get output displaying the Git version of your installation.

[Cygwin](https://cygwin.com/) provides a suite of commonly used GNU and Open Source tools in many development workflows. It provides shell tool functionality similar to many Unix-like systems. You should download the [Cygwin Installer](https://cygwin.com/install.html) and run it. It will guide you through your Cygwin installation, where you might need to pick a download mirror. Try to pick a mirror with the same TLD as your country (e.g., '.ca' preferred for Canadians -- this will likely give you a lower latency and generally faster download), but it might still end up taking a bit of time. At the package list window, please make sure to search for "make" and double-click it's "Skip" entry (it should show a specific version and be marked for installation), and then search for "util-linux" and double-click it's "Skip" entry (it should show a specific version and be marked for installation). If you would like to install any other packages you're interested in, please feel free to do so. After having selected the packages, please click "Next" to confirm the list of packages that you're installing, and then "Install" to begin the installation.

Once Cygwin and the packages have been installed, you can access the installed tools by pressing the four-flagged Windows key and searching for "Cygwin", clicking on the "Cygwin" application showing up (this will open up a black terminal window which you can use). From there, you should be able to run "git --version" and get output displaying the Git version of your installation. Additionally, you should also be able to access other common tools, such as `rm`, `rev`, `sort`, and [more](https://cygwin.com/packages/package_list.html).

If you would like to install more *nix tools through Cygwin, you can use the [cygwin packages list](https://cygwin.com/packages/package_list.html) to look for your package and install it by re-running the [Cygwin setup installer](https://cygwin.com/install.html).

The rest of the steps in this guide will assume that you are using Cygwin as your preferred Windows terminal. Git Bash is also very safe to use, but you will need to download [util-linux-ng](https://gnuwin32.sourceforge.net/packages/util-linux-ng.htm), which includes various system utilities (one of our scripts uses `rev`), and add its **bin/** to your PATH.

##### Unicode Support (Important)
Unfortunately, it seems that Cygwin, Git Bash, and Windows still have issues with UTF-8 encoding, resulting in odd errors in development and empty characters appearing in the Cygwin/Git Bash shell windows. To resolve this issue, each time you open up Cygwin/Git Bash, you will need to run `chcp.com 65001` or `chcp 65001` so as to change the encoding of the terminal to UTF-8.

##### 7-Zip
If you are using Git Bash you will need [7-Zip](https://www.7-zip.org/). Download and run the installer from their website.

Alternatively, if you have [Chocolatey](https://chocolatey.org/install) installed, you can open up a new command line as an administrator and run the following command:
```
choco install 7zip.install
```

##### Symbolic Link Support

The Drasil directory structure relies on symbolic links. Windows 10/11 has support for this, but it needs to be enabled.
1. Enable Developer Mode. On Windows 11, this is under Settings -> Privacy & Security -> For developers. On Windows 10, this is under Settings -> Update & Security -> For developers.
2. Enable symbolic link support in Git. Run `git config --global core.symlinks true` in the terminal. If you already cloned the Drasil repository at this point, you need to delete it and clone it again for the setting to take effect.

</details>

<details>

<summary><h4>Mac</h4></summary>

Mac already comes with a shell + good terminal, so you will only need to install `git`.

If you're using a recent version of Mac (> Mavericks/10.9), please open up a terminal window (`Apple Logo+SPACE`, then search "Terminal"), and type in `git --version` into it, and press enter. A GUI installer will appear prompting you to install Git. Please follow the on-screen steps, leaving all options default. Once the installation is complete, you should be able to type `git --version` into your Terminal window again and receive a version code back instead of having the GUI pop up again. If you're using an older version of Mac, you may follow through the steps of installing Git using any of the [official repositories](https://git-scm.com/download/mac).

</details>

<details>

<summary><h4>Linux</h4></summary>

It's very likely that your operating system came with an installation of `git` and many common development tools, however, if it didn't, you will be able to use your package manager to install it.

*Apt*-based distributions:
```
sudo apt install git build-essential
```

*Nix*:

Add `git` and `gnumake` to your system packages list. Most other common utilities should come pre-installed.

</details>

### Configuring `git`

Out of the box, git is _almost_ ready to use for our purposes. We require that you set your identity and create an SSH key to clone our repo from GitHub.

#### Setting your identity

You will need to set your name and email in your Git configuration so that when you contribute your work, others will know who to credit the work to.

Please run the following commands in your terminal, replacing the fields with your information.

```
git config --global user.name 'Your Name'
git config --global user.email 'YourEmail@YourEmailProvider.com'
```

If you would like to remain anonymous, you are free to set your name to anything you would like (but please keep it appropriate), and you can use your [GitHub's private personal email](https://docs.github.com/en/github/setting-up-and-managing-your-github-user-account/managing-email-preferences/blocking-command-line-pushes-that-expose-your-personal-email-address) as your email.

Please note that the email that you use must match one of the emails registered on your GitHub account. Otherwise, your work will not be counted in the Drasil statistics and contributions page.

#### SSH keys

As per [Wikipedia](https://en.wikipedia.org/wiki/Secure_Shell_Protocol), SSH uses public-key cryptography to authenticate your identity from other machines, frequently used for client-server connection authentication.

You can read more on GitHub's [About SSH](https://docs.github.com/en/github/authenticating-to-github/connecting-to-github-with-ssh/about-ssh) webpage if you're interested.

##### Creating an SSH key

Please follow GitHub's official documentation on [generating a new SSH key](https://docs.github.com/en/github/authenticating-to-github/connecting-to-github-with-ssh/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent).

##### Registering your SSH key with GitHub

Please follow GitHub's official documentation on [adding an SSH key to your account](https://docs.github.com/en/github/authenticating-to-github/connecting-to-github-with-ssh/adding-a-new-ssh-key-to-your-github-account) and [testing your connection](https://docs.github.com/en/github/authenticating-to-github/connecting-to-github-with-ssh/testing-your-ssh-connection).

##### Cloning `Drasil` locally

To copy/clone `Drasil`'s software on your machine, please open up your terminal window, navigate to a directory you want Drasil placed in, and run `git clone git@github.com:JacquesCarette/Drasil.git`. Afterwards, a `Drasil` folder will appear in your working directory and you can work on it as you see fit.

##### BONUS: Signing your commits

If you use GPG, you can also protect your commits by [signing them with GPG](https://docs.github.com/en/github/authenticating-to-github/managing-commit-signature-verification/signing-commits).

## Stack

Stack is an easy to use toolkit for building, executing, testing, and benchmarking your Haskell software, including tooling for isolated GHC installations and dependency management (similar to Python's `virtualenv` using a [curated listed of packages](https://www.stackage.org/)).

Stack has an amazing document repository available on their [Read the Docs Official Website](https://docs.haskellstack.org/en/stable/README/). It has an [Installation Guide](https://docs.haskellstack.org/en/stable/README/#how-to-install), [Quickstart Guide](https://docs.haskellstack.org/en/stable/README/#quick-start-guide), [User Guide](https://docs.haskellstack.org/en/stable/GUIDE/), and much more!

### Installation

Please follow the related instructions to your operating system.

<details>

<summary><h4>Windows</h4></summary>

Run the [Stack installer](https://get.haskellstack.org/stable/windows-x86_64-installer.exe) executable file and follow the on-screen steps. While going through the installer, don't change any of the default settings. The default settings will ensure that "stack" is available in your PATH, and hence usable in your Git Bash.

##### Confirm Installation
To confirm you've successfully installed stack, press the four-flagged Windows key, search for ```Git Bash```, and click the Git Bash application that shows up. A blue/black window should pop up. Finally, to confirm that "stack" has been successfully installed, type in ```stack --help``` into the terminal window. If some manual information appears, then you have successfully installed stack! Otherwise, you will need to go through these steps again.

##### Windows Security
After installing/running Stack, you might notice that Windows Security blocks or even deletes the **stack.exe** executable. If this happens, add an exclusion to Windows Security by going to **Start > Settings > Update & Security > Windows Security > Virus & threat protection > Manage settings** (under **Virus & threat protection settings**) **> Add or remove exclusions** (under **Exclusions**), then selecting the **bin/** folder with your **stack.exe** file. If Windows Security deletes the executable, simply reinstall it. (This issue was encountered in Windows 10).
</details>

<details>

<summary><h4>Mac</h4></summary>

For Mac users, open up your terminal (Apple logo + Space, then search "Terminal"), and run:
```
xcode-select --install
```
Note: It may take a while to run.

Finally, run:
```
curl -sSL https://get.haskellstack.org/ | sh
```
(from [Official Stack Webpage](https://docs.haskellstack.org/en/stable/README/)) inside the terminal window. This will fully install Stack. It might ask you for permissions while installing, this is normal, please give it permission.

##### Using homebrew (Alternative)
If you prefer to use Homebrew to install software on your machine, please run:
```
brew install haskell-stack
```
However, the homebrew Stack package is unofficial and may not be up-to-date.

##### Confirm Installation
To confirm you've successfully installed stack, you should run ```stack --help``` in your terminal window. If some manual information appears, then you have successfully installed stack! Otherwise, you will need to go through these steps again.

#### Important Note about PATH environment variable
If you later find errors in your `make` logs similar to:
```
Warning: Installation path /Users/YOUR_USERNAME/.local/bin
         not found on the PATH environment variable.
```
1. (MacOS)You may need to add the path listed above to your [`$PATH` environment](https://apple.stackexchange.com/questions/358687/right-way-to-add-paths-to-path-in-mojave/358873#358873).
2. (Linux-Ubuntu)After installing stack, it is necessary to restart the terminal or run ```source ~/.profile```. Also, you can manually add the path listed above to your [`$PATH` environment](https://askubuntu.com/questions/60218/how-to-add-a-directory-to-the-path).

</details>

<details>

<summary><h4>Linux</h4></summary>

For Linux users, please run:
```
curl -sSL https://get.haskellstack.org/ | sh
```
(from [Official Stack Webpage](https://docs.haskellstack.org/en/stable/README/)) inside a terminal window. This will fully install Stack. It might ask you for permissions while installing, this is normal, please give it permission.

##### Confirm Installation
To confirm you've successfully installed stack, you should run ```stack --help``` in your terminal window. If some manual information appears, then you have successfully installed stack! Otherwise, you will need to go through these steps again.

</details>

### Pinning your Stack Version
If you later experience issues with `.cabal` files being frequently rebuilt, you may pin your `stack` version against our LTS's preferred version. In your terminal window, please run `stack upgrade --binary-version CURRENT_STACK_VERSION` (e.g., `2.5.1`) and it should stop frequently rebuilding the `.cabal` files.

## VSCode

VSCode is a code editor with a large repository of plugins that you can use to extend it's functionality. VSCode has support for Haskell via the [`Haskell Language Server`](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) plugin ([github](https://github.com/haskell/haskell-language-server)). If you choose to use VSCode to work with Drasil, you may follow these steps, however, if you prefer to use an alternative IDE, you may want to look for a Haskell Language Server plugin for that IDE for similar support and tooling for Haskell.

### Installation
Please download the binary related to your operating system from the [Official VSCode Downloads webpage](https://code.visualstudio.com/Download). Then please follow the related installation instructions for your operating system.

<details>

<summary><h4>Windows</h4></summary>

Please run the executable from the above download link and follow the on-screen steps. You will need to just accept their EULA. Please leave all of the options along the installation default.

</details>

<details>

<summary><h4>Mac</h4></summary>

Once you've downloaded the zip file from the above download link, please open up the zip file and drag-and-drop the internal `Visual Studio Code.app` file into your `Applications` folder.

</details>

### Extensions
The following list of extensions are recommended and are helpful for working with Drasil specifically.

For each of the following plugins/extensions, the same general installation process is followed:
1. Open up VSCode
2. Press `CTRL+SHIFT+X` to open up the Extensions menu (if nothing opens on the left side of your screen, press `View > Extensions` at the top bar)
3. In the menu created on the left-hand side of your screen, search for the plugin name
4. Click on the target and press "Install". While installing, a little pop up might appear at the bottom right of your screen indicating the progress of the installation (and the installation of the dependencies).

**Recommended Extensions**:
1. [Haskell](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) - Please see the [Haskell support section](#haskell-support).
2. [Language Support (LaTeX, MD, etc)](https://open-vsx.org/extension/valentjn/vscode-ltex)
3. [Git Support](https://marketplace.visualstudio.com/items?itemName=eamodio.gitlens) - It's particularly helpful to see how a specific line of code (not just file!) came to be, and observe the history of the repository (often, this is more useful than using GitHub's interface because GitHub has restrictions on many things -- e.g., '--follow' is never enabled!).

**Helpful Extensions/Configurations (all optional):**
1. [Insert Unicode](https://marketplace.visualstudio.com/items?itemName=brunnerh.insert-unicode) - We often use unicode symbols in Drasil, so having a quick way to search unicode symbols by name is very helpful.
2. [Haskelly](https://marketplace.visualstudio.com/items?itemName=UCL.haskelly) - Some useful GHCi commands for Haskell among other things.
3. [Graphviz support](https://marketplace.visualstudio.com/items?itemName=joaompinto.vscode-graphviz) - Sometimes deal with graphviz commands and .dot files, so the `CTRL+K V` command from here helps to view those files quickly.
4. [Nix Environment Selector](https://marketplace.visualstudio.com/items?itemName=arrterian.nix-env-selector) - Automatically use the Drasil `shell.nix`!
5. If you're interested in using a font that mimics unicode characters for Haskell, consider using the [FiraCode](https://github.com/tonsky/FiraCode) font with ligatures enabled ([installation instructions](https://github.com/tonsky/FiraCode/wiki/VS-Code-Instructions)). It makes reading Haskell more like reading hand-written simple type theory works.
6. [TODO Tree](https://open-vsx.org/extension/Gruntfuggly/todo-tree) - Gather your code's TODO notes together in one nice VSCode panel. You need `ripgrep` installed on your machine for this plugin to work. For making sure TODO notes in Haskell source code are gathered correctly, you might need to add "--" to the regular expression used in searching for "TODO" notes in the TODO Tree extension settings.
7. [Rewrap](https://open-vsx.org/extension/stkb/rewrap) - Hard wrap text in your documents around the 80 character "limit" (configurable).

Despite most of these above being "optional", if you have a powerful computer, it's best to install all of them.

#### Haskell Support

**Note: Using the Haskell Language Server plugin, you should make sure your root folder open in VSCode is `./code/`!**

VSCode with the Haskell Language Server plugin creates a very smooth and powerful IDE for Haskell developers. However, the Haskell Language Server is still fairly new and under heavy development, so there will likely be issues that arise while using it. As Drasil is split into multiple packages (each `drasil-*` is a package), we end up with a multi-root software suite. Unfortunately, we are not able to use HLS efficiently under our multi-root software suite, resulting in an instance of HLS opening for each package. This also results in the lack of updates of cross package changes until we re-open HLS/VSCode. For example, if we update a component of `drasil-lang`, using an HLS tool in any of the packages that rely on `drasil-lang` will not "see" the changes you had already made to `drasil-lang`. This often makes refactoring large things across packages a bit difficult due to the HLS plugin showing you false errors. To resolve this issue, you may either restart your HLS plugin, or restart VSCode entirely.

##### Troubleshooting Haskell Support

You may find that the Haskell extension breaks at times (typically occurs when an update happens, and a file gets corrupted/outdated). The first resolution you should take is to completely clear your `.stack-work` (e.g., in the `code` folder, delete the `.stack-work` folder), and re-compile the entire project and re-open VSCode. If issues persist, it may also be helpful to delete your global Stack cache (on Linux, `~/.stack/`), and continue with another re-compilation of the entire project. If the issue still occurs, you may need to refer to the issue tracker of `haskell-language-server`, or `vscode-haskell`.

## Using the terminal/PowerShell/Cygwin/Git Bash in VSCode
With VSCode open, please press `` CTRL+SHIFT+` `` (or alternatively, press `Terminal > New Terminal`). It will open up a terminal window in the bottom of your screen.

### Using an alternative Shell
If you prefer to use a different shell (e.g., Cygwin or Git Bash instead of PowerShell, or bash instead of zsh) as your default in VSCode, after opening a shell, you may [select an alternative shell environment](https://stackoverflow.com/a/50527994) as your default using the dropdown on the right-hand side of the shell window that opens up.

### General helpful VSCode shortcuts

| Keybinding                   | Description                                         |
| ---------------------------- | --------------------------------------------------- |
| `` CTRL+SHIFT+P ``           | Show the command palette                            |
| `` CTRL+K CTRL+T ``          | Show the lists of themes                            |
| `` CTRL+T ``                 | Finding a class, function, or variable in all files |
| `` CTRL+SHIFT+<Any arrow> `` | Selects text by words in the direction given        |
| `` CTRL+F ``                 | Open search bar                                     |
| `` CTRL+L ``                 | Select the current line in the cursor.              |
| `` CTRL+R ``                 | Reload window                                       |
| `` CTRL+` ``                 | Toggle the terminal window                          |
| `` CTRL+SHIFT+` ``           | Create a new terminal instance                      |
| `` CTRL+P ``                 | Find a specific file in your current folder         |
| `` CTRL+K CTRL+Z ``          | Comment a block of code                             |
| `` CTRL+K CTRL+U ``          | Uncomment a block of code                           |
| `` CTRL+, ``                 | Go to user settings                                 |
| `` CTRL+SHIFT+ENTER ``       | Replace All                                         |
| `` ALT+ENTER ``              | Select all occurrences of Find match                |
| `` CTRL+F2 ``                | Select all occurrences of a word                    |

 

#### Note for MacOS users

Switch out CTRL for CMD if you’re using MacOS.

## LaTeX

LaTeX is a language for typesetting documents, similar to Word and PowerPoint but with much more flexibility. While you might not often handle LaTeX manually while working with Drasil, it may still be a useful piece of software to learn. Feel free to check out some [tutorials](https://latex-tutorial.com/tutorials/), or a [different tutorial with an online editor](https://www.overleaf.com/learn/latex/Learn_LaTeX_in_30_minutes), or even a [table generator](https://www.tablesgenerator.com/#).

[`MiKTeX`](https://miktex.org/) is an easy to use modern TeX distribution and package manager. It allows you to compile your TeX files with it's tools (e.g., `pdflatex`) and download packages and dependencies on-the-fly (from [CTAN](https://ctan.org/?lang=en) & [it's package list](https://miktex.org/packages)). `MiKTeX` is only one of many possible solutions you may use.

### Installation

Please follow the instructions according to your operating system and preferred method of installation.

<details>

<summary><h4>Mac, with homebrew</h4></summary>

For the GUI version of MacTeX, use: `brew install --cask mactex`. For the non-GUI version, use: `brew install --cask mactex-no-gui`. The GUI version is useful if you would for those that need the tex file editor TeXShop. The GUI version also installs LaTeXiT, TeX Live Utility, and BibDesk.

</details>

<details>

<summary><h4>Linux, with your package manager</h4></summary>

*Apt*-based distributions:

```
sudo apt install fonts-lmodern texlive-bibtex-extra texlive-latex-extra texlive-science texlive-xetex texlive-luatex
```

*Nix*:

Add `texlive.combined.scheme-full` to your nix packages.

</details>

<details>

<summary><h4>Windows / Mac / Linux, with <code>MiKTeX</code></h4></summary>

If you're using a MacOS-based computer and prefer to use `homebrew` to install your software packages, please skip to [Mac alternative section](#mac-with-homebrew). Alternatively, if you're using Linux-based computer and prefer to use your native package manager, please skip to the [Linux alternative section](#linux-with-your-package-manager).

Check out the installation documents from [`MiKTeX`](https://miktex.org/download). After installing MiKTeX, we recommend that you open up it's GUI, force update your packages, and manually install the `lm-math` package.

You may also want an IDE, in which case, you can choose from any of your choice VSCode (as installed above), [GNU Kile](https://apps.kde.org/kile/), [Gummi](https://gummi.app/) ([installation](https://github.com/alexandervdm/gummi/wiki/Installing-Gummi#install-for-your-platform)), etc.

#### Common `MiKTeX` troubles

##### `make tex` takes forever!

Running `make tex`, for the first time in particular, will require you to manually accept many pop up installation windows. This is normal. However, please be aware that when `MiKTeX` installs packages, it may take a while if you're using a slow mirror. You can try to change your mirror if you'd like from the `MiKTeX Console`.

##### `make tex` says I'm missing packages!

Please see the discussion from [issue 2347](https://github.com/JacquesCarette/Drasil/issues/2437).

1. Open your MikTeX console
2. Manually check for updates
3. Update all necessary packages
4. Go to the "Packages" tab
5. Search for "lm-" and an `lm-math` item should pop up
6. Install `lm-math`
7. Close all open Cygwin/Git Bash terminals you have, and re-open them
8. `cd Drasil/code`
9. `make tex`

Here's what `lm-math` should look like in your `MiKTeX` console:

![image](https://user-images.githubusercontent.com/1627302/117991427-09c7ca80-b30c-11eb-8c01-807a4f87112e.png)

Hopefully, it should work now, but if it doesn't, please file an issue.

</details>

### Learning Resources

* [A Simple Guide to LaTeX](https://latex-tutorial.com/tutorials/)
* [Overleaf's LaTeX Guide](https://www.overleaf.com/learn/latex/Creating_a_document_in_LaTeX)
* [Overleaf's LaTeX in 30min](https://www.overleaf.com/learn/latex/Learn_LaTeX_in_30_minutes)
* [Creating LaTeX tables](https://www.tablesgenerator.com/)

## Inkscape
We use Inkscape in order to get `svg` diagrams to render in LaTeX. The program itself is actually an `svg` image creator, but Drasil uses it on the command line instead.

### Installation Instructions
Please visit the [Inkscape website](https://inkscape.org/release/inkscape-1.1/) for more detailed instructions. For convenience, we have a summary for Linux, Windows, and Mac below.

<details>

<summary><h4>Linux</h4></summary>

On Ubuntu or Debian, enter these two commands:
```
sudo apt-get update
sudo apt-get install inkscape
```
The first command will ensure that all of the packages are up do date, and the second will fetch and automatically install Inkscape for you.

</details>

<details>

<summary><h4>Windows</h4></summary>

Download the executable installer for Inkscape ([64-bit](https://inkscape.org/release/1.1/windows/64-bit/) or [32-bit](https://inkscape.org/release/1.1/windows/32-bit/)), run the file, and follow the instructions on screen. Make sure you add Inkscape to your `PATH` when the installer asks. If the installer cannot automatically add Inkscape to you `PATH` or if you already have Inkscape without using the `PATH`, please follow [these instructions](https://tex.stackexchange.com/a/523685).

</details>

<details>

<summary><h4>Mac</h4></summary>

Download the [disk image for Inkscape](https://inkscape.org/release/inkscape-1.1/mac-os-x/1010-1015/dl/), run the file, and follow the on-screen instructions. Enabling command line access to inkscape may require adding the path to inkscape to the PATH environment variable.

</details>

## Graphviz
In Drasil, Graphviz is used to render generated `dot` graphs. These are used in creating traceability graphs to parallel the traceability matrices of the generated SRS documents. In addition, they are used to create dependency graphs for Drasil's class, type, and module structure.

### Installation Instructions
Please visit the [Graphviz website](https://graphviz.org/download/) for more detailed instructions. For convenience, we have a summary for some Linux distributions, Windows, and Mac below.

<details>

<summary><h4>Linux</h4></summary>

For Ubuntu and Debian, you can use the following command to install graphviz:
```
sudo apt install graphviz
```

</details>

<details>

<summary><h4>Windows</h4></summary>

Download, run, and follow the on-screen instructions of the [official GraphViz Windows installer](https://graphviz.org/download/#windows).

Alternatively, if you have [`chocolatey`](https://chocolatey.org/install) installed, you can open up a new command line as an administrator and run the following command:
```
choco install graphviz
```

</details>

<details>

<summary><h4>Mac</h4></summary>

If you use [homebrew](https://brew.sh/), you can install via the following command:
```
brew install graphviz
```
Or, you can use [MacPorts](https://www.macports.org/):
```
sudo port install graphviz
```

</details>

## Doxygen

Doxygen is used for generating documentation from code. Detailed download instructions can be found [here](https://www.doxygen.nl/download.html).

<details>

<summary><h4>Linux</h4></summary>

You can install Doxygen on Linux by using the following command:
```
sudo apt-get install doxygen
```

</details>

<details>

<summary><h4>Windows</h4></summary>

The [Doxygen download page](https://www.doxygen.nl/download.html) has a Windows executable download. Open it and follow the on-screen instructions. You will also need to add the Doxygen `bin` to your path:
1. In the search bar at the bottom left of the screen, type in `Edit the system variables`.
2. Click `Environment Variables`
3. In the top list (labelled `User variables`), navigate to the `Path` variable. Click on it and then select `Edit`.
4. Click `New` and add the installation path for Doxygen. By default, this path should be `C:\Program Files\doxygen\bin`
5. Select `OK` and restart any open terminals. Your Doxygen setup should now be complete.

</details>

<details>

<summary><h4>Mac</h4></summary>

The [Doxygen download page](https://www.doxygen.nl/download.html) has a disk image download for Mac OS. Open it and follow the on-screen instructions.

</details>

# Working with Drasil
Congratulations! Now that you've successfully setup your workspace, you should move on to our [Contributer's Guide](https://github.com/JacquesCarette/Drasil/wiki/Contributor's-Guide).