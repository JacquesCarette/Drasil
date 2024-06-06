To get started with Drasil, try running `make`. You can also run `make help` in `Drasil/code` to get a list of available commands, as well as some starting instructions. For convenience, we will list more Makefile information below, however it may not be as up-to-date as `make help`.

# Make and Drasil Packages
The most commonly used targets are listed here:
- `make help` will show you a list of available commands. Alternatively, if your terminal shell supports tab-complete, you should also be able to type a few characters towards a target and hit TAB to see a list of available targets that start with what you wrote.
- `make` runs `make test`. Generates each example and tests against a stable version of the generated artifacts.
  - `make tracegraphs` performs `make test` and then generates SVG, PNG, and PDF representations of the traceability graphs for viewing in the generated SRS documents.
- `make hlint`/`make hot_hlint` tests the linter against your codebase, checking for code that can be cleaned up.
- `make pr_ready` runs `make all` and `make hot_hlint`, testing your artifact diffs and checking lint tests. If it runs successfully, then your should be a bit closer to being "Pull Request Ready" (up to your judgment).
- `make clean` allows for make to be run from scratch (by running `stack clean` in all the directories)
- `make cleanArtifacts` (or `make clean_artifacts`) will clean your workspace, deleting all generated artifacts
- `make stabilize` lets you rapidly copy over any stable-breaking changes from the `build/` folder (`make <example>_stabilize` will allow you to do this for a specific example too). See this [Wiki article](https://github.com/JacquesCarette/Drasil/wiki/Workflow#updating-stable-folder-files) for more details.
- `make docs` builds the Haddock documentation for the full Drasil suite. Notably, it creates 2 folders `code/docs` and `code/docs/full`, respectively containing the [external Haddock documentation](https://jacquescarette.github.io/Drasil/docs/index.html) and [internal Haddock documentation](https://jacquescarette.github.io/Drasil/docs/full/index.html) (with all modules fully exposed). If you would like to preview what your documentation changes look like, you should open up the related `code/docs/index.html` or `code/docs/full/index.html` file in your web browser and navigate to the areas you're interested in.
- `make website` - "Lightweight" local deployment of the website. It does not create all required artifacts, only the website.
- `make deploy` - "Complete" local deployment of the website, compiling TeX, code files, and everything else too!
