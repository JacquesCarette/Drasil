There are a lot of folders in Drasil, and it can be especially confusing to a first-time contributor which folders we should change, and which folders are generated artifacts. This article will only document the subfolders within `Drasil/code`.

## Hand-made Folders
| Folder | Description |
| --- | --- |
| `datafiles`        | Contains helper files for each example, including system figures or images, graphs, sample inputs, and object-oriented code libraries for use with GOOL. |
| `drasil-build`     | Outlines basic types and functions needed to generate Makefiles and compile generated code.|
| `drasil-code`      | Holds the language for types, classes, and functions related to generating code. |
| `drasil-code-base` | Proxy package between `drasil-printers` and `drasil-code`, for the bits of code in `drasil-printers` that relies on bits of `drasil-code` (`CodeExpr` specifically). |
| `drasil-data`      | Fundamental knowledge that is shareable amongst many pieces of software. Includes common concepts in mathematics, physics, and computer science. |
| `drasil-database`  | Database aspects of information collection. Contains the `ChunkDB` and `SystemInformation` types which are central to document generation. |
| `drasil-docLang`   | Drasil language related to generated document structure. |
| `drasil-example`   | Examples of Drasil use. Generates code, SRS documents, and Traceability Graphs. |
| `drasil-gen`       | For generating all artifacts. |
| `drasil-gool`      | Renderer for generating code in object oriented languages. |
| `drasil-lang`      | Representation language for Drasil, often with Chunks. |
| `drasil-metadata`  | Metadata used throughout Drasil. Currently just domains. |
| `drasil-printers`  | Document renderer. Can print for `.dot`, `.tex`, `.html`, and `.md` files |
| `drasil-theory`    | General requirements knowledge for generating SRS with Definitions and Models |
| `drasil-utils`     | Various utility functions. |
| `drasil-website`   | Website generator for Drasil. |
| `scripts`          | General scripts used to help compile and analyze the Drasil framework. |
| `stable`           | Contains stable versions of all generated example SRSs and code. Changes should only be made to this folder if both the generated versions contain the change and the new change is correct. Essentially, changes should only be made by using `make stabilize` or by copying and pasting complete files from the generated `build` folder to this one. Otherwise, **manually changing something here will break the Continuous Integration**. |
| `Makefile` and other files | Holds information to compile Drasil and  |

Any of the above files/folders can be changed manually except for `stable`. For more information about `stable`, see the [Workflow wiki](Workflow#updating-stable-folder). All of the `drasil-` prefixed folders are the source code for generating files. Any changes meant to fix an issue with the generated SRS or code artifacts should be made there.

## Generated Folders
| Folder | Description |
| --- | --- |
| `analysis` | Generates dot graphs and a data table to analyze the Drasil framework. |
| `build` | Examples and example code generates here. Running `make tex` will also put the fully rendered PDFs in this folder. |
| `debug` | Automatic logs generate here from the examples with a list of all UIDs and some other knowledge that may be helpful in debugging.|
| `deploy` | Gathers all files needed to test deployment of the Drasil website locally. These include `analysis`, some parts of `build`, `docs`, `graphs`, `traceygraphs`, and `website |
| `docs` | Generates the Haddock documentation for Drasil in two variants: one with fully exposed modules and the other with some hidden modules. If you can't find what you are looking for in the normal version, take a look at the full version (under the `full` folder within `docs`).|
| `graphs` | Holds module dependency graphs. See `make graphs` for more details. |
| `logs`  | Automatic tests compare the contents of `build` against those in `stable`. The `diff` logs are then placed here. |
| `traceygraphs` | Generates dot graphs to mirror the traceability matrices found on each SRS. |
| `website` | Holds the HTML files needed to produce the website.|

These folders shouldn't be changed manually, as any changes will be overwritten by the `make` targets.

**Note:** `cabal` files should not be manually changed, only use `package.yaml` for configuration settings.
