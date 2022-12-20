# Jason's Master's Thesis

## System Requirements

Please run `make system_requirements` for an up-to-date list of system
requirements for building the report.

## Build

Before building, please ensure that you have the related [system
requirements](#system-requirements) installed. Everything related to building
the thesis report is contained in the `Makefile`. To build it, you can run `make
build` in your shell, and it will create a `build/` folder locally, containing
the desired final `thesis.pdf` file.

If you're building for printing, please edit the `compilingforprinting` option
in the `manifest.tex` configuration file and rebuild the document.

### Debugging

`make build` runs so fast (because it runs without interactivity) that we might
not be able to find errors with our documents. To run the TeX build
interactively, please run `make debug` instead of `make build` when building
your thesis.

## Configuration

Please view `manifest.tex` for information about configuring the build.

## Source Code Organization

Personally, I tend to prefer “just text” in my TeX files, importing cluttering
code (such as that in tables, figures, etc.) from external files. This helps me
quickly find and edit my assets. However, you should not restrict yourself to my
preferences. Please feel free to use my general schema, or not. In particular,
I've divided up my chapters into a separate file for each, my front matter into
separate files, and each of my “assets” into a separate folder and file as
required.

`thesis.tex` is the main TeX file, importing all other things. In particular, it
imports:
* metadata from `manifest.tex`,
* bib references from `references.bib`,
* external assets (such as images, figures, tables) from `assets.tex`,
* chapter information from `front.tex`, `chapters.tex`, and `back.tex`, all
  respectively for the front, main, and back matters,
* helpful macros you might build for yourself, or that I found particularly,
  useful when building my thesis, from `macros.tex`, and
* the McMaster University colour swatch from `mcmaster_colours.tex`.

You should be mindful of all of these above listed files. Each one will have its
own discussion of its usage in its respective header, but you are free to ignore
the general schema I've built, or to modify it to your liking.

## Citation

If you need to cite this work, please cite using the following BibTeX:
```
@mastersthesis{Balaci2022MSc,
  author = {Balaci, Jason},
  school = {McMaster University},
  title  = {Adding Types and Theory Kinds to Drasil},
  year   = {2022}
}
```
