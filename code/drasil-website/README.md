# Drasil Website Package

This package contains all of the necessary code to generate the [Drasil website](https://jacquescarette.github.io/Drasil/).

Last updated: August 12, 2021

------------------------------------------------

### Summary of Folder Structure and File Contents

The layout of this folder is similar to that of the drasil-example folder. However, instead of individual examples, our only 'document' that will be generated from this package is the Drasil website.

**app**
- Contains the `Main.hs` file that runs the Drasil website. The main function within also takes in many environment variables (see `make website` in `../Makefile` for more details).

**lib**
- Contains all of the individual section files to create the Drasil website. Each section is gathered into `Body.hs`, then sent off to `Main.hs` in `app` to be printed. Please see the website package's [Haddock documentation](https://ant13731.github.io/Drasil/docs/full/drasil-website-0.1.0.0/index.html) for more details.

**WebInfo**
- Contains the images, `css` file, and example descriptions used in the Drasil website. However, both the `css` and `descriptions` folders are currently unused and are leftover from the previous generation of the website. Instead, the Drasil generators create the required `css` files and the descriptions of each example can be found in `lib/Drasil/Website/Example.hs`.

README.md
  - This file