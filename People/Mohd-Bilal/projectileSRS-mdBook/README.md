# Software Requirements Specification (SRS) Demo using mdBook

This project represents a manually crafted Software Requirements Specification (SRS), utilizing mdBook, for the [Projectile](https://jacquescarette.github.io/Drasil/examples/projectile/SRS/srs/Projectile_SRS.html)
case study within the [Drasil](https://github.com/JacquesCarette/Drasil) framework. It is intended to serve as a foundational reference for future Drasil development, guiding the generation of a new SRS artifact format.

## Installation

Please follow the installation instructions in the [mdBook Documentation](https://rust-lang.github.io/mdBook/guide/installation.html).

## Demo

1. **Clone the Repository**: Clone this repository to your local machine.
2. **Launch the Server**: Open a command line interface in the cloned repository's directory and run `mdbook serve --open`.
3. **Acces the Local Server**: The terminal will display the `localhost` server address.

## mdBook Quirks

- Have to wrap certain elements in `<div id=""></div>` to be able to be referenced.
- Cannot add captions to tables and images. Have to use `<p align="center">CAPTION</p>`.
- Mathjax LaTeX equations do not wrap similar to LaTeX (`$EQUATION$`) or HTML (`\(EQUATION\)`). mdBook syntax is `\\(EQUATION\\)` for inline and `\\[EQUATION\\]` for block equations.
- Some elements of equations require an extra `\`. Ex. new line is `\\\`.
- `\symbf` does not work in Mathjax LaTeX equations, have to use `\boldsymbol` instead.
- Typical list syntax does not work inside tables in Markdown. Have to wrap in `<ul> <li>item1</li> <li>item2</li> </ul>`.
- Accessible assets (Ex. images) need to be inside the `src` folder in order to be rendered. NOTE: There may be a way to use assets outside `src` through the `book.toml`
- Sometimes there is very little space or no space at all between components. Added `</br>` to add extra space.
- Need to add mathjax support in the `book.toml`. Not added by default.

## Other Notes

- Having a page for Table of Contents may be redundant as mdBook automatically creates one for you.
- The HTML SRS for variables or short equations often uses HTML syntax, rather than LaTeX. For example, `<em></em>` for italicizing single variables. This creates slight differences in appearance within the SRS. The mdBook demo uses LaTeX for everything.
- Each section and subsection has its own page. As a result, some pages have very little content.
- Can format code for tables in two ways. Both formats look the same when rendered.
  - Visually readable code but longer
    https://github.com/BilalM04/projectileSRS-mdbook/blob/9cc1553a9c9f95de1735dcc81c5f3285f6c073f2/src/table-of-units.md?plain=1#L6-L12
  - Less readable code but shorter
    https://github.com/BilalM04/projectileSRS-mdbook/blob/9cc1553a9c9f95de1735dcc81c5f3285f6c073f2/src/auxiliary-constants.md?plain=1#L4-L10
