# `drasil-printers` Shallow File Analysis

## `drasil-printers/lib/Language/Drasil/Config.hs`

Contains two kinds of options:

1. Options for the TeX renderer.
2. Options for bibliography rendering (i.e., formats: MLA, APA, Chicago).

On the bibliography formats:

1. MLA and APA are "Works Cited"-style bibliographies (i.e., only showing cited references).
2. Chicago is a true bibliography (showing more than cited references).

So, that is to say we should

- [ ] Allow Chicago formatting to contain an extra list of references that will always be displayed in the bibliography (even if not cited within the document).

However, a question arises: why does `drasil-printers` know about references at all? These should have been rendered earlier (at the level of the semantic document language being rendered into the `LayoutObj` language that `drasil-printers` defines).

- [ ] References and citations should have been "erased" (rendered) earlier.

Finally:

- [ ] All options should be routed through a record.

## `drasil-printers/lib/Language/Drasil/HTML/**.hs`

- `drasil-printers/lib/Language/Drasil/HTML/CSS.hs`
- `drasil-printers/lib/Language/Drasil/HTML/Helpers.hs`
- `drasil-printers/lib/Language/Drasil/HTML/Monad.hs`
- `drasil-printers/lib/Language/Drasil/HTML/Print.hs`

We will ignore these files because they are being rewritten in [#5319](https://github.com/JacquesCarette/Drasil/pull/5319).

## `drasil-printers/lib/Language/Drasil/JSON/**.hs`

This folder does not contain a JSON generator. It contains an IPYNB generator (Jupyter/Interactive Python Notebook generator).

* [ ] Rename this folder to `IPYNB` OR `Jupyter`.

### `drasil-printers/lib/Language/Drasil/JSON/Helpers.hs`

This file contains:

* Two "extras" (functions) common for our needs of `pretty` (i.e., `br` [wrap in braces], `stripnewLine`).
* Duplicated functions from the HTML `Doc` generator.
* The real rendering functions for the Jupyter files!

### `drasil-printers/lib/Language/Drasil/JSON/Print.hs`

Contains the two 'main'/entry rendering functions:

```haskell
-- | Build the general Jupyter Notebook document.
genJupyterLessonPlan :: Document -> PNew.Doc ann
genJupyterLessonPlan (Document t a c) =
  let
    titleCell = markdownCell (vcat [text "# " <> pSpec t, text "## " <> pSpec a])
    cells = JArray $ titleCell : concatMap printLO' c
  in renderJSON pretty $ JObject $ ("cells", cells) : makeMetadata

-- | Build an SRS document in JSON format.
genJupyterSRS :: Document -> PNew.Doc ann
genJupyterSRS (Document t a c) =
  let
    titleCell = markdownCell (vcat [text "# " <> pSpec t, text "## " <> pSpec a])
    cells = JArray $ titleCell : [markdownCell (print c)]
  in renderJSON pretty $ JObject $ ("cells", cells) : makeMetadata
```

It is a massive problem that `drasil-printers` is aware of the existence of `drasil-srs` and `drasil-lesson-plan`. The difference between the two is that _effectively_ between using `printLO` and `printLO'`:

```haskell
-- | Helper for rendering LayoutObjects into JSON
-- printLO is used for generating SRS
printLO :: LayoutObj -> Doc
printLO (Cell _)                         = empty
printLO (HDiv _ layoutObs _)             = vcat (map printLO layoutObs)

-- printLO' is used for generating general notebook (lesson plans)
printLO' :: LayoutObj -> [JSON]
printLO' (Header n contents l)            = [markdownCell $ (h (n + 1) <> pSpec contents) $$ refID (pSpec l)]
printLO' (Cell layoutObs)                 = concatMap printLO' layoutObs
printLO' HDiv{}                           = mempty
```

`printLO'` encourages splitting into multiple cells while `printLO` does not. This system is known to be in need of a design ([#2346](https://github.com/JacquesCarette/Drasil/issues/2346)). In the interim, a good step forward is to:

* [ ] Drop `genJupyterSRS` in favour of `genJupyterLessonPlan`, letting the SRS be generated across many Jupyter cells rather than one giant cell. This would require at least the following modification as well:
    ```haskell
    printLO' HDiv{}                           = mempty
    -- ^ This line would need to be:
    printLO' (HDiv _ layoutObs _)             = concatMap printLO' layoutObs
    ```
    `printLO' Definition{}                     = []` would also need to be fixed.
* [ ] Re-evaluate the constructors for `LayoutObj` to learn more about _how_ and _where_ the final renderer can really infer when things should be `LayoutObj`s should be split into cells or merged together as one.

## `drasil-printers/lib/Language/Drasil/Markdown/**.hs`

This folder does not contain a complete `Markdown` generator/renderer. It contains an `mdBook` generator.

* [ ] Rename this folder to `MdBook`.

### `drasil-printers/lib/Language/Drasil/Markdown/Helpers.hs`

Contains actual helper functions for Markdown and HTML printing!

This file contains functions that really ought to be exposed options. For example:

```haskell
bold :: Doc -> Doc
bold t = ast <> ast <> t <> ast <> ast
```

Note: `ast = text "*"`.

`bold` can be written with either two underscores or asterisks. We currently support a few things through `BibFormatter`, but we should add more, such as for bold.

### `drasil-printers/lib/Language/Drasil/Markdown/Print.hs`

Contains the main `mdBook` generator. It requires some hacking (re-interpreting) of a single `Document` to split it up into multiple files through `Language.Drasil.Printing.Import.Document`'s `makeProject` function.

One major issue that the `mdBook` generator brings up: `Reference`s! As it stands, we have chunks that know their internal labels to be used in the document generator. The document generator uses said labels. The problem with that is that those labels don't know which label they will be placed in. Previously, there was a key assumption that all chunks would be rendered in the same file. This assumption is obviously bad in the `mdBook` generator.

## `drasil-printers/lib/Language/Drasil/Plain/Print.hs`

This file is exclusively used in `drasil-code`. It contains renderers for `Sentence`, units (`USymb`), `Symbol`, `Expr`, and `CodeExpr`. This file contains the renderers that prints these aforementioned data types in a comment-friendly and variable-name-friendly style for the code renderer.

* [ ] A better name for this folder would be "PlainText" or "CodeFriendlyText."
* [ ] This file is the only file for which `drasil-code` is dependant on `drasil-printers` for. If we moved this file to `drasil-code`, then we should also be able to move the files in the `Drasil.Code` namespace but living in `drasil-lang` to `drasil-code`. I don't think this is a bad idea necessarily. Since this printer relies on the printing language ASTs that live in `drasil-printers`, we would still need `drasil-code` to depend on `drasil-printers`, but at least `drasil-printers` would not depend on the `Drasil.Code`-related things. This is a net-good.

## `drasil-printers/lib/Language/Drasil/Printers.hs`

This package revolves around developing these two kinds of 'renderers':

1. Building a generic typesetting/layout program: `DL.Document -> DP.Document` & `DL.Document -> DP.Project` renderers (`makeDocument` and `makeProject`).
2. Typesetting in a real language: `DP.Document -> Doc` renderers (`genHTML`, `genJupyterX`, `genTeX`, etc.).

The first is about generating a general typesetting/layout program. This "program" (which is defined by `[LayoutObj]`) provides the second kind of renderers with information about how and where things should be placed. The benefit of this is that we can erase all chunks _first_ before the final rendering. This simplifies work necessary for the latter renderers, else there would be considerable duplicate code and finicky maintenance.

This file is a reexport file for the various renderers as well as for the layout program generation functions: `makeDocument` and `makeProject`.

## `drasil-printers/lib/Language/Drasil/Printing/**.hs`

### `drasil-printers/lib/Language/Drasil/Printing/AST.hs`

This file contains _some_ of the ASTs core to `drasil-printers`. However, it does not contain the "top-level" AST that packages really rely on `drasil-printers` for `LayoutObj`. These ASTs are secondary to `LayoutObj`.

### `drasil-printers/lib/Language/Drasil/Printing/Citation.hs`

Contains an AST for "citations:"

```haskell
-- | A citation contains an entry id, the kind of citation, and the appropriate citation fields.
data Citation = Cite EntryID CitationKind [CiteField]
```

See discussion in the recent [Analysis of `Reference`s](./07-15-2026-References.md) about terminology.

However, by now, `Citation`s should have been erased (as is also mentioned in `LayoutObj.hs`)! Why? Because we currently have the HTML generator define its own `Citation` renderer which the mdBook and Jupyter generators reuse. That's somewhat good, but also bad because the mdBook and Jupyter generators should be generating Markdown code that produces the same result. How could that have happened? That would have happened if the `Citation`s were written to `Spec`s and then translated into the output formats (HTML/Markdown). This would also benefit the TeX renderer as an option. For example, if we wanted to generate bibliographies/works cited page that _we_ formatted using a custom format ill-supported by `natbib` or `biblatex`, we could! Plus, this is just another generation option.

So why does this issue occur and how can we fix it? Well, I partially explained the latter above but we need to make it more concrete.

At its root, the problem appears to exist because of TeX rendering and weak support for generating multi-file projects. TeX rendering wants us to generate `.bib` files. We don't do that. Instead, we generate embedded `\begin{filecontents*}{bibfile.bib}` files (which works but is unconventional, definitely should be a generation option). The problem is that we do this "too late." The production of the `.bib` contents should have happened without the TeX renderer. The TeX renderer should have only accepted the `.bib` file contents and used it in the body of `\begin{filecontents*}{bibfile.bib}`. So, we need four things:

1. Improved support for multi-file generation, and then to pass along the name of the generated `.bib` file.
2. To separate the `.bib` file generator from the TeX renderer and to pass along the `.bib` `Doc` contents to the TeX renderer if "embedding the `.bib` file contents" is an option selected in the generator.
3. To create a `Citation -> Spec` printer.
4. To open an option for generating using (1), (2), and (3).

### `drasil-printers/lib/Language/Drasil/Printing/Helpers.hs`

Contains a number of various "helper" functions. This file primarily sits atop `Text.PrettyPrint`.

Idea: create `drasil-prettyprinter` that exports various tools that `drasil-gool` and `drasil-printers` could both benefit from. Perhaps also reexporting the entire `Prettyprinter` library at the same time if we feel like we can add enough to the core library.

### `drasil-printers/lib/Language/Drasil/Printing/LayoutObj.hs`

Contains a lot more than `LayoutObj`:

```haskell
-- | A document must contain a title, author, and contents (as 'LayoutObj's).
data Document = Document Title Author [LayoutObj]
-- | A Project must contain a title, author, RefMap, and Files.
data Project  = Project Title Author RefMap [File]
-- | A File must contain a title, filename, depth, and contents (as 'LayoutObj's).
data File     = File Title Filename Depth [LayoutObj]
-- | An author is just a sentence ('Spec').
type Author   = Spec
-- | Contents are just a sentence ('Spec').
type Contents = Spec
-- | A group of layout objects.
type Items    = [LayoutObj]
-- | Tags.
type Tags     = [String]
-- | Depth of a header.
type Depth    = Int
-- | Horizontal dimension of a graph.
type Width    = Float
-- | Vertical dimension of a graph.
type Height   = Float
-- | Holds a file path.
type Filepath = String
-- | Holds a file name.
type Filename = String
-- | A caption is just a sentence ('Spec').
type Caption  = Spec
-- | A mapping of refs to the file that contains them.
type RefMap   = Map String Filename

data LayoutObj =
     Table Tags [[Spec]] Label Bool Caption                          -- ^ Holds all information needed for a table.
   | Header Depth Title Label                                        -- ^ Holds all information needed for a header.
   | Paragraph Contents                                              -- ^ Paragraph.
   | EqnBlock Contents                                               -- ^ Equation block.
   | Definition [(String,[LayoutObj])] Label                         -- ^ Definition. Holds the type, contents, and a label.
   | List ListType                                                   -- ^ List.
   | Figure Label (Maybe Caption) Filepath MaxWidthPercent           -- ^ Holds all information needed for a figure.
   | Graph [(Spec, Spec)] (Maybe Width) (Maybe Height) Caption Label -- ^ Holds all information needed for a graph.
   | CodeBlock Contents                                              -- ^ Code block.
   | HDiv Tags [LayoutObj] Label                                     -- ^ Holds tags, more contents, and a label.
   | Cell [LayoutObj]
   -- this shouldn't be here, it should have been expanded.
   | Bib BibRef                                                      -- ^ Bibliography section.
```

There is no clear reason why the majority of these exist in this file.

* [ ] The "large" ones (`Document`, `Project`, `File`) should be moved to another file. However we should dig deeper and see if these _should_ be necessary. They have a code smell to them in this package.
* [ ] `Author` should be removed. Definitely information that should have been erased earlier.
* [ ] `RefMap` appears to only be used in the mdBook generator. It should be moved there because the "reference" system also needs to be rebuilt and we don't want to encourage reusing this type.
* [ ] `HDiv` should be investigated. It does not look like a good constructor in a generic object layout language.

### `drasil-printers/lib/Language/Drasil/Printing/PrintingInformation.hs`

The file contains the following:

```haskell
-- | Notation can be scientific or for engineering.
data Notation = Scientific
              | Engineering

-- | Printing information contains a database, a stage, and a printing configuration.
data PrintingInformation =
  PI { _sysdb :: ChunkDB
     , _stg :: Stage
     , _notation :: Notation
     }
makeLenses ''PrintingInformation

-- | Builds a document's printing information based on the system information.
piSys :: ChunkDB -> Stage -> Notation -> PrintingInformation
piSys = PI
```

* [ ] We should remember to go classy-lenses on various "options" records as well. That _can_ go a long way when we start nesting them in each other.

A major issue with `PrintingInformation` is that it solely makes sense in the context of generating the `drasil-printers` ASTs, erasing chunks (hence the `ChunkDB`), but in particular for the purpose of typesetting (1) mathematical expressions or (2) sentences (hence the `Notation` and `Stage`).

### `drasil-printers/lib/Language/Drasil/Printing/Import/**.hs`

A folder that contains the "renderers" from the types in `drasil-lang` into the various "atom" types in `drasil-printers`: `Spec`, `Expr`, `Citation` as well as to the main layout programs: `Document` and `Project`.

#### `drasil-printers/lib/Language/Drasil/Printing/Import/Citation.hs`

Contains the following and the definition of `layField`:

```haskell
-- | For importing a bibliography.
layCite :: Citation -> P.Citation
layCite c = P.Cite (showUID c) (c ^. citeKind) (map layField (c ^. getFields))
```

I won't bother to comment on `Citation`s as I've already discussed it earlier.

#### `drasil-printers/lib/Language/Drasil/Printing/Import/CodeExpr.hs`

Contains the renderer for the `CodeExpr` language to the printing `Expr` language.

#### `drasil-printers/lib/Language/Drasil/Printing/Import/Document.hs`

Contains the definitions of the two main "layout program" generators: `makeDocument` and `makeProject`.

#### `drasil-printers/lib/Language/Drasil/Printing/Import/Expr.hs`

Contains the renderer for the mathematical `Expr` language to the printing `Expr` expression language.

What this and `../Import/CodeExpr.hs` tell me is that we might need a different design for `CodeExpr`, `Expr`, and `ModelExpr` that instead relies on de-embedding more things in `Expr`. See [Rendering chunks in Drasil and the role of Haskell](https://github.com/DrasilOrg/Notes/blob/ghrcCodeVariants/projects/Re-architecting%20drasil-code%20for%20new%20generation%20options%202025/notes/RENDERING-IN-DRASIL.md) for more relevant information. We should take the "de-embedding `sin`" exercise seriously.

#### `drasil-printers/lib/Language/Drasil/Printing/Import/Helpers.hs`

Breaking this file up in [#5374](https://github.com/JacquesCarette/Drasil/pull/5374).

#### `drasil-printers/lib/Language/Drasil/Printing/Import/Literal.hs`

Contains the renderer for the mathematical `Literal` language to the printing `Expr` expression language.

#### `drasil-printers/lib/Language/Drasil/Printing/Import/ModelExpr.hs`

Contains the renderer for the mathematical `ModelExpr` language to the printing `Expr` expression language.

#### `drasil-printers/lib/Language/Drasil/Printing/Import/Sentence.hs`

Contains the renderer for the natural language encoding (`Sentence`) to the printing sentence language.

* [ ] The printing sentence language is called `Spec`, but I don't immediately understand what `Spec` means. Renaming to something more like `Inline` (like Pandoc) or just plain `Sentence` would be good.
* [ ] The `Spec` language does not support some changing fonts (bold, italics, face, etc.). If we wanted to "erase" `Citation` earlier, then having these would be necessary.

#### `drasil-printers/lib/Language/Drasil/Printing/Import/Space.hs`

Contains a renderer for the mathematical type language (`Space`) to the printing `Expr` language.

This code is only used in the `ModelExpr` renderer for when rendering the `Spc` constructor. However, the code contains:

```haskell
space _  Integer        = P.MO P.Integer
space _  Rational       = P.MO P.Rational
space _  Real           = P.MO P.Real
...
space _  Char           = P.Ident "Char"
space _  String         = P.Ident "String"
...
space _  (Actor s)      = P.Ident s
...
space sm (Function i t) = P.Row $
  intersperse (P.MO P.Cross) (map (space sm) $ toList i) ++  -- AxBxC...xY
  [P.MO P.RArrow, space sm t]                                -- -> Z
```

Now, a few questions:

1. Why are we not erasing `Integer` and using unicode? I suppose this is because of TeX, but we should have this option.
2. Why are we rendering `Char` and `String` to `Char` and `Strong`? I would expect something more like `\mathbb{A}` and `\mathbb{T}` (or some other convention we establish).
3. `Actor`: does not belong here. We need to split `Space`.
4. `Function`: It looks like we ought to have a product type.

What this really makes me think about is `D-Q-D-`s and their relationship to `ConceptChunk`s. We currently link them together and say "this quantity is a variable (instance) of this conceptual quantity." Consider this: replacing `ConceptChunk` with a `ConceptualValue` where `ConceptualValue` is similar to a `ConceptChunk` but also has _type_ information (which would include _dimension_ information instead of specific units).

#### `drasil-printers/lib/Language/Drasil/Printing/Import/Symbol.hs`

Contains a renderer for the `Symbol` language to printing `Expr` language.

## `drasil-printers/lib/Language/Drasil/TeX/**.hs`

Contains the `TeX` printer for the core printing language.

### `drasil-printers/lib/Language/Drasil/TeX/Helpers.hs`

Contains a mixed bag of `TeX`-related `Doc`-targeting constructors.

* [ ] As of now, I realize there is considerable duplicate code within `drasil-printers`. A major issue is the fact that we don't have an intermediate representation for `LaTeX`. We should be able to share more code if we had so. Furthermore, the idea of a `drasil-prettyprinter` looks like an increasingly better idea.
* [ ] Contains consider duplicate code within the same file.
* [ ] `mkEnv`, `mkEnvArgBr`, and `mkEnvArgSq` can be deduplicated.
* [ ] This file has a lot of good code, but "Helpers" makes it sounds like it's nonessential code, but it contains all the important smart constructors.

### `drasil-printers/lib/Language/Drasil/TeX/Monad.hs`

Contains the printing monad used for switching back and forth between math and text mode in generated LaTeX.

### `drasil-printers/lib/Language/Drasil/TeX/Preamble.hs`

Contains code for declaring and determining which packages a to-be-generated LaTeX will need for it to be compilable. I wonder if this can be added instead to the printing monad, logging which things were used in a set and post-facto determining which packages were assumed existent in the rendered code.

### `drasil-printers/lib/Language/Drasil/TeX/Print.hs`

Contains the final rendering code for the document layout language (`LayoutObj`, `Spec`, etc.) to `Doc`. Also contains a `.bib` renderer.
