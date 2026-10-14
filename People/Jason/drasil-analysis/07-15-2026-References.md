# Analysis of `Reference`s

## What is a `Reference` (In Drasil)?

In Drasil, a `Reference` corresponds to one of three distinct concepts (which are captured with `LbLType` constructors):

1. Document-internal _cross-references_ (`RP`), e.g., things that go in `\ref` and `<a href="#..">`.
2. Bibliographic _citations_, e.g., `\cite` keys (which are not all that different from (1)!).
3. External hyperlinks_, e.g., `../../../Image.png`.

We use `Reference`s in two ways:

1. As a chunk at times.
2. As a packet of information we pull out from other, "real" chunks that are meant to be rendered in a document and contain their own local-document-referrable labels.

The first way is to encode `URI`s, which we also (1) cheat with (encoding invalid file and http URIs) and (2) insert into the `ChunkDB`. The second way is _sometimes_ inserted into the `ChunkDB` in place of the actual chunks themselves. This second part is particularly bad. The original goal behind [5325](https://github.com/JacquesCarette/Drasil/pull/5325) was to fix just that. However, along the way I realize our terminology is rather confusing and mixed up across our documentation, code, and generated artifacts.

## Terminology

First, let's establish a baseline terminology for our academic-writing context (as _I_ understand it, we can revise to one _we_ agree on):

* **Source:** An _external_ document that contains well-argued claims we may make further arguments based on.
* **Reference:** An _instructive textual description_ that informs the reader _how to access a source_.
* **Citation:** An inline marker (often a number or abbreviation, placed in a box or otherwise) that indicates the sentence containing the marker is _supported/assuming_ knowledge from an _external_ source. A citation _refers the reader to a reference_. One may think of a citation is an abbreviated reference or as a short reference (not readily usable) to a long reference (readily usable) for finding how to access a source.
* **Cross-Reference:** An inline marker (often a number or abbreviation, placed in a box or otherwise) that _directs_ the reader's attention to another part of the _same document_. The reason to direct the reader's attention is not always to persuade the reader of something, it can be purely for directing them to data, for example.
* **Works Cited:** An enumeration of references _cited_ within the document. The emphasis here is on "cited" because a "Works Cited" is not allowed to contain extra, indirectly relevant entries.
* **Bibliography:** An enumeration of references _used in the production_ of a document. This includes both works cited and references to background source materials the reader would likely be interested in as well.
* **External Link:** A URI.

Reminder: these are the definitions as I understand them.

## Terminology We Use

First, `Citation`, for which we have two definitions:

[`drasil-lang/lib/Language/Drasil/Chunk/Citation.hs`](https://github.com/JacquesCarette/Drasil/blob/c4ecd3f18d0995e1fbb8c530cfc76e3102f8fd7e/code/drasil-lang/lib/Language/Drasil/Chunk/Citation.hs#L30-L48):
```haskell
-- | A list of 'Citation's.
type BibRef = [Citation]
-- | A 'String' that should contain no spaces.
type EntryID = String

-- | All citations require a unique identifier used by the Drasil chunk.
-- We will re-use the 'UID' part as an EntryID ('String') used for creating reference links.
-- Finally we will have the reference information ('CitationKind', 'CiteField's, and a 'ShortName').
--
-- Ex. A reference to a thesis paper like Koothoor's "Document driven approach to certifying
-- scientific computing software" would include the affiliated university, publishing year, and city.
data Citation = Cite
  { _citeKind :: CitationKind
  , _fields   :: [CiteField]
  , _citeID   :: UID
  ,  sn       :: ShortName
  }
declareHasChunkRefs ''Citation
makeLenses ''Citation
```

Where `CiteField` and `CitationKind` are defined separately:

[`drasil-lang/lib/Language/Drasil/Data/Citation.hs`](https://github.com/JacquesCarette/Drasil/blob/c4ecd3f18d0995e1fbb8c530cfc76e3102f8fd7e/code/drasil-lang/lib/Language/Drasil/Data/Citation.hs#L33-L79):
```haskell
-- | Fields used in citations.
data CiteField = Address      String
               | Author       People
               | BookTitle    String -- ^ Used for 'InCollection' references only.
               | Chapter      Int
               | Edition      Int
               | Editor       People
               | HowPublished HP     -- ^ Can be published via URL or something else.
               | Institution  String
               | Journal      String
               | Month        Month
               | Note         String
               | Number       Int
               | Organization String
               | Pages        [Int] -- ^ Range of pages (ex1. 1-32; ex2. 7,31,52-55).
               | Publisher    String
               | School       String
               | Series       String
               | Title        String
               | Type         String -- ^ BibTeX "type" field.
               | Volume       Int
               | Year         Int

-- | 'Citation's should have a fields ('CiteField').
class HasFields c where
  -- | Provides a 'Lens' to 'CiteField's.
  getFields :: Lens' c [CiteField]

-- | How something is published. Necessary for URLs to work properly.
data HP = URL String
        | Verb String

-- | External references come in many flavours. Articles, Books, etc.
-- (we are using the types available in Bibtex).
data CitationKind = Article
                  | Book
                  | Booklet
                  | InBook
                  | InCollection
                  | InProceedings
                  | Manual
                  | MThesis
                  | Misc
                  | PhDThesis
                  | Proceedings
                  | TechReport
                  | Unpublished
```

And then we have our second definition, [`drasil-printers/lib/Language/Drasil/Printing/Citation.hs`](https://github.com/JacquesCarette/Drasil/blob/main/code/drasil-printers/lib/Language/Drasil/Printing/Citation.hs#L11-L42)
```haskell
-- | A collection of citations.
type BibRef = [Citation]

-- | A citation contains an entry id, the kind of citation, and the appropriate citation fields.
data Citation = Cite EntryID CitationKind [CiteField]

-- | Fields used in citations. More suitable to printing
data CiteField = Address      Spec
               | Author       People
               | BookTitle    Spec -- ^ Used for 'InCollection' references only.
               | Chapter      Int
               | Edition      Int
               | Editor       People
               | HowPublished HP
               | Institution  Spec
               | Journal      Spec
               | Month        Month
               | Note         Spec
               | Number       Int
               | Organization Spec
               | Pages        [Int] -- ^ Range of pages (ex1. 1-32; ex2. 7,31,52-55)
               | Publisher    Spec
               | School       Spec
               | Series       Spec
               | Title        Spec
               | Type         Spec -- ^ BibTeX "type" field
               | Volume       Int
               | Year         Int

-- | How something is published. Necessary for URLs to work properly.
data HP = URL Spec
        | Verb Spec
```

Note that we also have two different kinds of "Bibliography Reference" types (`BibRef`), specialized per-`Citation`-type:

```haskell
-- | A collection of citations.
type BibRef = [Citation]
```

And then we have a `Reference` type, [`drasil-lang/lib/Language/Drasil/Document/Reference.hs`](https://github.com/JacquesCarette/Drasil/blob/c4ecd3f18d0995e1fbb8c530cfc76e3102f8fd7e/code/drasil-lang/lib/Language/Drasil/Document/Reference.hs#L20-L31):
```haskell
-- | A Reference contains the identifier ('UID'), a reference address ('LblType'),
-- a human-readable shortname ('ShortName'), and any extra information about the reference ('RefInfo').
data Reference = Reference
  { _ui :: UID
  ,  ra :: LblType
  ,  sn :: ShortName}
makeLenses ''Reference

-- | A class that contains a list of 'Reference's.
class HasReference c where
  -- | Provides a 'Lens' to the 'Reference's.
  getReferences :: Lens' c [Reference]
```

And we also have a `Ref :: Sentence` data constructor, [`drasil-lang/lib/Language/Drasil/Sentence.hs`](https://github.com/JacquesCarette/Drasil/blob/c4ecd3f18d0995e1fbb8c530cfc76e3102f8fd7e/code/drasil-lang/lib/Language/Drasil/Sentence.hs#L78-L79):
```haskell
-- | Takes a 'UID' to a reference, a display name ('Sentence'), and any additional reference display information ('RefInfo'). Resolves the reference later (similar to Ch).
Ref   :: UID -> Sentence -> RefInfo -> Sentence
```

And finally, we have some constructors for `Reference`s and `Reference :: Sentence`s, [`drasil-lang/lib/Language/Drasil/Document/Reference.hs`](https://github.com/JacquesCarette/Drasil/blob/c4ecd3f18d0995e1fbb8c530cfc76e3102f8fd7e/code/drasil-lang/lib/Language/Drasil/Document/Reference.hs#L50-L71):
```haskell
-- | Projector function that creates a 'Reference' from something 'Referable'.
ref :: (IsChunk r, HasRefAddress r, HasShortName r) => r -> Reference
ref r = Reference (r ^. uid) (getRefAdd r) (shortname r)

-- Maybe just use r ^. uid without 'ref'?
-- | Takes the reference 'UID' and wraps it into a 'Sentence'.
refS :: (IsChunk r, HasRefAddress r, HasShortName r) => r -> Sentence
refS r = namedRef r EmptyS

-- | Takes a 'Reference' with a name to be displayed and wraps it into a 'Sentence'.
-- Does not overwrite the shortname contained in the reference, but will only display as the given 'Sentence'.
namedRef :: (IsChunk r, HasRefAddress r, HasShortName r) => r -> Sentence -> Sentence
namedRef r s = namedComplexRef r s None

-- | Takes a 'Reference' with additional display info. Uses the internal shortname for its display name.
complexRef :: (IsChunk r, HasRefAddress r, HasShortName r) => r -> RefInfo -> Sentence
complexRef r = Ref (ref r ^. uid) EmptyS

-- | Takes a 'Reference' with a name to be displayed and any additional information and wraps it into a 'Sentence'.
-- Does not overwrite the shortname contained in the reference, but will only display as the given 'Sentence' along with the given 'RefInfo'.
namedComplexRef :: (IsChunk r, HasRefAddress r, HasShortName r) => r -> Sentence -> RefInfo -> Sentence
namedComplexRef r = Ref (ref r ^. uid)
```

`ref` shows us what `Reference` really is: a projection of a chunk out of another that contains information about "references." `Reference`s are later used in document generation for getting the in-text labels of things, [`drasil-printers/lib/Language/Drasil/Printing/Import/Sentence.hs`](https://github.com/JacquesCarette/Drasil/blob/4244c777c43c4054bdf23ede904de7a5bbca7017/code/drasil-printers/lib/Language/Drasil/Printing/Import/Sentence.hs#L22-L60):
```haskell
-- | Translates 'Sentence' to the printable representation of a 'Sentence' ('Spec').
spec :: PrintingInformation -> Sentence -> P.Spec
  -- make sure these optimizations are clear
spec sm (EmptyS :+: b)          = spec sm b
spec sm (a :+: EmptyS)          = spec sm a
spec sm (a :+: b)               = spec sm a P.:+: spec sm b
spec _  (S s)                   = either error P.S $ checkValidStr s invalidChars
  where invalidChars = ['<', '>', '\"', '&', '$', '%', '&', '~', '^', '\\', '{', '}']
spec _  (Sy s)                  = P.E $ pUnit s
spec sm (NP np)                 = spec sm (toSent $ phraseNP np)
spec _  Percent                 = P.E $ P.MO P.Perc
spec _  (P s)                   = P.E $ symbol s
spec sm (SyCh s)                = P.E $ symbol $ lookupSymb sm s

-- First term is the tooltip, second term is the rendered short form
spec sm (Ch ShortStyle caps s)  = P.Tooltip (spec sm $ lookupT
  sm s caps) (spec sm $ lookupS sm s caps)

spec sm (Ch TermStyle caps s)   = spec sm $ lookupT sm s caps
spec sm (Ch PluralTerm caps s) = spec sm $ lookupP sm s caps
spec sm (Ref u EmptyS notes)    =
  case refResolve (sm ^. sysdb) u of
    (Reference _ (RP rp ra) sn) ->
      P.Ref P.Internal ra (spec sm $ renderShortName sm rp sn)
    (Reference _ (Citation ra) _) ->
      P.Ref (P.Cite2 (spec sm (renderCitInfo notes)))    ra (spec sm $ S ra)
    (Reference _ (URI ra) sn) ->
      P.Ref P.External    ra (spec sm $ getSentSN sn)
spec sm (Ref u dName notes) =
  case refResolve (sm ^. sysdb) u of
    (Reference _ (RP _ ra) _) ->
      P.Ref P.Internal ra (spec sm dName)
    (Reference _ (Citation ra) _) ->
      P.Ref (P.Cite2 (spec sm (renderCitInfo notes)))   ra (spec sm dName)
    (Reference _ (URI ra) _) ->
      P.Ref P.External    ra (spec sm dName)
spec sm (Quote q)          = P.Quote $ spec sm q
spec _  EmptyS             = P.EmptyS
spec sm (E e)              = P.E $ modelExpr e sm
```

## Terminology Issue Summary

1. `Reference` is closer to a "citation." However, a `Reference` is also used to hack in _external links_ and treated like a chunk at times. So, it's not really an encoding of a citation either.
2. `Ref` is closer to a "cite" action (verb) than `Reference` is because it is a constructor of `Sentence` and citations are just inline markers.
2. `Citation` is closer to a "reference," paired with a LaTeX-convention-inspired label.
3. `BibRef`: mentioning "Ref" is unnecessary. Just `Bibliography` is enough. And the full name is preferred to avoid confusion with LaTeX `.bib` files. What we really care for generating is most often a list of "works cited," however.
4. `HasFields` is really about gathering all pieces of "path information" for a user to track down and access a source.
5. No real encoding/notion of a cross-reference nor external link.
6. "Source" is not mentioned anywhere in our documentation.

## What are we doing in [#5325](https://github.com/JacquesCarette/Drasil/pull/5325)?

There are a few things we do:

1. Insert more chunks in the `ChunkDB` instead of only their `Reference`s in the rendering-focused `Reference` map. Currently inserting dummy chunks for `Section`s and `LabelledContent`.
2. Delete the `Reference` map.
3. Insert strictly the `Reference`s that are external links into the `ChunkDB`. These have _actually unique_ `UID`s, unlike the ones that would be projected from other chunks.

## What should we do next?

1. Convert the `Reference`s that are external links into a (new) `ExternalLink` chunk (or something else of a different name).
2. Look into whether `Reference` is really a necessary type. It is only effectively used in the above `spec` function. It appears that we can get rid of the `Reference` projection currently done in `refResolve` in favour of immediately using the chunk's "reference" information.
3. Correct the terminology issues.
4. A general analysis of the "future chunk `Reference`s" issue.
5. `drasil-printers` & "document"-related things in `drasil-lang` file analysis.
