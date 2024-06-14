Please see [Chunks](Chunks), [Recipes](Recipes), and [Information Encoding](Information-Encoding) for more details on the use of data types in Drasil.

## Using Haskell types for Drasil

At a higher level, here is how we embed knowledge in Haskell:
- *data constructors* (whether ADTs or GADTs) are useful when we have multiple potential choices, and we need to discover at the point of use what we have, as these can be queried (by pattern matching)
- *class methods* are useful when we have many different representations, but they all share some core ideas. At the point of use, we just want "the idea" (via a function) and don't care about the details at all.
- *record fields* (with data) are useful when we have a specific representation that's made up of pieces, and we want to access the whole and the pieces
- *record fields* (with all functions) are one way to implement fake objects; they're basically equivalent to (Haskell) class methods

The above is a slightly edited copy of [@JacquesCarette's discussion](https://github.com/JacquesCarette/Drasil/issues/2853#issuecomment-1034427454) on a similar topic.

## Type Dependency Graphs

To see a table of all up-to-date type dependency graphs in Drasil, visit our [website](https://jacquescarette.github.io/Drasil/#Sec:Analysis). There are also class-instance dependency graphs along with module dependency graphs.

## Datatypes list (Updated June 2021)
An (for now) unordered summary of the construction of datatypes in Drasil. This should eventually become ordered by which types are built off of each other. Eventually I would like to put this in a visually-friendly format. Perhaps types can be shown as intersecting circles. If one type contains another, its circle is encased within the 'larger' type. Another way would be a dependency graph as is currently done for imports on the Drasil website. This was last updated in June 2021.

### Types in `drasil-lang`

- ConceptChunk
   - IdeaDict
   - Sentence
   - [UID]
- CommonConcept
   - CI
   - Sentence
- ConceptInstance
   - ConceptChunk
   - String
   - ShortName
- Citation
   - CitationKind
   - [CiteField]
   - UID
   - ShortName
- CI
   - UID
   - NP
   - String
   - [UID]
- ConstrainedChunk
   - QuantityDict
   - [Constraint]
   - Maybe Expr
- ConstrConcept
   - DefinedQuantityDict
   - [Constraint]
   - Maybe Expr
- DefinedQuantityDict
   - ConceptChunk
   - Stage Symbol
   - Space
   - Maybe UnitDefn
- QDefinition
   - QuantityDict
   - Sentence
   - Expr
   - [UID]
- NamedArgument
   - QuantityDict
- NamedChunk
   - UID
   - NP
- IdeaDict
   - NamedChunk
   - Maybe String
- QuantityDict
   - IdeaDict
   - Space
   - Stage Symbol
   - Maybe UnitDefn
- RelationConcept
   - ConceptChunk
   - Relation
- UncertainChunk
   - ConstrainedChunk
   - Uncertainty
- UncertQ
   - ConstrConcept
   - Uncertainty
- UnitDefn
   - ConceptChunk
   - UnitSymbol
   - [UID]
- UnitEquation
   - [UID]
   - USymb
- UnitalChunk
   - DefinedQuantityDict
   - UnitDefn
- UnitaryChunk
   - QuantityDict
   - UnitDefn
- UnitaryConceptDict
   - UnitaryChunk
   - Sentence
   - [UID]
- CiteField
   - Variety of fields needed to make a citation related field (not a record type).
- HP
   - URL or Verb (not a record type).
- CitationKind
   - Kind of citation (Article, Book, etc.). Not a record type.
- Month
   - Enumeration of months (not a record type).
- ListType
   - Bullet [(Itemtype, Maybe String)] | Numeric [(ItemType, Maybe String)] | Simple [ListTuple] | Desc [ListTuple] | Definitions [ListTuple]
- ItemType
   - Flat Sentence | Nested Header ListType
- Contents
   - UlC UnlabelledContent
   - LlC LabelledContent
- DType
   - General | Instance | Theory | Data
- RawContent
   - Table [Sentence] [ [Sentence] ] Title Bool
   - Paragraph Sentence    
   - EqnBlock DisplayExpr  
   - DerivBlock Sentence [RawContent]  
   - Enumeration ListType  
   - Defini DType [(Identifier, [Contents])] 
   - Figure Lbl Filepath MaxWidthPercent 
   - Bib BibRef 
   - Graph [(Sentence, Sentence)] (Maybe Width) (Maybe Height) Lbl
- LabelledContent
   - Reference
   - RawContent
- UnlabelledContent
   - RawContent
- LblType
   - PR IRefProg String | Citation String | URI String
- IRefProg
   - Deferred UID | RS String | RConcat IRefProg IRefProg | Name
- CapitalizationRule
   - CapFirst | CapWords | Replace Sentence
- PluralRule
   - AddS | AddE | AddES | SelfPlur | IrregPlur (String -> String)
- NP
   - ProperNoun String PluralRule | CommonNoun String PluralRule CapitalizationRule | Phrase Sentence PluralForm CapitalizationRule CapitalizationRule
- URI
   - URL Scheme Authority Path Query Fragment | ISBN String
- Scheme
   - HTTP | FTP
- Authority
   - Full Username Password Host Port | Simple Host Port
- Port
   - P Int | NA
- Uncertainty
   - Maybe Double
   - Maybe Int
- ConstriantReason
   - Physical | Software
- Constraint
   - Range ConstraintReason RealInterval Expr Expr | EnumeratedReal ConstraintReason [Double] | EnumeratedStr ConstraintReason [String]
- Derivation
   - Derivation Sentence [Sentence]
- DisplayBinOp
   - Defines | IsIn
- DisplayAssocBinOp
   - And | Equivalence
- DisplayExpr
   - AlgebraicExpr Expr | SpaceExpr Expr | BinOp DisplayBinOp DisplayExpr DisplayExpr| AssocBinOp DisplayAssocBinOp [DisplayExpr]
- Expression Related stuff (may look at later)
   - ArithBinOp
      - Frac | Pow | Subt
   - EqBinOp
      - Eq | NEq
   - BoolBinOp
      - Impl | Iff
   - LABinOp
      - Index
   - OrdBinOp
      - Lt | Gt | LEq | GEq
   - VVVBinOp
      - Cross
   - VVNBinOp
      - Dot
   - AssocArithOper
      - AddI | AddRe | MulI | MulRe
   - AssocBoolOper
      - And | Or
   - UFunc
      - Abs | Log | Ln | Sin | Cos | Tan | Sec | Csc | Cot | Arcsin | Arccos | Arctan | Exp | Sqrt | Neg
   - UFuncB
      - Not
   - UFuncVec
      - Norm | Dim
   - Completeness
      - Complete | Incomplete
   - Expr
      - Dbl Double | Int Integer | ExactDbl Integer | Str String | Perc Integer Integer | AssocA AssocArithOper [Expr] | AssocB AssocBoolOper [Expr] | Deriv DerivType Expr UID | C UID | FCall UID [Expr] [(UID, Expr)] | New UID [Expr] [(UID, Expr)] | Message UID UID [Expr] [(UID, Expr)] | Field UID UID | Case Completeness [(Expr, Relation)] | Matrix [ [Expr] ] | UnaryOp UFunc Expr | UnaryOpB UFuncB Expr | UnaryOpVec UFuncVec Expr | ArithBinaryOp ArithBinOp Expr Expr | BoolBinaryOp BoolBinOp Expr Expr | EqBinaryOp EqBinOp Expr Expr | LABinaryOp LABinOp Expr Expr | OrdBinaryOp OrdBinOp Expr Expr | VVVBinaryOp VVVBinOp Expr Expr | VVNBinaryOp VVNBinOp Expr Expr | Operator AssocArithOper DomainDesc Expr Expr Expr | RealI UID RealInterval Expr Expr
   - DerivType
      - Part | Total
- SecCons
   - Sub Section | Con Contents
- Section
   - Title
   - [SecCons]
   - Reference
- Document
   - Document Title Author [Section]
- Person
   - String
   - String
   - [String]
   - Conv
- Conv
   - Western | Eastern | Mono
- RefInfo
   - None | Equation [Int] | Page [Int] | RefNote String
- Reference
   - UID
   - LblType
   - ShortName
   - RefInfo
- SentenceStyle
   - PluralTerm | SymbolStyle | TermStyle | ShortStyle
- Sentence
   - Ch SentenceStyle UID | Sy Usymb | S String | P Symbol | E DisplayExpr | Ref UID RefInfo | Quote Sentence | Percent | Sentence :+: Sentence | EmptyS
- ShortName
   - ShortNm Sentence
- Space
   - Integer | Rational | Real | Natural | Boolean | Char | String | Radians | Vect Space | Array Space | Actor String | DiscreteD [Double] | DiscreteS [String] | Void
- RTopology
   - Continuous | Discrete
- DomainDesc
   - BoundedDD Symbol RTopology a b
   - AllDD Symbol RTopology
- Inclusive
   - Inc | Exc
- RealInterval
   - Bounded (Inclusive, a) (Inclusive, b)
   - UpTo (Inclusive, a)
   - UpFrom (Inclusive, b)
- Stage
   - Equational | Implementation
- Decoration
   - Hat | Vector | Prime
- Symbol
   - Variable String | Label String | Integ Int | Special Special | Atop Decoration Symbol | Corners  [Symbol] [Symbol] [Symbol] [Symbol] Symbol | Concat [Symbol] | 
Empty
- Special
   - Partial | Circle
- UDefn
   - USynonym USymb | UScale Double USymb | UShift Double USymb
- UnitSymbol
   - BaseSI USymb | DerivedSI USymb USymb UDefn | Defined USymb UDefn
- USymb
   - US [(Symbol, Integer)]

# Typeclasses
## How to Read This Section (Updated July 2017)
Headers in bold denote typeclasses. The symbol associated with this typeclass precedes the header. Functions listed after the header represent the typeclasses defining fields. Types are listed under a typeclass with a (&bull;).

### HasName
- Person
### LayoutObj
- Contents
- Section
### NounPhrase
- NP
- CI
### Chunk: id
Everything that follows is a `chunk`
### CommonIdea
- CI
### NamedIdea: term
All types that are instances of `Concept` and `SymbolForm`* in addition to;
- CI
- LCChunk
- MethodChunk
- ReqChunk
- NamedChunk
- NamedRelation
- NWrapper

\* Note `SF` is not an instance of `NamedIdea`
### UnitEq
- DerUChunk
### Unit
All `UnitEq` have instance `Unit`. This just adds `DerUChunk` currently.
- FundUnit
- UnitDefn
### <sup>C</sup>Concept: defn, cdom
All `Unit`s are `Concept`s in addition to;
- ConceptChunk
- CWrapper
- ConVar&#9734;
- ModualChunk
- UnitalChunk&#9734;&Dagger;
- CQSWrapper&#9734;
- UCWrapper&#9734;&Dagger;

Note `UncertQ` and `ConstrConcept` are also instances of `Concept`.
### &#9734; SymbolForm
All `Constrained` and `UncertaiQuantity` chunks are an instance of `SymbolForm` in addition to;
- QDefinition
- E
- SF
- UnitaryChunk&Dagger;
- VarChunk
- QSWapper
- UWrapper&Dagger;
### &Dagger; Unitary
These are `Chunks` that have a unit.
### Quantity
These are `Chunks` that are a quantity (not necessarily have a unit). All `SymbolForm`s are instances of `Quantity` with the exception of `SF`.
### UncertainQuantity
- UncertQ<sup>C</sup>&#9734;
- UncertainChunk&#9734;
- UncertainWrapper&#9734;
### Constrained
All `UncertainQuantity` are instances of `Constrained` in addition to;
- ConstrainedChunk&#9734;
- ConstrWrapper&#9734;
- ConstrConcept<sup>C</sup>&#9734;
### ExprRelat: relat
All ExprRelat are NamedIdeas
- NamedRelation
- RelationConcept<sup>C</sup>
- QDefinition&#9734;&Dagger;
- GenDefn<sup>C</sup>
### Theory
- TWrapper (not a `Chunk`)
- TheoryChunk
- TheoryModel<sup>C</sup>
### CodeEntity
- CodeDefinition&#9734;&Dagger;<sup>C</sup>
- CodeChunk&#9734;&Dagger;<sup>C</sup>
### CodeIdea
All `CodeEntity`s are `CodeIdea`s in addition to
- CodeName