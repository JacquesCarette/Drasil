-- | The Drasil language, including expressions, chunks, sentences, references,
-- classes, datatypes, and generally useful functions. Re-exports modules to simplify external use.
module Language.Drasil (
  -- * The Drasil Expression Language
  -- | Encodes mathematical and display related expressions.

  -- ** Base Expression Language
  -- | Defines the expression types and common operators.

  -- Language.Drasil.Expr
  Expr
  , ExprC(..)
  , frac, recip_
  , square, half
  , oneHalf, oneThird
  , apply1, apply2
  , m2x2, vec2D, dgnl2x2, rowVec, columnVec, mkSet
  , Completeness, Relation

  -- ** Literals Language
  , Literal
  , LiteralC(..)

  -- ** Expression Modelling Language
  -- | Defines display-related expression functions. Used in models.

  -- Language.Drasil.ModelExpr
  , ModelExpr(Spc)
  , DerivType
  , ModelExprC(..)

  -- ** Unicode symbols
  -- | Some expressions need special unicode characters.

  -- Language.Drasil.Unicode
  , Special(..), RenderSpecial(..)
  -- * The Drasil Language (Information Encoding)
  -- | This is the basis of the Drasil language for encoding information.
  -- Every chunk used in Drasil is defined here, along with some classes
  -- that help us to use these chunks.

  -- ** Classes
  -- | Contains many of the classes used in Drasil, along with their methods.

  -- *** Chunk-related
  -- Language.Drasil.Symbol
  , HasSymbol(symbol)
  -- Language.Drasil.Classes
  , NamedIdea(term)
  , Idea(getA)
  , CommonIdea(abrv)
  , Definition(defn)
  , ConceptDomain(cdom)
  , Concept
  , HasSpace(typ)
  , HasUnitSymbol(usymb)
  , Quantity
  , HasReasVal(reasVal)
  , Constrained(constraints)
  , HasAdditionalNotes(getNotes)
  , IsUnit(getUnits)
  , DefiningExpr(defnExpr)
  , Express(express)
  -- *** References
  -- Language.Drasil.Symbol
  , HasRefAddress(getRefAdd)
  , Referable(..)
  -- Language.Drasil.Classes
  , HasReference(getReferences)
  -- ** Types
  -- | Contains helper functions and smart constructors for each type.
  -- Similar types are grouped together.

  -- *** Basic types
  -- Language.Drasil.Chunk.NamedIdea
  , nc, ncUID, IdeaDict , mkIdea
  , nw -- bad name (historical)
  -- Language.Drasil.Chunk.CommonIdea
  , CI, commonIdeaWithDict, prependAbrv

  -- *** Concepts
  -- Language.Drasil.Chunk.Concept.Core
  , ConceptChunk, ConceptInstance, sDom
  -- Language.Drasil.Chunk.Concept
  , dcc, dccAWDS, dccA, dccWDS, cc', ccs, cw, cic
  -- Language.Drasil.Chunk.Relation
  , RelationConcept, makeRC

  -- *** Quantities and Units
  -- Language.Drasil.Chunk.Eq
  , QDefinition, fromEqn, fromEqn', fromEqnSt, fromEqnSt', fromEqnSt''
  , mkQDefSt, mkQuantDef, mkQuantDef', ec
  , mkFuncDef, mkFuncDef', mkFuncDefByQ
  -- Language.Drasil.Chunk.Unitary
  , unit_symb
  -- Language.Drasil.Chunk.DefinedQuantity
  , DefinedQuantityDict, dqd, dqd', dqdNoUnit, dqdNoUnit', dqdQd, dqdWr
  , DefinesQuantity(defLhs), implVar, implVar', implVarAU'
  -- Language.Drasil.Chunk.Unital
  , UnitalChunk(..), uc, uc', ucStaged, ucStaged', ucuc, ucw
  -- Language.Drasil.Chunk.UnitDefn
  , UnitDefn(..)
  , fromUDefn, unitCon, makeDerU
  , (^:), (/:), (*:), (*$), (/$), (^$), newUnit
  , scale, shift
  , derUC, derUC', derUC''
  , fund, fund', compUnitDefn, derCUC, derCUC', derCUC''
  , unitWrapper, getCu, MayHaveUnit(getUnit)

  -- *** Constrained and Uncertain Values
  -- Language.Drasil.Constraint
  , ConstraintReason(..), Constraint(..), ConstraintE
  , physRange, sfwrRange, physElem, sfwrElem, isPhysC, isSfwrC
  -- Language.Drasil.Chunk.Constrained
  , ConstrConcept(..)
  , constrained', cuc', cuc'', cucNoUnit', constrainedNRV'
  , cnstrw'
  -- Language.Drasil.Chunk.UncertainQuantity
  , UncertQ, uq, uqc, uqcND
  -- Language.Drasil.Uncertainty
  , Uncertainty, uncty, HasUncertainty(..)
  , defaultUncrt, uncVal, uncPrec, exact

  -- ** Referencing
  -- Language.Drasil.Label.Type
  , getAdd, prepend
  , LblType(RP, Citation, URI), IRefProg(..)
  -- Language.Drasil.Reference
  , Reference(..), ref, refS, namedRef, complexRef, namedComplexRef
  -- Language.Drasil.Decorated Reference
  , DecRef(refInfo), dRefInfo, dRef, HasDecRef(..)

  -- *** Citations
  -- Language.Drasil.Chunk.Citation
  , EntryID, Citation, BibRef
  , HasCitation(getCitations)
  , HasFields(getFields)
  -- accessors
  , citeID, citeKind
  -- smart constructors
  , cArticle, cBookA, cBookE, cBooklet
  , cInBookACP, cInBookECP, cInBookAC, cInBookEC, cInBookAP, cInBookEP
  , cInCollection, cInProceedings, cManual, cMThesis, cMisc, cPhDThesis
  , cProceedings, cTechReport, cUnpublished
  -- Language.Drasil.Data.Date
  , Month(..)
  -- Language.Drasil.Data.Citation; should be moved to Language.Drasil.Development
  , CiteField(..), HP(..), CitationKind(..)
  , compareAuthYearTitle
    -- CiteFields smart constructors
      -- People -> CiteField
  , author, editor
      -- Sentence -> CiteField
  , address, bookTitle, howPublished, howPublishedU, institution, journal, note
  , organization, publisher, school, series, title, typeField
      -- Int -> CiteField
  , chapter, edition, number, volume, year
      -- [Int] -> CiteField
  , pages
      -- Month -> CiteField
  , month
  -- Language.Drasil.People
  , People, Person, person, HasName, name, person', personWM
  , personWM', mononym, nameStr, rendPersLFM, rendPersLFM', rendPersLFM''
  , comparePeople

  -- * Sentences
  -- | Things like expressions and terms are displayed by using 'Sentence's.
  -- We also use 'NounPhrase's to record the proper pluralization and capitalization of terms.

  -- Language.Drasil.Sentence
  , Sentence(..), SentenceStyle(..), TermCapitalization(..), RefInfo(..), (+:+), (+:+.), (+:), (!.), capSent, headSent
  , ch, eS, eS', sC, sDash, sParen
  -- Language.Drasil.Sentence.Generators
  , fromSource, fterms, getTandS, checkValidStr
  -- Language.Drasil.NounPhrase
  , NounPhrase(..), NP, pn, pn', pn'', pn''', pnIrr, cn, cn', cn'', cn''', cnIP
  , cnIrr, cnIES, cnICES, cnIS, cnUM, nounPhrase, nounPhrase'
  , CapitalizationRule(..), atStartNP, atStartNP'
  , PluralRule(..)
  , compoundPhrase, compoundPhrase', compoundPhrase'', compoundPhrase''', compoundPhraseP1
  , titleizeNP, titleizeNP', nounPhrase'', nounPhraseSP, nounPhraseSent
  -- Language.Drasil.Development.Sentence
  , introduceAbb, phrase, plural, phrasePoss, pluralPoss, atStart, atStart'
  , titleize, titleize', short
  -- Language.Drasil.ShortName
  , ShortName, shortname', getSentSN, HasShortName(..)

  -- * Sentence Fold-type utilities.
  -- | From "Utils.Drasil.Fold". Defines many general fold functions
  -- for use with Drasil-related types.

  -- ** Folding Options as Types
  , EnumType(..), WrapType(..), SepType(..), FoldType(..)

  -- ** Folding functions
  -- *** Expression-related
  , foldConstraints

  -- *** Sentence-related
  , foldlEnumList, foldlList, foldlSent
  , foldlSent_,foldlSentCol, foldlsC, foldNums, numList

  -- * Basic Document Language
  -- | Holds all the types and helper functions needed especially in @drasil-docLang@
  -- Language.Drasil.Document
  , Document(..), ShowTableOfContents(..), DType(..), Section(..)
  , Contents(..), SecCons(..), ListType(..), ItemType(..), ListTuple
  , LabelledContent(..), UnlabelledContent(..), HasCaption(..)
  , mkParagraph, mkRawLC, checkToC
  , llcc, llccFig, llccTab, llccEqn, llccFig', llccTab', llccEqn', ulcc
  , section, fig, figNoCap, figWithWidth, figNoCapWithWidth
  , MaxWidthPercent
  , HasContents(accessContents)
  , RawContent(..)
  , mkFig
  , makeTabRef, makeFigRef, makeSecRef, makeEqnRef, makeURI
  , makeTabRef', makeFigRef', makeSecRef', makeEqnRef', makeURI'

  -- * Symbols, Stages, Spaces
  -- | Used for rendering mathematical symbols in Drasil.

  -- Language.Drasil.Space
  , Space(..)
  , RealInterval(..), Inclusive(..)
  , DomainDesc(..), RTopology(..), ContinuousDomainDesc, DiscreteDomainDesc
  , getActorName, getInnerSpace
  , mkFunction, Primitive
  -- Language.Drasil.Symbol
  , Decoration, Symbol
  -- Language.Drasil.UnitLang
  , USymb(US)
  -- Language.Drasil.Stages
  , Stage(Equational,Implementation)
  -- Language.Drasil.Symbol.Helpers
  , eqSymb, codeSymb, hasStageSymbol
  , sortBySymbol, sortBySymbolTuple
  , autoStage, hat, prime, staged, sub, subStr, sup , unicodeConv, upperLeft, vec
  , label, variable

  -- * Type Synonyms
  , ConstQDef, SimpleQDef, ModelQDef
  , PExpr

  -- * Type systems
  , TypingContext
  , TypeError
  , inferFromContext
  , Typed(..)
  , RequiresChecking(..)
  , temporaryIndent -- FIXME: Once a proper breadcrumb system is built (hopefully soon, we can remove this export.)
) where

import Prelude hiding (log, sin, cos, tan, sqrt, id, return, print, break, exp, product)

import Language.Drasil.WellTyped (RequiresChecking(..), Typed(..), TypingContext,
  TypeError, inferFromContext, temporaryIndent)

import Language.Drasil.Expr.Class (ExprC(..),
  frac, recip_, square, half, oneHalf, oneThird, apply1, apply2,
  m2x2, vec2D, dgnl2x2, rowVec, columnVec, mkSet, PExpr)
import Language.Drasil.Expr.Lang (Expr, Completeness, Relation)
import Language.Drasil.Literal.Class (LiteralC(..))
import Language.Drasil.Literal.Lang (Literal)
import Language.Drasil.ModelExpr.Class (ModelExprC(..))
import Language.Drasil.ModelExpr.Lang (ModelExpr, DerivType, ModelExpr(Spc))
import Language.Drasil.Document (section, fig, figNoCap, figWithWidth, figNoCapWithWidth
  , Section(..), SecCons(..) , llcc, llccFig, llccTab, llccEqn, llccFig', llccTab', llccEqn', ulcc, Document(..)
  , mkParagraph, mkFig, mkRawLC, ShowTableOfContents(..), checkToC
  , makeTabRef, makeFigRef, makeSecRef, makeEqnRef, makeURI
  , makeTabRef', makeFigRef', makeSecRef', makeEqnRef', makeURI')
import Language.Drasil.Document.Core (Contents(..), ListType(..), ItemType(..), DType(..)
  , RawContent(..), ListTuple, MaxWidthPercent
  , HasContents(accessContents)
  , LabelledContent(..), UnlabelledContent(..), HasCaption(..))
import Language.Drasil.Unicode (RenderSpecial(..), Special(..))
import Language.Drasil.Symbol (HasSymbol(symbol), Decoration, Symbol)
import Language.Drasil.Classes (Definition(defn), ConceptDomain(cdom), Concept, HasUnitSymbol(usymb),
  IsUnit(getUnits), CommonIdea(abrv), HasAdditionalNotes(getNotes), Constrained(constraints),
  HasReasVal(reasVal), DefiningExpr(defnExpr), Quantity, Express(..))
import Language.Drasil.Data.Date (Month(..))
import Language.Drasil.Chunk.Citation (
    Citation, EntryID, BibRef
  , HasCitation(..)
  , citeID, citeKind
  , cArticle, cBookA, cBookE, cBooklet
  , cInBookACP, cInBookECP, cInBookAC, cInBookEC, cInBookAP, cInBookEP
  , cInCollection, cInProceedings, cManual, cMThesis, cMisc, cPhDThesis
  , cProceedings, cTechReport, cUnpublished)
import Language.Drasil.Chunk.CommonIdea
import Language.Drasil.Chunk.Concept
import Language.Drasil.Chunk.Concept.Core (sDom)
import Language.Drasil.Chunk.Constrained
import Language.Drasil.Constraint (physRange, sfwrRange, physElem, sfwrElem, isSfwrC, isPhysC,
  Constraint(..), ConstraintE, ConstraintReason(..))
import Language.Drasil.Chunk.DefinedQuantity
import Language.Drasil.Chunk.Eq (QDefinition, fromEqn, fromEqn', fromEqnSt,
  fromEqnSt', fromEqnSt'', mkQDefSt, mkQuantDef, mkQuantDef', ec,
  mkFuncDef, mkFuncDef', mkFuncDefByQ, ConstQDef, SimpleQDef, ModelQDef)
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Relation(RelationConcept, makeRC)
import Language.Drasil.Chunk.UncertainQuantity
import Language.Drasil.Chunk.Unital(UnitalChunk(..), uc, uc', ucStaged, ucStaged',
  ucuc, ucw)
import Language.Drasil.Data.Citation (CiteField(..), HP(..), CitationKind(..)
  , HasFields(getFields)
  , author, editor
  , address, bookTitle, howPublished, howPublishedU, institution, journal, note
  , organization, publisher, school, series, title, typeField
  , chapter, edition, number, volume, year, month, pages
  , compareAuthYearTitle)
import Language.Drasil.NounPhrase
import Language.Drasil.ShortName (ShortName, shortname', getSentSN, HasShortName(..))
import Language.Drasil.Space (Space(..), RealInterval(..), Inclusive(..),
  RTopology(..), DomainDesc(..), ContinuousDomainDesc, DiscreteDomainDesc,
  getActorName, getInnerSpace, HasSpace(..), mkFunction, Primitive)
import Language.Drasil.Sentence (Sentence(..), SentenceStyle(..), TermCapitalization(..), RefInfo(..), (+:+),
  (+:+.), (+:), (!.), capSent, headSent, ch, eS, eS', sC, sDash, sParen)
import Language.Drasil.Sentence.Fold
import Language.Drasil.Sentence.Generators (fromSource, fterms, getTandS, checkValidStr)
import Language.Drasil.Reference (Reference(..), namedRef, complexRef, namedComplexRef, ref, refS, HasReference(..))
import Language.Drasil.DecoratedReference(DecRef(refInfo), dRefInfo, dRef, HasDecRef(..))
import Language.Drasil.Symbol.Helpers (eqSymb, codeSymb, hasStageSymbol,
  autoStage, hat, prime, staged, sub, subStr, sup, unicodeConv, upperLeft, vec,
  label, variable, sortBySymbol, sortBySymbolTuple)
import Language.Drasil.Stages (Stage(..))
import Language.Drasil.People (People, Person, person, HasName(..),
  person', personWM, personWM', mononym, name, nameStr, rendPersLFM,
  rendPersLFM', rendPersLFM'', comparePeople)
import Language.Drasil.Label.Type hiding (name)

import Language.Drasil.UnitLang (USymb(US))
import Language.Drasil.Uncertainty

import Language.Drasil.Development.Sentence -- are these really development?
import Language.Drasil.Chunk.UnitDefn (UnitDefn(..)
  , fromUDefn, unitCon, makeDerU
  , (^:), (/:), (*:), (*$), (/$),(^$), newUnit
  , scale, shift
  , derUC, derUC', derUC''
  , fund, fund', compUnitDefn, derCUC, derCUC', derCUC''
  , unitWrapper, getCu, MayHaveUnit(getUnit))
import Language.Drasil.Chunk.Unitary (unit_symb)
