{- re-export many things to simplify external use -}
module Language.Drasil (
  -- Expr
  Expr(..), BinOp(..), UFunc(..), ArithOper(..), BoolOper(..), DerivType(..)
  , Completeness(..), Relation
  , ($=), ($<), ($<=), ($>), ($>=), ($^), ($&&), ($||), ($=>), ($<=>), ($.)
  -- Expr.Extract
  , dep
  -- Expr.Math
  , log, ln, abs, sin, cos, tan, sec, csc, cot, arcsin, arccos, arctan, exp
  , sqrt, square, euclidean
  , dim, idx, int, dbl, str, perc, isin, completeCase, incompleteCase
  , sumAll, defsum, prodAll, defprod, defint, intAll
  , realInterval
  , deriv, pderiv
  , sy -- old "Chunk" constructor C
  , apply, apply1, apply2
  , cross, m2x2, vec2D, dgnl2x2
  -- all the stuff from Unicode
  , Special(..), RenderSpecial(..)
   -- UID
  , UID
  -- Classes.Core
  , HasUID(uid)
  , HasShortName(shortname)
  , HasRefAddress(getRefAdd)
  , HasSymbol(symbol)
  -- Classes.Document
  , HasFields(getFields)
  -- Classes
  , NamedIdea(term)
  , HasAdditionalNotes(getNotes)
  , HasSpace(typ)
  , HasUnitSymbol(usymb)
  , HasReference(getReferences)
  , HasReasVal(reasVal)
  , HasDerivation(derivations)
  , Idea(getA)
  , Definition(defn)
  , ConceptDomain(cdom)
  , Concept
  , IsUnit(getUnits)
  , CommonIdea(abrv)
  , Constrained(constraints)
  , ExprRelat(relat)
  , DefiningExpr(defnExpr)
  , HasUncertainty(unc)
  , Quantity
  -- Chunk.Concept
  , cw , ConceptChunk , CommonConcept, ConceptInstance
  -- Chunk.Concept.Core
  , sDom
  -- Chunk.CommonIdea
  , commonIdea, CI, getAcc, getAccStr, commonIdeaWithDict, prependAbrv
  -- Chunk.NamedIdea
  , NamedChunk, short, nc, IdeaDict , mkIdea
  , nw -- bad name (historical)
  -- Constraint
  , physc, sfwrc, enumc , isPhysC, isSfwrC
  , Constraint(..), ConstraintReason(..)
  -- Chunk.Constrained
  , ConstrainedChunk(..), ConstrConcept(..)
  , cuc, cvc, constrained', cuc', cuc'', constrainedNRV'
  , cnstrw, cnstrw'
  -- Chunk.Eq
  , QDefinition, fromEqn, fromEqn', equat, ec
  -- Chunk.Quantity
  , QuantityDict, qw, mkQuant, mkQuant'
  , codeVC, implVar , dcc, dcc', dccWDS, dccWDS', vc, vc'', vcUnit, ccs, cc, cc', cic
  -- Chunk.UncertainQuantity
  , UncertainChunk(..), UncertQ, uq, uqc, uqcND, uncrtnChunk, uvc
  , uncrtnw
  -- Chunk.Unital
  , UnitalChunk(..), makeUCWDS
  , uc, uc', ucs, ucs', ucsWS
  -- Chunk.Unitary
  , Unitary(..), UnitaryChunk, unitary, unitary', unit_symb
  -- Chunk.Relation
  , RelationConcept, makeRC
  --Chunk.DefinedQuantity
  , DefinedQuantityDict, dqd, dqd', dqdNoUnit, dqdQd, dqdWr
  -- Chunk.UnitaryConcept
  , ucw, UnitaryConceptDict
  -- Derivation
  , Derivation(Derivation), mkDeriv, mkDerivName, mkDerivNoHeader
  -- ShortName
  , ShortName, shortname', getStringSN
  --Citations
  , Citation, EntryID, BibRef
  , citeID, citeKind
    -- Citation smart constructors
  , cArticle, cBookA, cBookE, cBooklet
  , cInBookACP, cInBookECP, cInBookAC, cInBookEC, cInBookAP, cInBookEP
  , cInCollection, cInProceedings, cManual, cMThesis, cMisc, cPhDThesis
  , cProceedings, cTechReport, cUnpublished
  -- Chunk.Citation
  , HasCitation(getCitations)
  -- Sentence
  , Sentence(..), sParen, sDash, (+:+), (+:+.), sC, (+:), ch
  , SentenceStyle(..)
  -- Sentence.Extract
  , sdep, shortdep
  -- RefProg
  , Reference(..), RefInfo(..)
  -- NounPhrase
  , NounPhrase(..), NP, pn, pn', pn'', pn''', pnIrr, cn, cn', cn'', cn''', cnIP
  , cnIrr, cnIES, cnICES, cnIS, cnUM, nounPhrase, nounPhrase'
  , CapitalizationRule(..), atStartNP, atStartNP'
  , PluralRule(..)
  , compoundPhrase, compoundPhrase', compoundPhrase'', compoundPhrase''', compoundPhraseP1
  , titleizeNP, titleizeNP', nounPhrase'', nounPhraseSP, nounPhraseSent
  -- Document
  , Referable(..), Document(..), DType(..), Section(..), Contents(..)
  , SecCons(..), ListType(..), ItemType(..), ListTuple
  , LabelledContent(..), UnlabelledContent(..), extractSection
  , mkParagraph, mkRawLC
  , llcc, ulcc
  , section, fig, figWithWidth
  , MaxWidthPercent
  , HasContents(accessContents)
  , RawContent(..)
  , mkFig
  , makeTabRef, makeFigRef, makeSecRef, makeLstRef, makeURI
  -- Space
  , Space(..)
  , RealInterval(..), Inclusive(..), RTopology(..), DomainDesc(AllDD, BoundedDD)
  -- Symbol
  , Decoration(..), Symbol(..), autoStage, compsy, hat, prime, staged, sub, sup
  , upperLeft, vec
  -- Misc
  , mkTable
  -- People
  , People, Person, person, HasName, name, person', personWM
  , personWM', mononym, nameStr, rendPersLFM, rendPersLFM', rendPersLFM''
  , comparePeople
  -- Stages
  , Stage(Equational,Implementation)
  -- Symbol.Helpers
  , eqSymb, codeSymb, hasStageSymbol
  -- Reference
  , makeRef2S, makeCite, makeCiteS, makeRef2, makeCiteInfo, makeCiteInfoS
  -- Label.Type
  , getAdd, prepend
  , LblType(RP, Citation, URI), IRefProg(..)
  -- Development.Sentence
  , introduceAbb, phrase, plural, phrasePoss, pluralPoss, atStart, atStart'
  , titleize, titleize'
  -- Uncertainty.Core
  , Uncertainty, uncty
  -- Uncertainty
  , defaultUncrt, uncVal, uncPrec
  -- UnitLang
  , USymb(US)
  -- Data.Date
  , Month(..)
  -- Data.Citation ; should be moved to Language.Drasil.Development
  , CiteField(..), HP(..), CitationKind(..)
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
  -- Chunk.UnitDefn
  , UnitDefn(..)
  , fromUDefn, unitCon, makeDerU
  , (^:), (/:), (*:), (*$), (/$),(^$), newUnit
  , scale, shift
  , derUC, derUC', derUC''
  , fund, fund', compUnitDefn, derCUC, derCUC', derCUC''
  , unitWrapper, getCu, MayHaveUnit(getUnit)
) where

import Prelude hiding (log, sin, cos, tan, sqrt, id, return, print, break, exp, product)
import Language.Drasil.Expr (Expr(..), BinOp(..), UFunc(..), ArithOper(..), 
          DerivType(..), BoolOper(..), Completeness(..), Relation,
          ($=), ($<), ($<=), ($>), ($>=), ($^), ($&&), ($||), ($=>), ($<=>), ($.))
import Language.Drasil.Expr.Extract (dep) -- exported for drasil-database FIXME: move to development package?
import Language.Drasil.Expr.Math (log, ln, sin, cos, tan, sqrt, square, sec, 
          csc, cot, arcsin, arccos, arctan, exp,
          dim, idx, int, dbl, str, perc, isin, completeCase, incompleteCase,
          sumAll, defsum, prodAll, defprod,
          realInterval,
          apply, apply1, apply2,
          sy, deriv, pderiv,
          cross, m2x2, vec2D, dgnl2x2, euclidean, defint, intAll)
import Language.Drasil.Document (section, fig, figWithWidth
  , Section(..), SecCons(..) , llcc, ulcc, Document(..)
  , mkParagraph, mkFig, mkRawLC, extractSection
  , makeTabRef, makeFigRef, makeSecRef, makeLstRef, makeURI)
import Language.Drasil.Document.Core (Contents(..), ListType(..), ItemType(..), DType(..)
  , RawContent(..), ListTuple, MaxWidthPercent
  , HasContents(accessContents)
  , LabelledContent(..), UnlabelledContent(..) )
import Language.Drasil.Unicode -- all of it
import Language.Drasil.UID (UID)
import Language.Drasil.Classes.Core (HasUID(uid), HasSymbol(symbol),
  HasRefAddress(getRefAdd), HasShortName(shortname))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom), Concept, HasUnitSymbol(usymb),
  IsUnit(getUnits), CommonIdea(abrv), HasAdditionalNotes(getNotes), Constrained(constraints), 
  HasReasVal(reasVal), ExprRelat(relat), HasDerivation(derivations), 
  HasReference(getReferences), HasSpace(typ), Referable(refAdd, renderRef),
  DefiningExpr(defnExpr), Quantity, HasUncertainty(unc))
import Language.Drasil.Classes.Citations (HasFields(getFields))
import Language.Drasil.Classes.Document (HasCitation(getCitations))
import Language.Drasil.Derivation (Derivation(Derivation), mkDeriv, mkDerivName, mkDerivNoHeader)
import Language.Drasil.Data.Date (Month(..))
import Language.Drasil.Chunk.Citation (
  -- Types
    Citation, EntryID, BibRef
    -- Accessors
  , citeID, citeKind
    -- CiteFields smart constructors
      -- People -> CiteField
    -- Citation smart constructors
  , cArticle, cBookA, cBookE, cBooklet
  , cInBookACP, cInBookECP, cInBookAC, cInBookEC, cInBookAP, cInBookEP
  , cInCollection, cInProceedings, cManual, cMThesis, cMisc, cPhDThesis
  , cProceedings, cTechReport, cUnpublished)
import Language.Drasil.Chunk.CommonIdea
import Language.Drasil.Chunk.Concept
import Language.Drasil.Chunk.Concept.Core (sDom) -- exported for drasil-database FIXME: move to development package?
import Language.Drasil.Chunk.Constrained
import Language.Drasil.Constraint (physc, sfwrc, enumc, isPhysC, isSfwrC,
  Constraint(..), ConstraintReason(..))
import Language.Drasil.Chunk.DefinedQuantity
import Language.Drasil.Chunk.Eq (QDefinition, fromEqn, fromEqn', equat, ec)
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.Relation(RelationConcept, makeRC)
import Language.Drasil.Chunk.UncertainQuantity
import Language.Drasil.Chunk.Unital(UnitalChunk(..), makeUCWDS, uc, uc',
  ucs, ucs', ucsWS)
import Language.Drasil.Chunk.Unitary
import Language.Drasil.Chunk.UnitaryConcept
import Language.Drasil.Data.Citation(CiteField(..), HP(..), CitationKind(..) -- for Printing
  , author, editor
      -- Sentence -> CiteField
  , address, bookTitle, howPublished, howPublishedU, institution, journal, note
  , organization, publisher, school, series, title, typeField
      -- Int -> CiteField
  , chapter, edition, number, volume, year
      -- [Int] -> CiteField
  , pages
      -- Month -> CiteField
  , month)
import Language.Drasil.NounPhrase
import Language.Drasil.ShortName (ShortName, shortname', getStringSN)
import Language.Drasil.Space (Space(..)
  , RealInterval(..), Inclusive(..), RTopology(..), DomainDesc(AllDD, BoundedDD))
import Language.Drasil.Sentence (Sentence(..), sParen, sDash, sC, (+:+), (+:+.), (+:), ch
  , SentenceStyle(..))
import Language.Drasil.Sentence.Extract (sdep, shortdep) -- exported for drasil-database FIXME: move to development package?
import Language.Drasil.Reference (makeCite, makeCiteS, makeRef2, makeRef2S, makeCiteInfo, makeCiteInfoS)
import Language.Drasil.Symbol (Decoration(..), Symbol(..), autoStage, compsy,
  hat, prime, staged, sub, sup, upperLeft, vec)
import Language.Drasil.Symbol.Helpers (eqSymb, codeSymb, hasStageSymbol)
import Language.Drasil.Stages (Stage(..))
import Language.Drasil.Misc -- all of it
import Language.Drasil.People (People, Person, person, HasName(..),
  person', personWM, personWM', mononym, name, nameStr, rendPersLFM, 
  rendPersLFM', rendPersLFM'', comparePeople)
import Language.Drasil.RefProg(Reference(..), RefInfo(..))
import Language.Drasil.Label.Type (getAdd, LblType(RP, Citation, URI), IRefProg(..), prepend)

import Language.Drasil.UnitLang (USymb(US))
import Language.Drasil.Uncertainty.Core(Uncertainty, uncty)
import Language.Drasil.Uncertainty(defaultUncrt, uncVal, uncPrec)

import Language.Drasil.Development.Sentence -- are these really development?
import Language.Drasil.Chunk.UnitDefn (UnitDefn(..)
  , fromUDefn, unitCon, makeDerU
  , (^:), (/:), (*:), (*$), (/$),(^$), newUnit
  , scale, shift
  , derUC, derUC', derUC''
  , fund, fund', compUnitDefn, derCUC, derCUC', derCUC''
  , makeDerU, unitWrapper, getCu, MayHaveUnit(getUnit))
