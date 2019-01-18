{- re-export many things to simplify external use -}
module Language.Drasil (
  -- SystemInformation
    SystemInformation(..), Block(..), citeDB
  , ReferenceDB, rdb, RefMap, simpleMap
  -- Expr
  , Expr(..), BinOp(..), UFunc(..), ArithOper(..), BoolOper(..), DerivType(..)
  , Relation
  , ($=), ($<), ($<=), ($>), ($>=), ($^), ($&&), ($||), ($=>), ($<=>), ($.)
  -- Expr.Math
  , log, ln, abs, sin, cos, tan, sec, csc, cot, exp, sqrt, square, euclidean
  , dim, idx, int, dbl, str, isin, case_
  , sum_all, defsum, prod_all, defprod, defint, int_all
  , real_interval
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
  , IsUnit
  , CommonIdea(abrv)
  , Constrained(constraints)
  , ExprRelat(relat)
  , DefiningExpr(defnExpr)
  , UncertainQuantity(uncert)
  , Quantity
  -- Chunk.Concept
  , cw , ConceptChunk , CommonConcept, ConceptInstance
  -- Chunk.CommonIdea
  , commonIdea, CI, getAcc, commonIdeaWithDict
  -- Chunk.NamedIdea
  , NamedChunk, short, nc, IdeaDict , mkIdea
  , nw -- bad name (historical)
  -- Constraint
  , physc, sfwrc, enumc , isPhysC, isSfwrC
  , Constraint(..), ConstraintReason(..)
  -- Chunk.Constrained
  , ConstrainedChunk(..), ConstrConcept(..)
  , cuc, cvc, constrained', cuc', constrainedNRV'
  , cnstrw, cnstrw'
  -- Chunk.Eq
  , QDefinition, fromEqn, fromEqn', equat, ec
  -- Chunk.DataDefinition
  , DataDefinition, mkQuantDef, mkDD, qdFromDD
  -- Chunk.GenDefn
  , GenDefn, gd', gd''
  -- Chunk.InstanceModel
  , InstanceModel
  , inCons, outCons, imOutput, imInputs, im', im''
  , Constraints
  -- Chunk.Quantity
  , QuantityDict, qw, mkQuant
  , codeVC, vc, implVar , dcc, dcc', dccWDS, dccWDS', vc'', ccs, cc, cc', cic
  -- Chunk.UncertainQuantity
  , UncertainChunk(..), UncertQ, uq, uqc, uqcND, uncrtnChunk, uvc
  , uncrtnw
  -- Chunk.Unital
  , UnitalChunk(..), makeUCWDS
  , uc, uc', ucs, ucs', ucsWS
  -- Chunk.Unitary
  , Unitary(..), UnitaryChunk, unitary, unit_symb
  -- Chunk.Relation
  , RelationConcept, makeRC
  --Chunk.DefinedQuantity
  , dqd, dqd', DefinedQuantityDict, dqdWr, dqdQd
  -- Chunk.UnitaryConcept
  , ucw, UnitaryConceptDict
  -- Derivation
  , Derivation
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
  , Sentence(..), sParen, (+:+), (+:+.), sC, (+:), ch
  , SentenceStyle(..)
  -- RefProg
  , Reference(..)
  -- NounPhrase
  , NounPhrase(..), NP, pn, pn', pn'', pn''', pnIrr, cn, cn', cn'', cn''', cnIP
  , cnIrr, cnIES, cnICES, cnIS, cnUM, nounPhrase, nounPhrase'
  , CapitalizationRule(..), at_startNP, at_startNP'
  , PluralRule(..)
  , compoundPhrase, compoundPhrase', compoundPhrase'', compoundPhrase''', compoundPhraseP1
  , titleizeNP, titleizeNP', nounPhrase'', nounPhraseSP, nounPhraseSent
  -- Document
  , Referable(..), Document(..), DType(..), Section(..), Contents(..)
  , SecCons(..), ListType(..), ItemType(..), ListTuple
  , LabelledContent(..), UnlabelledContent(..), extractSection
  , mkParagraph, mkRawLC
  , llcc, ulcc
  , section, fig, figWithWidth, section''
  , MaxWidthPercent
  , HasContents(accessContents)
  , RawContent(..)
  , mkFig
  , makeTabRef, makeFigRef, makeSecRef, makeLstRef, makeURI
  -- Space
  , Space(..)
  , RealInterval(..), Inclusive(..), RTopology(..), DomainDesc(AllDD, BoundedDD)
  -- Symbol
  , Decoration(..), Symbol(..), sub, sup, vec, hat, prime, compsy
  -- Misc
  , mkTable
  -- People
  , People, Person, person, HasName, name, manyNames, person', personWM
  , personWM', mononym, nameStr, rendPersLFM, rendPersLFM', rendPersLFM''
  -- Chunk.Theory
  , TheoryModel, tm, Theory(..)
  -- Stages
  , Stage(Equational,Implementation)
  -- Symbol.Helpers
  , eqSymb, codeSymb, hasStageSymbol
  -- ChunkDB
  , ChunkDB, cdb
  , symbLookup, symbolTable
  , termLookup, termTable
  , conceptMap, traceMap, defTable, defLookup, labelledconLookup
  , unitTable, collectUnits
  , traceLookup, traceTable, TraceMap, generateRefbyMap, RefbyMap
  , refbyLookup, refbyTable, labelledcontentTable
  , datadefnLookup, insmodelLookup, sectionLookup
  , gendefLookup, theoryModelLookup, conceptinsLookup, dataDefnTable
  , insmodelTable, gendefTable, theoryModelTable, sectionTable
  , conceptinsTable, asOrderedList
  -- AssumpChunk
  , AssumpChunk(AC), assuming, assump
  -- Reference
  , makeRef2S, makeCite, makeCiteS, makeRef2
  -- Document.getChunk
  , vars, vars', combine, combine', ccss, getIdeaDict
  -- Label.Type
  , getAdd
  , LblType(RP, Citation, URI), IRefProg(..)
  -- Development.Sentence
  , introduceAbb, phrase, plural, phrase's, plural's, at_start, at_start'
  , titleize, titleize'
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
  , from_udefn, unitCon, makeDerU
  , (^:), (/:), (*:), (*$), (/$),(^$), new_unit
  , scale, shift
  , derUC, derUC', derUC''
  , fund, fund', comp_unitdefn, derCUC, derCUC', derCUC''
  , unitWrapper, getCu, MayHaveUnit(getUnit)
) where

import Prelude hiding (log, sin, cos, tan, sqrt, id, return, print, break, exp, product)
import Language.Drasil.SystemInformation
import Language.Drasil.Expr (Expr(..), BinOp(..), UFunc(..), ArithOper(..), DerivType(..),
          BoolOper(..), Relation,
          ($=), ($<), ($<=), ($>), ($>=), ($^), ($&&), ($||), ($=>), ($<=>), ($.))
import Language.Drasil.Expr.Math (log, ln, sin, cos, tan, sqrt, square, sec, csc, cot, exp,
          dim, idx, int, dbl, str, isin, case_,
          sum_all, defsum, prod_all, defprod,
          real_interval,
          apply, apply1, apply2,
          sy, deriv, pderiv,
          cross, m2x2, vec2D, dgnl2x2, euclidean, defint, int_all)
import Language.Drasil.Document (section, fig, figWithWidth
  , section'', Section(..), SecCons(..) , llcc, ulcc, Document(..)
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
  IsUnit, CommonIdea(abrv), HasAdditionalNotes(getNotes), Constrained(constraints), 
  HasReasVal(reasVal), ExprRelat(relat), HasDerivation(derivations), 
  HasReference(getReferences), HasSpace(typ), Referable(refAdd, renderRef),
  DefiningExpr(defnExpr), Quantity, UncertainQuantity(uncert))
import Language.Drasil.Classes.Citations (HasFields(getFields))
import Language.Drasil.Classes.Document (HasCitation(getCitations))
import Language.Drasil.Derivation (Derivation)
import Language.Drasil.ChunkDB.GetChunk(vars, combine', vars', combine, ccss, getIdeaDict)
import Language.Drasil.Chunk.AssumpChunk
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
import Language.Drasil.Chunk.Constrained
import Language.Drasil.Constraint (physc, sfwrc, enumc, isPhysC, isSfwrC,
  Constraint(..), ConstraintReason(..))
import Language.Drasil.Chunk.DefinedQuantity
import Language.Drasil.Chunk.Eq (QDefinition, fromEqn, fromEqn', equat, ec)
import Language.Drasil.Chunk.DataDefinition (DataDefinition, mkQuantDef, mkDD, qdFromDD)
import Language.Drasil.Chunk.GenDefn
import Language.Drasil.Chunk.InstanceModel
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.Relation(RelationConcept, makeRC)
import Language.Drasil.Chunk.Theory
import Language.Drasil.Chunk.UncertainQuantity
import Language.Drasil.Chunk.Unital(UnitalChunk(..), makeUCWDS, uc, uc', ucs, ucs', ucsWS)
import Language.Drasil.Chunk.Unitary
import Language.Drasil.Chunk.UnitaryConcept
import Language.Drasil.ChunkDB
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
import Language.Drasil.Sentence (Sentence(..), sParen, sC, (+:+), (+:+.), (+:), ch
  , SentenceStyle(..))
import Language.Drasil.Reference (makeCite, makeCiteS, makeRef2, makeRef2S)
import Language.Drasil.Symbol (Decoration(..), Symbol(..), sub, sup, vec, hat, 
  prime, compsy)
import Language.Drasil.Symbol.Helpers (eqSymb, codeSymb, hasStageSymbol)
import Language.Drasil.Stages (Stage(..))
import Language.Drasil.Misc -- all of it
import Language.Drasil.People (People, Person, person, HasName(..), manyNames
  , person', personWM, personWM', mononym, name, nameStr, rendPersLFM, 
  rendPersLFM', rendPersLFM'')
import Language.Drasil.RefProg(Reference(Reference))
import Language.Drasil.Label.Type (getAdd, LblType(RP, Citation, URI), IRefProg(..))

import Language.Drasil.UnitLang (USymb(US))

import Language.Drasil.Development.Sentence -- are these really development?
import Language.Drasil.Chunk.UnitDefn (UnitDefn(..)
  , from_udefn, unitCon, makeDerU
  , (^:), (/:), (*:), (*$), (/$),(^$), new_unit
  , scale, shift
  , derUC, derUC', derUC''
  , fund, fund', comp_unitdefn, derCUC, derCUC', derCUC''
  , makeDerU, unitWrapper, getCu, MayHaveUnit(getUnit))
