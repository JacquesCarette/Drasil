{- re-export many things to simplify external use -}
module Language.Drasil (
  -- SystemInformation
    SystemInformation(..), Block(..), citeDB, getRefDB
  -- Expr
  , Expr(..), BinOp(..), UFunc(..), ArithOper(..), BoolOper(..), DerivType(..)
  , Relation, RealInterval(..), Inclusive(..), RTopology(..), DomainDesc(AllDD, BoundedDD)
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
  -- Expr.Extract
  , dep, names', lnames, lnames'
  -- Expr.Precendence
  , precA, precB, eprec
  -- all the stuff from Unicode
  , Special(..), RenderSpecial(..)
   -- UID
  , UID
  -- Classes
  , HasUID(uid)
  , NamedIdea(term)
  , HasAdditionalNotes(getNotes)
  , Idea(getA)
  , Definition(defn)
  , ConceptDomain(cdom)
  , Concept
  , HasSpace(typ)
  , HasUnitSymbol(usymb)
  , IsUnit
  , HasReference(getReferences)
  , CommonIdea(abrv)
  , Constrained(constraints)
  , HasReasVal(reasVal)
  , ExprRelat(relat)
  , DefiningExpr(defnExpr)
  , HasDerivation(derivations)
  , HasRefAddress(getRefAdd)
  , HasShortName(shortname)
  , UncertainQuantity(uncert)
  , Quantity
  , HasFields(getFields)
  -- Chunk.Concept
  , cw , ConceptChunk , CommonConcept, ConceptInstance
  -- Chunk.CommonIdea
  , commonIdea, CI, getAcc, commonIdeaWithDict
  -- Chunk.NamedIdea
  , NamedChunk, short, nc, IdeaDict , mkIdea
  , nw -- bad name (historical)
  -- Chunk.Constrained.Core
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
  , Sentence(..), sParen, (+:+), (+:+.), sC, (+:)
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
  -- Symbol
  , Decoration(..), Symbol(..), sub, sup, vec, hat, prime, compsy
  -- SymbolAlphabet
  , cA, cB, cC, cD, cE, cF, cG, cH, cI, cJ, cK, cL, cM, cN, cO, cP, cQ, cR, cS, cT, cU, cV, cW, cX, cY, cZ
  , lA, lB, lC, lD, lE, lF, lG, lH, lI, lJ, lK, lL, lM, lN, lO, lP, lQ, lR, lS, lT, lU, lV, lW, lX, lY, lZ
  , lAlpha, cAlpha, lBeta, cBeta, lGamma, cGamma, lDelta, cDelta, lEpsilon, vEpsilon, cEpsilon, lZeta, cZeta
  , lEta, cEta, lTheta, cTheta, lIota, cIota, lKappa, cKappa, lLambda, cLambda, lMu, cMu, lNu, cNu, lXi, cXi 
  , lOmicron, cOmicron, lPi, cPi, lRho, cRho, lSigma, cSigma, lTau, cTau, lUpsilon, cUpsilon, lPhi, vPhi, cPhi
  , lChi, cChi, lPsi, cPsi, lOmega, cOmega, lNabla, lEll
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
  , HasSymbol(symbol), eqSymb, codeSymb, hasStageSymbol
  -- ChunkDB
  , ChunkDB, cdb
  , HasSymbolTable, symbolMap, symbLookup, symbolTable
  , HasTermTable, termLookup, termTable
  , HasDefinitionTable, conceptMap, defTable, defLookup, labelledconLookup
  , HasUnitTable, unitMap, unitTable, collectUnits, LabelledContentMap
  , TraceMap, traceLookup, HasTraceTable(..), generateRefbyMap, RefbyMap
  , refbyLookup, HasRefbyTable(..), DatadefnMap, InsModelMap, AssumptionMap, HasLabelledContent(..)
  , ConceptInstanceMap, GendefMap, TheoryModelMap, datadefnLookup, insmodelLookup, sectionLookup
  , gendefLookup, theoryModelLookup, assumptionLookup, conceptinsLookup, HasDataDefnTable(..)
  , HasInsModelTable(..), HasGendefTable(..), HasTheoryModelTable(..), HasSectionTable(..)
  , HasAssumpTable(..), HasConceptInstance(..), SectionMap
  -- AssumpChunk
  , AssumpChunk(AC), assuming, assump
  -- Reference
  , makeRef2S, makeCite, makeCiteS, makeRef2
  , ReferenceDB, AssumpMap, assumpLookup, assumptionsFromDB
  , rdb, assumpRefTable, HasAssumpRefs
  , RefBy(..)
  , assumpDB, RefMap, simpleMap
  , citationRefTable
  -- Document.getChunk
  , vars, vars', combine, combine', ccss, getIdeaDict
  -- Chunk.Sentence.EmbedSymbol
  , ch
  -- Sentence.Extract
  , sdep
  -- Expr.Extract
  , names
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
) where

import Prelude hiding (log, sin, cos, tan, sqrt, id, return, print, break, exp, product)
import Language.Drasil.SystemInformation
import Language.Drasil.Expr (Expr(..), BinOp(..), UFunc(..), ArithOper(..), DerivType(..),
          BoolOper(..), Relation, RealInterval(..), Inclusive(..), RTopology(..), 
          DomainDesc(AllDD, BoundedDD),
          ($=), ($<), ($<=), ($>), ($>=), ($^), ($&&), ($||), ($=>), ($<=>), ($.))
import Language.Drasil.Expr.Math (log, ln, sin, cos, tan, sqrt, square, sec, csc, cot, exp,
          dim, idx, int, dbl, str, isin, case_,
          sum_all, defsum, prod_all, defprod,
          real_interval,
          apply, apply1, apply2,
          sy, deriv, pderiv,
          cross, m2x2, vec2D, dgnl2x2, euclidean, defint, int_all)
import Language.Drasil.Expr.Extract (dep, names', names)
import Language.Drasil.Expr.Precedence (precA, precB, eprec)
import Language.Drasil.Sentence.EmbedSymbol(ch)
import Language.Drasil.Sentence.Extract(sdep, lnames, lnames')
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
import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom), Concept, HasSymbol(symbol), HasUnitSymbol(usymb),
  IsUnit, CommonIdea(abrv), HasAdditionalNotes(getNotes), Constrained(constraints), 
  HasReasVal(reasVal), ExprRelat(relat), HasDerivation(derivations), 
  HasReference(getReferences),
  HasRefAddress(getRefAdd), HasSpace(typ),
  DefiningExpr(defnExpr), HasShortName(shortname), Quantity, UncertainQuantity(uncert),
  HasFields(getFields))
-- import Language.Drasil.Label.Core (Label)
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
  , cProceedings, cTechReport, cUnpublished
  -- move?
  , HasCitation(getCitations) )
import Language.Drasil.Chunk.CommonIdea
import Language.Drasil.Chunk.Concept
import Language.Drasil.Chunk.Constrained
import Language.Drasil.Chunk.Constrained.Core (physc, sfwrc, enumc, isPhysC, isSfwrC,
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
import Language.Drasil.Space (Space(..))
import Language.Drasil.Sentence (Sentence(..), sParen, sC, (+:+), (+:+.), (+:), SentenceStyle(..))
import Language.Drasil.Reference (makeCite, makeCiteS, ReferenceDB, makeRef2
 , AssumpMap, assumpLookup, HasAssumpRefs, assumpDB , assumpRefTable, assumptionsFromDB
 , rdb, RefBy(..), Referable(..), citationRefTable, RefMap, simpleMap, makeRef2S)
import Language.Drasil.Symbol (Decoration(..), Symbol(..), sub, sup, vec, hat, 
  prime, compsy)
import Language.Drasil.Symbol.Helpers (eqSymb, codeSymb, hasStageSymbol)
import Language.Drasil.Stages (Stage(..))
import Language.Drasil.SymbolAlphabet
import Language.Drasil.Misc -- all of it
import Language.Drasil.People (People, Person, person, HasName(..), manyNames
  , person', personWM, personWM', mononym, name, nameStr, rendPersLFM, 
  rendPersLFM', rendPersLFM'')
import Language.Drasil.RefProg(Reference(Reference))
import Language.Drasil.Label.Type (getAdd, LblType(RP, Citation, URI), IRefProg(..))

import Language.Drasil.UnitLang (USymb(US))

import Language.Drasil.Development.Sentence -- are these really development?
