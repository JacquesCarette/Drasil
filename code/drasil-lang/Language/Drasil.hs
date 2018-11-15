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
  , dep, names'
  -- Expr.Precendence
  , precA, precB, eprec
  -- all the stuff from Unicode
  , Special(..), RenderSpecial(..)
   -- UID
  , UID
  -- Classes
  , HasUID(uid)
  , HasLabel(getLabel)
  , MayHaveLabel(getMaybeLabel)
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
  , DataDefinition, mkQuantDef, mkDD, qdFromDD, mkDDL
  -- Chunk.GenDefn
  , GenDefn, gd', gd''
  -- Chunk.InstanceModel
  , InstanceModel
  , inCons, outCons, imOutput, imInputs, im, im', im'', im'''
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
  , resolveSN, ShortName, shortname', getStringSN
  --Citations
  , Citation, EntryID, BibRef
  , citeID, citeKind
    -- Citation smart constructors
  , cArticle, cBookA, cBookE, cBooklet
  , cInBookACP, cInBookECP, cInBookAC, cInBookEC, cInBookAP, cInBookEP
  , cInCollection, cInProceedings, cManual, cMThesis, cMisc, cPhDThesis
  , cProceedings, cTechReport, cUnpublished
  -- Sentence
  , Sentence(..), sParen, sSqBr , (+:+), (+:+.), sC, (+:)
  , RefProg, RefProg2, Reference2(..)
  -- NounPhrase
  , NounPhrase(..), NP, pn, pn', pn'', pn''', pnIrr, cn, cn', cn'', cn''', cnIP
  , cnIrr, cnIES, cnICES, cnIS, cnUM, nounPhrase, nounPhrase'
  , CapitalizationRule(..)
  , PluralRule(..)
  , compoundPhrase, compoundPhrase', compoundPhrase'', compoundPhrase''', compoundPhraseP1
  , titleize, titleize', nounPhrase'', nounPhraseSP, nounPhraseSent
  -- Document
  , Referable(..), Document(..), DType(..), Section(..), Contents(..)
  , SecCons(..), ListType(..), ItemType(..), ListTuple
  , LabelledContent(..), UnlabelledContent(..)
  , mkParagraph, mkRawLC
  , llcc, ulcc
  , section, fig, figWithWidth, section''
  , MaxWidthPercent
  , HasContents(accessContents)
  , RawContent(..)
  , mkFig
  -- Reference
  , makeRef, makeRefS, mkRefFrmLbl, makeRef2, makeRef2S
  -- Space
  , Space(..)
  -- Symbol
  , Decoration(..), Symbol(..), sub, sup, vec, hat, prime, sCurlyBrSymb, compsy
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
  , HasDefinitionTable, conceptMap, defTable, defLookup
  , HasUnitTable, unitMap, unitTable, collectUnits
  -- AssumpChunk
  , AssumpChunk, assuming, assump
  -- Referencing
  , ReferenceDB, AssumpMap, assumpLookup, assumptionsFromDB
  , rdb, assumpRefTable, customRef, HasAssumpRefs
  , RefBy(..)
  , assumpDB, RefMap, simpleMap
  , citationRefTable
  -- RefTypes
  , RefAdd, RefType(Cite, Tab, EqnB, Req, LCh, UnCh, Def, Lst, Link, Sect, Blank, Assump)
  , ReqType(FR, NFR)
  , Reference(Reference)
  -- Label
  , Label 
  , mkLabelRA', mkLabelSame, mkEmptyLabel, mkURILabel
  , mkLabelRAAssump', mkLabelRAFig, mkLabelRASec
  , modifyLabelEqn
  -- Document.getChunk
  , vars, vars', combine, combine', ccss
  -- Chunk.Sentence.EmbedSymbol
  , ch
  -- Sentence.Extract
  , sdep
  -- Expr.Extract
  , names
  -- Label.Core
  , getAdd
  -- Development.Sentence
  , introduceAbb, phrase, plural, phrase's, plural's, at_start, at_start'
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
import Language.Drasil.Sentence.Extract(sdep)
import Language.Drasil.Document (section, fig, figWithWidth
  , section''
  , Section(..), SecCons(..) 
  , llcc, ulcc, Document(..)
  , mkParagraph, mkFig, mkRawLC)
import Language.Drasil.Document.Core (Contents(..), ListType(..), ItemType(..)
  , RawContent(..), ListTuple, MaxWidthPercent
  , HasContents(accessContents)
  , LabelledContent(..), UnlabelledContent(..) )
import Language.Drasil.Unicode -- all of it
import Language.Drasil.UID (UID)
import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom), Concept, HasSymbol(symbol), HasUnitSymbol(usymb),
  IsUnit, CommonIdea(abrv), HasAdditionalNotes(getNotes), Constrained(constraints), 
  HasReasVal(reasVal), ExprRelat(relat), HasDerivation(derivations), HasReference(getReferences), 
  HasLabel(getLabel), MayHaveLabel(getMaybeLabel), HasRefAddress(getRefAdd), HasSpace(typ),
  DefiningExpr(defnExpr), HasShortName(shortname), Quantity, UncertainQuantity(uncert),
  HasFields(getFields))
import Language.Drasil.Label.Core (Label)
import Language.Drasil.Derivation (Derivation)
import Language.Drasil.ChunkDB.GetChunk(vars, combine', vars', combine, ccss)
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
import Language.Drasil.Chunk.Constrained.Core (physc, sfwrc, enumc, isPhysC, isSfwrC,
  Constraint(..), ConstraintReason(..))
import Language.Drasil.Chunk.DefinedQuantity
import Language.Drasil.Chunk.Eq (QDefinition, fromEqn, fromEqn', equat, ec)
import Language.Drasil.Chunk.DataDefinition (DataDefinition, mkQuantDef, mkDD, qdFromDD, mkDDL)
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
import Language.Drasil.NounPhrase hiding (at_start, at_start', titleize
                                          , titleize', phrase, plural)
import Language.Drasil.ShortName (resolveSN, ShortName
  , shortname', getStringSN)
import Language.Drasil.Space (Space(..))
import Language.Drasil.Sentence (Sentence(..), sParen, sSqBr, sC, (+:+), (+:+.), (+:), RefProg,
  RefProg2, Reference2(Reference2))
import Language.Drasil.Reference (makeRef, makeRefS, mkRefFrmLbl, ReferenceDB, assumpDB
                                 , AssumpMap, assumpLookup, HasAssumpRefs
                                 , assumpRefTable, assumptionsFromDB
                                 , rdb, RefBy(..)
                                 , Referable(..), customRef
                                 , citationRefTable, RefMap
                                 , simpleMap, makeRef2S, makeRef2)
import Language.Drasil.Symbol (Decoration(..), Symbol(..), sub, sup, vec, hat, 
  prime, compsy)
import Language.Drasil.Symbol.Helpers (eqSymb, codeSymb, hasStageSymbol, sCurlyBrSymb)
import Language.Drasil.Stages (Stage(..))
import Language.Drasil.SymbolAlphabet
import Language.Drasil.Misc -- all of it
import Language.Drasil.People (People, Person, person, HasName(..), manyNames
  , person', personWM, personWM', mononym, name, nameStr, rendPersLFM, 
  rendPersLFM', rendPersLFM'')
import Language.Drasil.RefTypes(RefAdd, RefType(..),
  DType(..), Reference(Reference), ReqType(FR, NFR))
import Language.Drasil.Label (mkLabelRA', mkLabelSame, 
  mkEmptyLabel, mkURILabel, mkLabelRAAssump', mkLabelRAFig, mkLabelRASec, modifyLabelEqn)
import Language.Drasil.Label.Type (getAdd)
--Should be in lang-dev package?

import Language.Drasil.UnitLang (USymb(US))

import Language.Drasil.Development.Sentence -- are these really development?
