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
  -- UnitLang
  , UDefn(..), from_udefn
  -- Unit
  , DerUChunk(..), UnitDefn(..), unitWrapper
  , makeDerU, unitCon, fund, comp_unitdefn
  , (^:), (/:), (*:), (*$), (/$), new_unit
  -- UID
  , UID
  -- Classes
  , HasUID(uid)
  , HasLabel(getLabel)
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
  , HasDerivation(derivations)
  -- Chunk.VarChunk
  , VarChunk, codeVC
  , vc, implVar
  , dcc, dcc', dccWDS, dccWDS', vc'', ccs, cc, cc', cic
  -- Chunk.Concept
  , cw , ConceptChunk , CommonConcept, ConceptInstance
  -- Chunk.CommonIdea
  , commonIdea, CI, getAcc
  -- Chunk.NamedIdea
  , NamedChunk, short, nc, IdeaDict
  , nw -- bad name (historical)
  , compoundNC, compoundNC', compoundNC'', compoundNC''', compoundNCP1, compoundNCPlPh, compoundNCPlPl
  , the, theCustom
  -- Chunk.Constrained.Core
  , physc, sfwrc, enumc , isPhysC, isSfwrC
  , Constraint(..), ConstraintReason(..)
  , Reason(..), TheoryConstraint(..)
  -- Chunk.Constrained
  , ConstrainedChunk(..), ConstrConcept(..)
  , constrained, cuc, cvc, cvc', constrained', cuc', constrainedNRV'
  , cnstrw
  -- Chunk.Eq
  , QDefinition, fromEqn, fromEqn', fromEqn'', getVC, equat, ec, fromEqn''', fromEqn''''
  -- Chunk.DataDefinition
  , DataDefinition, mkDataDef, mkDD, mkDataDef', qdFromDD
  -- Chunk.GenDefn
  , GenDefn, gd, gdUnit, gd'
  -- Chunk.InstanceModel
  , InstanceModel
  , inCons, outCons, imOutput, imInputs, im, imQD, im', imQD', im'', im'''
  -- Chunk.Quantity
  , Quantity(..), QuantityDict, qw, mkQuant
  -- Chunk.UncertainQuantity
  , UncertainQuantity(..), UncertainChunk(..), UncertQ, uq, uqNU, uqc, uqcNU, uqcND, uncrtnChunk, uvc
  , uncrtnw
  -- Chunk.Unital
  , UnitalChunk(..), makeUCWDS
  , uc, uc', ucs, ucs', ucsWS, ucFromDQD
  -- Chunk.Unitary
  , Unitary(..), UnitaryChunk, unitary
  -- Chunk.Relation
  , RelationConcept, makeRC, makeRC'
  --Chunk.DefinedQuantity
  , dqd, dqd', DefinedQuantityDict, dqdWr, dqdEL
  -- Chunk.UnitaryConcept
  , ucw, UnitaryConceptDict
  -- Chunk.Attributes --FIXME: Changed a lot
  , getSource
  , Derivation, getDerivation, getShortName
  , sourceref
  , References
  -- Chunk.ShortName
  , ShortName, shortname', HasShortName(shortname)
  --Citations
  , Citation(..), EntryID, BibRef, CiteField(..), Month(..), HP(..)
  , HasFields(..)
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
    -- Citation smart constructors
  , cArticle, cBookA, cBookE, cBooklet
  , cInBookACP, cInBookECP, cInBookAC, cInBookEC, cInBookAP, cInBookEP
  , cInCollection, cInProceedings, cManual, cMThesis, cMisc, cPhDThesis
  , cProceedings, cTechReport, cUnpublished
  , CitationKind(..)
  -- Spec
  , USymb(..), Sentence(..), sParen, sParenNum, sSqBr, sSqBrNum
  , (+:+), (+:+.), (+.), sC, (+:), semiCol, sParenDash
  , sDash
  -- NounPhrase
  , NounPhrase(..), NP, pn, pn', pn'', pn''', pnIrr, cn, cn', cn'', cn''', cnIP
  , cnIrr, cnIES, cnICES, cnIS, cnUM, nounPhrase, nounPhrase'
  , CapitalizationRule(..)
  , PluralRule(..)
  , compoundPhrase, compoundPhrase', compoundPhrase'', compoundPhrase''', compoundPhraseP1
  , titleize, titleize', nounPhrase'', nounPhraseSP, nounPhraseSent
  -- Document
  , Referable(..), Document(..), DType(..), Section(..), Contents(..)
  , SecCons(..), ListType(..), ItemType(..), ListPair
  , section, fig, figWithWidth, section'' 
  , datadefn, reldefn, MaxWidthPercent
  -- Reference
  , makeRef, acroTest, find'
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
  , mkTable, unit_symb, introduceAbb, phrase, plural, phrase's 
  , plural's, at_start, at_start'
  , unitToSentence, unitToSentenceUnitless
  -- Generate
  --, gen, genCode
  -- People
  , People, Person, person, HasName, name, manyNames, person', personWM
  , personWM', mononym, nameStr, rendPersLFM, rendPersLFM', rendPersLFM''
  -- CodeSpec
  --, CodeSpec, codeSpec, Choices(..), ImplementationType(..)
  --, Logging(..), ConstraintBehaviour(..), Structure(..), Comments(..)
  --, defaultChoices, getStr
  --, Mod(..), packmod, FuncDef(..), FuncStmt(..), funcDef, ($:=), ffor, fdec -- hacks
  --, relToQD, funcData, funcQD, Func(..), asExpr, asVC   -- hacks
  -- Code.DataDesc
  --, DataDesc
  --, entry, listEntry, junk, singleton, junkLine, singleLine, multiLine
  --, multiLine', straight, repeated, repeated', Ind(..)
  -- Chunk.Theory
  , Theory(..), tc', TheoryChunk, TheoryModel, tm, tm'
  -- Chunk.SymbolForm
  , Stage(Equational,Implementation), HasSymbol(symbol), eqSymb, codeSymb, hasStageSymbol
  -- ChunkDB
  , ChunkDB, cdb
  , HasSymbolTable, symbolMap, symbLookup, getUnitLup, symbolTable
  , HasTermTable, termLookup, termTable
  , HasDefinitionTable, conceptMap, defTable
  , HasUnitTable, unitMap, unitTable
  -- AssumpChunk
  , AssumpChunk, assuming, assump
  -- Attribute
  , snToSentence
  -- Referencing
  , ReferenceDB, AssumpMap, assumpLookup, assumptionsFromDB
  , rdb, assumpRefTable, customRef
  , HasAssumpRefs, HasReqRefs, reqRefTable, reqLookup
  , HasChangeRefs, changeRefTable, changeLookup, RefBy(..)
  , reqDB, assumpDB, RefMap, simpleMap
  -- ReqChunk
  , ReqChunk, ReqType(..), reqType, requires, frc, nfrc
  -- Change
  , Change, ChngType(..), chngType, chng, lc, ulc
  , citationRefTable, citeLookup
  -- Goal
  , Goal, mkGoal
  -- PhysSystDesc
  , PhysSystDesc, pSysDes, psd
  -- RefTypes
  , RefAdd, RefType(Cite)
  -- Document.getChunk
  , vars, combine', ccss
  -- Chunk.Sentence.EmbedSymbol
  , ch
  -- Chunk.Sentence.Extract
  , sdep, vars',snames, combine
  -- Chunk.Expr.Extract
  , names
  -- Document.Extract
  , egetDoc, getDoc
  -- Config
  , StyleGuide(..), verboseDDDescription, numberedTMEquations, numberedDDEquations
  , bibStyleH, numberedSections, hyperSettings, fontSize, bibFname, bibStyleT, colBwidth
  , colAwidth
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
import Language.Drasil.Document.Extract(egetDoc, getDoc)
import Language.Drasil.Expr.Extract (dep, names', names)
import Language.Drasil.Expr.Precedence (precA, precB, eprec)
import Language.Drasil.Sentence.EmbedSymbol(ch)
import Language.Drasil.Sentence.Extract(sdep,  snames)
import Language.Drasil.Document (Document(..), DType(..)
  , Section(..), Contents(..), SecCons(..), ListType(..), ItemType(..)
  , section, fig, figWithWidth, section''
  , datadefn, reldefn
  , ListPair, MaxWidthPercent)
import Language.Drasil.Unicode -- all of it
import Language.Drasil.Development.UnitLang -- all of it
import Language.Drasil.Development.Unit -- all of it
import Language.Drasil.UID (UID)
import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom), Concept, HasSymbol(symbol),HasSpace(typ),  HasUnitSymbol(usymb),
  IsUnit, CommonIdea(abrv), HasAdditionalNotes(getNotes),
  Constrained(constraints), HasReasVal(reasVal), ExprRelat(relat), HasDerivation(derivations),
  HasReference(getReferences), HasLabel(getLabel))
import Language.Drasil.Document.GetChunk(vars, combine', vars', combine, ccss)
import Language.Drasil.Config (StyleGuide(..), verboseDDDescription, numberedTMEquations,
  numberedDDEquations, bibStyleH, numberedSections, hyperSettings, bibFname, fontSize,
  colAwidth, colBwidth, bibStyleT)
import Language.Drasil.Chunk.AssumpChunk
import Language.Drasil.Chunk.Attribute
import Language.Drasil.Chunk.Derivation (Derivation)
import Language.Drasil.Chunk.References (References)
import Language.Drasil.Chunk.ShortName (ShortName, shortname', HasShortName(shortname))
import Language.Drasil.Chunk.Change
import Language.Drasil.Chunk.Citation (
  -- Types
    Citation(..), EntryID, BibRef, CiteField(..), Month(..), HP(..), CitationKind(..)
  , HasFields(..)
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
    -- Citation smart constructors
  , cArticle, cBookA, cBookE, cBooklet
  , cInBookACP, cInBookECP, cInBookAC, cInBookEC, cInBookAP, cInBookEP
  , cInCollection, cInProceedings, cManual, cMThesis, cMisc, cPhDThesis
  , cProceedings, cTechReport, cUnpublished)
import Language.Drasil.Chunk.CommonIdea
import Language.Drasil.Chunk.Concept
import Language.Drasil.Chunk.Constrained
import Language.Drasil.Chunk.Constrained.Core (physc, sfwrc, enumc, isPhysC, isSfwrC,
  Constraint(..), ConstraintReason(..), Reason(..), TheoryConstraint(..))
import Language.Drasil.Chunk.DefinedQuantity
import Language.Drasil.Chunk.Eq (QDefinition, fromEqn, fromEqn', fromEqn'', getVC,
 equat, ec, fromEqn''', fromEqn'''')
import Language.Drasil.Chunk.DataDefinition (DataDefinition, mkDataDef, mkDD, mkDataDef',
  qdFromDD)
import Language.Drasil.Chunk.GenDefn
import Language.Drasil.Chunk.Goal (Goal, mkGoal)
import Language.Drasil.Chunk.InstanceModel
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.PhysSystDesc (PhysSystDesc, pSysDes, psd)
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.Relation(RelationConcept, makeRC, makeRC')
import Language.Drasil.Chunk.ReqChunk(ReqChunk, ReqType(..), reqType, requires
                                     , frc, nfrc)
import Language.Drasil.Chunk.SymbolForm (eqSymb, codeSymb, hasStageSymbol)
import Language.Drasil.Chunk.Theory
import Language.Drasil.Chunk.UncertainQuantity
import Language.Drasil.Chunk.Unital(UnitalChunk(..), makeUCWDS,
                                   uc, uc', ucs, ucs', ucsWS, ucFromDQD)
import Language.Drasil.Chunk.Unitary
import Language.Drasil.Chunk.UnitaryConcept
import Language.Drasil.Chunk.VarChunk
import Language.Drasil.ChunkDB
import Language.Drasil.NounPhrase hiding (at_start, at_start', titleize
                                          , titleize', phrase, plural)
import Language.Drasil.Space (Space(..))
import Language.Drasil.Spec (Sentence(..),
  sParen, sParenNum, sSqBr, sSqBrNum, sC, (+:+), (+:+.), (+.), (+:),
  semiCol, sParenDash, sDash)
import Language.Drasil.Reference (makeRef, acroTest, ReferenceDB, assumpDB, reqDB
                                 , AssumpMap, assumpLookup, HasAssumpRefs
                                 , assumpRefTable, assumptionsFromDB
                                 , rdb, reqRefTable, reqLookup, RefBy(..)
                                 , HasReqRefs, Referable(..), customRef
                                 , HasChangeRefs, changeRefTable, changeLookup
                                 , citationRefTable, citeLookup, RefMap
                                 , simpleMap, find')
import Language.Drasil.Symbol (Decoration(..), Symbol(..), sub, sup, vec, hat, 
  prime, sCurlyBrSymb, compsy, Stage(..))
import Language.Drasil.SymbolAlphabet
import Language.Drasil.Misc -- all of it
--import Language.Drasil.Generate -- moved in SubPackages
import Language.Drasil.People (People, Person, person, HasName(..), manyNames
  , person', personWM, personWM', mononym, name, nameStr, rendPersLFM, 
  rendPersLFM', rendPersLFM'')
import Language.Drasil.RefTypes(RefAdd, RefType(Cite))

--Should be in lang-dev package?
