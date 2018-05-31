{- re-export many things to simplify external use -}
module Language.Drasil (
  -- Output.Formats
    DocType(SRS,MG,MIS,Website), DocSpec(DocSpec)
  -- SystemInformation
  , SystemInformation(..), Block(..), citeDB
  -- Expr
  , Expr
  , Relation, RealInterval(..), Inclusive(..)
  , ($=), ($<), ($<=), ($>), ($>=), ($^), ($&&), ($||), ($=>), ($<=>), ($.)
  -- Expr.Math
  , log, abs, sin, cos, tan, sec, csc, cot, exp, sqrt, square, euclidean, vars
  , dim, idx, int, dbl, str, isin, case_
  , sum_all, defsum, prod_all, defprod, defint, int_all
  , real_interval
  , deriv, pderiv
  , sy -- old "Chunk" constructor C
  , apply, apply1, apply2
  , cross, m2x2, vec2D, dgnl2x2
  -- all the stuff from Unicode
  , Greek(..), Special(..)
  -- UnitLang
  , UDefn(..), from_udefn
  -- Unit
  , DerUChunk(..), UnitDefn(..), unitWrapper
  , makeDerU, unitCon, fund, comp_unitdefn
  , (^:), (/:), (*:), (*$), (/$), new_unit
  -- Classes
  , HasUID(uid)
  , NamedIdea(term)
  , Idea(getA)
  , Definition(defn), ConceptDomain(cdom)
  , Concept
  , HasUnitSymbol(usymb)
  , IsUnit
  , HasAttributes(attributes)
  , HasReference(getReferences)
  , CommonIdea(abrv)
  , Constrained(constraints)
  , HasReasVal(reasVal)
  , ExprRelat(relat)
  -- Chunk.VarChunk
  , VarChunk
  , vc, implVar
  , dcc, dcc', dccWDS, dccWDS', vc'', ccs, cc, cc'
  -- Chunk.Concept
  , cw , ConceptChunk , CommonConcept
  -- Chunk.CommonIdea
  , commonIdea, CI, getAcc
  -- Chunk.NamedIdea
  , NamedChunk, short, nc, IdeaDict
  , nw -- bad name (historical)
  , compoundNC, compoundNC', compoundNC'', compoundNC'''
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
  , QDefinition, fromEqn, fromEqn', fromEqn'', getVC, equat, ec
  -- Chunk.GenDefn
  , GenDefn, gd, gdUnit
  -- Chunk.InstanceModel
  , InstanceModel
  , inCons, outCons, imOutput, imInputs, im, imQD
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
  , dqd, dqd', dqdEL, DefinedQuantityDict, dqdWr
  -- Chunk.UnitaryConcept
  , ucw, UnitaryConceptDict
  -- Chunk.Attributes.Core
  , Attributes
  -- Chunk.Attributes
  , getSource
  , Derivation, getDerivation, getShortName, shortname
  , sourceref, derivationsteps
  , References
  --Citations
  , Citation, BibRef, CiteField, Month(..), HP
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
  , compoundPhrase, compoundPhrase', compoundPhrase'', compoundPhrase''', titleize, titleize'
  , nounPhrase'', nounPhraseSP, nounPhraseSent
  -- Document
  , Referable(..), Document(..), DType(..), Section(..), Contents(..)
  , SecCons(..), ListType(..), ItemType(..), ListPair
  , section, fig, figWithWidth
  , datadefn, reldefn
  -- Reference
  , makeRef, acroTest, find'
  -- Space
  , Space(..)
  -- Symbol
  , Symbol(..), sub, sup, vec, hat, prime, sCurlyBrSymb, compsy
  -- SymbolAlphabet
  , cA, cB, cC, cD, cE, cF, cG, cH, cI, cJ, cK, cL, cM, cN, cO, cP, cQ, cR, cS, cT, cU, cV, cW, cX, cY, cZ
  , lA, lB, lC, lD, lE, lF, lG, lH, lI, lJ, lK, lL, lM, lN, lO, lP, lQ, lR, lS, lT, lU, lV, lW, lX, lY, lZ
  -- Misc
  , mkTable, unit'2Contents, unit_symb, introduceAbb, phrase, plural, phrase's, plural's, at_start, at_start'
  , unitHidingUnitless
  -- Printing.Helpers
  , capitalize, paren, sqbrac
  -- Generate
  , gen, genCode
  -- People
  , People, Person, person, HasName, name, manyNames, person', personWM
  , personWM', mononym
  -- CodeSpec
  , CodeSpec, codeSpec, Choices(..), ImplementationType(..)
  , Logging(..), ConstraintBehaviour(..), Structure(..), Comments(..)
  , defaultChoices, getStr
  , Mod(..), packmod, FuncDef(..), FuncStmt(..), funcDef, ($:=), ffor, fdec -- hacks
  , relToQD, funcData, funcQD, Func(..), asExpr, asVC   -- hacks
  -- DataDesc
  , DataDesc
  , entry, listEntry, junk, singleton, junkLine, singleLine, multiLine
  , multiLine', straight, repeated, repeated', Ind(..)
  -- Chunk.Theory
  , Theory(..), tc', TheoryChunk, TheoryModel, tm
  -- Chunk.SymbolForm
  , Stage(Equational,Implementation), HasSymbol(symbol), eqSymb, codeSymb, hasStageSymbol
  -- Code.Imperative.Lang
  , Lang(..)
  -- ChunkDB
  , ChunkDB, cdb
  , HasSymbolTable, symbolMap, symbLookup, getUnitLup, symbolTable
  , HasTermTable, termLookup, termTable
  , HasDefinitionTable, conceptMap, defTable
  , HasUnitTable, unitMap, unitTable
  -- AssumpChunk
  , AssumpChunk, assuming, assump
  -- Referencing
  , ReferenceDB, AssumpMap, assumpLookup, assumptionsFromDB
  , rdb, assumpRefTable, customRef
  , HasAssumpRefs, HasReqRefs, reqRefTable, reqLookup
  , HasChangeRefs, changeRefTable, changeLookup, RefBy(..)
  , reqDB, assumpDB, RefMap, simpleMap
  -- ReqChunk
  , ReqChunk, ReqType(..), reqType, requires, frc, nfrc, rc'
  -- Change
  , Change, ChngType(..), chngType, chng, lc, ulc, chc'
  , citationRefTable, citeLookup
  -- Goal
  , Goal, mkGoal
  -- PhysSystDesc
  , PhysSystDesc, pSysDes, psd, psd'
  -- RefTypes
  , RefAdd
) where

import Prelude hiding (log, sin, cos, tan, sqrt, id, return, print, break, exp, product)
import Language.Drasil.SystemInformation
import Language.Drasil.Expr (Expr(..), Relation,
          RealInterval(..), Inclusive(..), 
          ($=), ($<), ($<=), ($>), ($>=), ($^), ($&&), ($||), ($=>), ($<=>), ($.))
import Language.Drasil.Expr.Math (log, sin, cos, tan, sqrt, square, sec, csc, cot, exp,
          dim, idx, int, dbl, str, isin, case_,
          sum_all, defsum, prod_all, defprod,
          real_interval,
          apply, apply1, apply2,
          sy, deriv, pderiv,
          cross, m2x2, vec2D, dgnl2x2, euclidean, defint, int_all)
import Language.Drasil.Expr.Extract (vars)
import Language.Drasil.Output.Formats (DocType(SRS,MG,MIS,Website),DocSpec(DocSpec))
import Language.Drasil.Document (Document(..), DType(..)
  , Section(..), Contents(..), SecCons(..), ListType(..), ItemType(..)
  , section, fig, figWithWidth
  , datadefn, reldefn
  , ListPair)
import Language.Drasil.Unicode -- all of it
import Language.Drasil.UnitLang -- all of it
import Language.Drasil.Unit -- all of it
import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom), Concept, HasSymbol(symbol), HasUnitSymbol(usymb),
  IsUnit, HasAttributes(attributes), CommonIdea(abrv),
  Constrained(constraints), HasReasVal(reasVal), ExprRelat(relat), HasReference(getReferences))
import Language.Drasil.Chunk.AssumpChunk
import Language.Drasil.Chunk.Attribute
import Language.Drasil.Chunk.Attribute.Core (Attributes)
import Language.Drasil.Chunk.Attribute.Derivation (Derivation)
import Language.Drasil.Chunk.Attribute.References (References)
import Language.Drasil.Chunk.Change
import Language.Drasil.Chunk.Citation (
  -- Types
    Citation, BibRef, CiteField, Month(..), HP, CitationKind(..)
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
import Language.Drasil.Chunk.Eq (QDefinition, fromEqn, fromEqn', fromEqn'', getVC, equat, ec)
import Language.Drasil.Chunk.GenDefn
import Language.Drasil.Chunk.Goal (Goal, mkGoal)
import Language.Drasil.Chunk.InstanceModel
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.PhysSystDesc (PhysSystDesc, pSysDes, psd, psd')
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.Relation(RelationConcept, makeRC, makeRC')
import Language.Drasil.Chunk.ReqChunk(ReqChunk, ReqType(..), reqType, requires
                                     , frc, nfrc, rc')
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
import Language.Drasil.Symbol (Symbol(..), sub, sup, vec, hat, prime, sCurlyBrSymb, 
  compsy, Stage(..))
import Language.Drasil.SymbolAlphabet
import Language.Drasil.Misc -- all of it
import Language.Drasil.Printing.Helpers (capitalize, paren, sqbrac)
import Language.Drasil.Generate
import Language.Drasil.People (People, Person, person, HasName(..), manyNames
                               ,person', personWM, personWM', mononym, name)
import Language.Drasil.CodeSpec hiding (outputs, inputs)
import Language.Drasil.DataDesc
import Language.Drasil.Code.Imperative.Lang
import Language.Drasil.RefTypes(RefAdd)