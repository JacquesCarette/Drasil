{- re-export many things to simplify external use -}
module Language.Drasil (
  -- Output.Formats
    DocType(SRS,MG,MIS,LPM,Website)
  -- SystemInformation
  , SystemInformation(..), Block(..)
  -- Recipe
  , Recipe(..)
  -- Expr
  , Expr(C,V,Int,Dbl,IsIn,Deriv,FCall,Grouping,Case)
  , Relation, DerivType(..), RealInterval(..), Inclusive(..)
  , ($=), ($<), ($<=), ($>), ($>=), ($^), ($&&), ($||), ($=>), ($<=>), ($.)
  , log, abs, sin, cos, tan, sec, csc, cot, exp, sqrt, square, euclidean, vars
  , dim, idx
  , sum_all, defsum, prod_all, defprod, defint, int_all
  , cross, m2x2, vec2D, dgnl2x2
  -- all the stuff from Unicode
  , Greek(..), Special(..)
  -- Unit
  , Unit(..), UDefn(..), DerUChunk(..), FundUnit(..), UnitDefn, unitWrapper
  , from_udefn , makeDerU, unitCon
  , (^:), (/:), (*:), new_unit
  -- Chunk
  , Chunk(..), VarChunk(..), ConceptChunk
  , makeVC, makeVC', vc, makeVCObj, SymbolForm
  , dcc, dccWDS, dccWDS', cv, dcc', vc', vc'', ccs, cc, cc', makeVC''
  -- Chunk.Concept
  , Concept, cw, Definition(defn), ConceptDomain(cdom)
  -- Chunk.CommonIdea
  , CommonIdea(..) , commonIdea, CI, getAcc
  -- Chunk.NamedIdea
  , NamedIdea(..), NamedChunk, Idea(..), short, nc, IdeaDict
  , nw -- bad name (historical)
  , compoundterm, for, for', for'', of_, of_', of_'', of__, of'', compoundNC, compoundNC'
  , compoundNC'', compoundNC''', with, with', and_, and_', andRT, aNP, the, a_
  , ofA,theCustom, this
  -- Chunk.Constrained
  , Constrained(..), ConstrainedChunk(..), Constraint(..), ConstrConcept(..)
  , ConstraintReason(..)
  , physc, sfwrc, enumc, constrained, cuc, cvc, constrained', cuc', constrainedNRV'
  , isPhysC, isSfwrC, renderC
  , cnstrw
  , Reason(..), TheoryConstraint(..)
  -- Chunk.Eq
  , QDefinition(..), fromEqn, fromEqn', fromEqn'', getVC, equat
  -- Chunk.Quantity
  , Quantity(..), QuantityDict, qw, ConVar(..), cvR, cvRs
  , symbol
  -- Chunk.UncertainQuantity
  , UncertainQuantity(..), UncertainChunk(..), UncertQ, uq, uqNU, uqc, uqcNU, uqcND, uncrtnChunk, uvc
  , uncrtnw
  -- Chunk.Unital
  , UnitalChunk(..), makeUCWDS, ucFromCV
  , uc, uc', ucs, ucs', ucsWS
  -- Chunk.Unitary
  , Unitary(..), UnitaryChunk, unitary
  -- Chunk.Relation
  , NamedRelation, makeNR, RelationConcept, makeRC, makeRC', relat, ExprRelat
  --Chunk.DefinedQuantity
  , cqs, DefinedQuantityDict
  -- Chunk.UnitaryConcept
  , ucw, UnitaryConceptDict
  -- Chunks w/ Attributes
  , Attribute(..), Attributes, attributes, getSource, aqd -- TODO: Remove aqd
  , HasAttributes, Derivation, getDerivation
  --Citations
  , BibRef, City, State, Citation(..), CiteField(..), Month(..), getAuthors, getYear
  , CitationKind(..)
  -- Spec
  , USymb(..), Sentence(..), Accent(..), sParen, sParenNum, sSqBr, sSqBrNum
  , (+:+), (+:+.), (+.), sC, (+:), semiCol, sParenDash
  , sDash
  -- NounPhrase
  , NounPhrase(..), NP, pn, pn', pn'', pn''', pnIrr, cn, cn', cn'', cn''', cnIP
  , cnIrr, cnIES, cnICES, cnIS, cnUM, nounPhrase, nounPhrase', at_start, at_start'
  , CapitalizationRule(..)
  , PluralRule(..), compoundPhrase, compoundPhrase', compoundPhrase'', compoundPhrase''', titleize, titleize'
  , nounPhrase'', nounPhraseSP, nounPhraseSent
  -- Document
  , LayoutObj(..), Document(..), DType(..), Section(..), Contents(..)
  , SecCons(..), ListType(..), ItemType(..), ListPair
  , section, fig, figWithWidth
  -- Reference
  , makeRef, acroTest
  -- Space
  , Space(..)
  -- Symbol
  , Symbol(..), sub, sup, vec, hat, prime, sCurlyBrSymb
  -- SymbolAlphabet
  , cA, cB, cC, cD, cE, cF, cG, cH, cI, cJ, cK, cL, cM, cN, cO, cP, cQ, cR, cS, cT, cU, cV, cW, cX, cY, cZ
  , lA, lB, lC, lD, lE, lF, lG, lH, lI, lJ, lK, lL, lM, lN, lO, lP, lQ, lR, lS, lT, lU, lV, lW, lX, lY, lZ
  -- Misc
  , mkTable, unit'2Contents, unit_symb, introduceAbb, phrase, plural, phrase's, plural's, unitHidingUnitless
  -- Printing.Helpers
  , capitalize, paren, sqbrac
  -- Generate
  , gen, genCode
  -- People
  , People, Person, person, HasName, name, manyNames, person', personWM
  , personWM', mononym
  -- CodeSpec
  , CodeSpec, codeSpec, codeSpec', Choices(..), ImplementationType(..)
  , Logging(..), ConstraintBehaviour(..), Structure(..), Comments(..)
  , defaultChoices
  , Mod(..), packmod, FuncDef(..), FuncStmt(..), funcDef, fasg, ffor, fdec -- hacks
  , relToQD, funcData, funcQD, Func(..), asExpr, asVC   -- hacks
  -- DataDesc
  , DataDesc
  , entry, listEntry, junk, singleton, junkLine, singleLine, multiLine
  , multiLine', straight, repeated, repeated', Ind(..)
  -- Chunk.Theory
  , Theory(..), tc', TheoryChunk, TheoryModel, tm, tw
  -- Chunk.SymbolForm
  , SymbolChunk, sc, ssc, ssc', Stage(Equational,Implementation), StagedSymbolChunk
  , eqSymb, codeSymb , ssc'', hasStageSymbol
  -- Code.Imperative.Lang
  , Lang(..)
  -- ChunkDB
  , elements
  , ChunkDB(..), cdb
  , HasSymbolTable, symbolMap, symbLookup, getUnitLup, symbolTable
  , HasTermTable, termLookup, termTable
  , HasDefinitionTable, conceptMap, defTable
  , HasUnitTable, unitMap, unitTable
  -- Chunk.GenDefn
  , GenDefn, gd, gdUnit
  -- Chunk.InstanceModel
  , InstanceModel, inCons, outCons, outputs, inputs, im, imQD
) where

import Prelude hiding (log, sin, cos, tan, sqrt, id, return, print, break, exp, product)
import Language.Drasil.SystemInformation
import Language.Drasil.Expr (Expr(..), Relation, DerivType(..), 
          RealInterval(..), Inclusive(..),
          ($=), ($<), ($<=), ($>), ($>=), ($^), ($&&), ($||), ($=>), ($<=>), ($.))
import Language.Drasil.Expr.Math (log, sin, cos, tan, sqrt, square, sec, csc, cot, exp,
          dim, idx,
          sum_all, defsum, prod_all, defprod,
          cross, m2x2, vec2D, dgnl2x2, euclidean, defint, int_all)
import Language.Drasil.Expr.Extract (vars)
import Language.Drasil.Output.Formats (DocType(SRS,MG,MIS,LPM,Website))
import Language.Drasil.Document (LayoutObj(..), Document(..), DType(..)
  , Section(..), Contents(..), SecCons(..), ListType(..), ItemType(..)
  , section, fig, figWithWidth
  , ListPair)
import Language.Drasil.Recipe (Recipe(..))
import Language.Drasil.Unicode -- all of it
import Language.Drasil.Unit -- all of it
import Language.Drasil.Chunk
import Language.Drasil.Chunk.Attribute
import Language.Drasil.Chunk.Attribute.Derivation (Derivation)
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Concept
import Language.Drasil.Chunk.SymbolForm hiding (symbol)
import Language.Drasil.Chunk.CommonIdea
import Language.Drasil.Chunk.VarChunk
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.DefinedQuantity
import Language.Drasil.Chunk.UncertainQuantity
import Language.Drasil.Chunk.ConVar
import Language.Drasil.Chunk.ExprRelat
import Language.Drasil.Chunk.Eq (QDefinition(..), fromEqn, fromEqn', fromEqn'', getVC, equat, aqd)
import Language.Drasil.Chunk.Constrained
import Language.Drasil.Chunk.Theory
import Language.Drasil.Chunk.Unital(UnitalChunk(..), makeUCWDS, ucFromCV
                                  , uc, uc', ucs, ucs', ucsWS)
import Language.Drasil.Chunk.Unitary
import Language.Drasil.Chunk.Relation(NamedRelation, makeNR, RelationConcept, 
                                      makeRC, makeRC')
import Language.Drasil.Chunk.UnitaryConcept
import Language.Drasil.ChunkDB
import Language.Drasil.Citations
import Language.Drasil.NounPhrase hiding (at_start, at_start', titleize
                                          , titleize', phrase, plural)
import Language.Drasil.Space (Space(..))
import Language.Drasil.Spec (USymb(..), Sentence(..), Accent(..), 
                              sParen, sParenNum, sSqBr, sSqBrNum, sC, (+:+), (+:+.), (+.), (+:),
                              semiCol, sParenDash, sDash)
import Language.Drasil.Reference (makeRef, acroTest)
import Language.Drasil.Symbol (Symbol(..), sub, sup, vec, hat, prime, sCurlyBrSymb)
import Language.Drasil.SymbolAlphabet
import Language.Drasil.Misc -- all of it
import Language.Drasil.Printing.Helpers (capitalize, paren, sqbrac)
import Language.Drasil.Generate
import Language.Drasil.People (People, Person, person, HasName(..), manyNames
                               ,person', personWM, personWM', mononym, name)
import Language.Drasil.CodeSpec hiding (outputs, inputs)
import Language.Drasil.DataDesc
import Language.Drasil.Code.Imperative.Lang
import Language.Drasil.Chunk.InstanceModel
import Language.Drasil.Chunk.GenDefn 
