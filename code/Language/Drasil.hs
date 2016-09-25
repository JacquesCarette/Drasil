{- re-export many things to simplify external use -}
module Language.Drasil (
  -- Output.Formats
    DocType(SRS,MG,MIS,LPM,Website)
  -- Recipe
  , Recipe(..)
  -- Expr
  , Expr(..), Relation, UFunc(..), Bound(..), DerivType(..)
  -- all the stuff from Unicode
  , Greek(..), Special(..)
  -- Unit
  , Unit(..), Unit'(..), UDefn(..), DerUChunk(..), FundUnit(..), UnitDefn(..)
  , from_udefn , makeDerU, unitCon
  -- Chunk
  , Chunk(..), VarChunk(..), ConceptChunk(..), makeCC, makeVC
  , descr, Quantity(..)
  -- Chunk.Constrained
  , Constrained, ConstrainedMUC(..), fromMUC
  -- Chunk.Eq
  , EqChunk(..), fromEqn, fromEqn'
  -- Chunk.Unital
  , UnitalChunk(..), makeUC
  -- Chunk.MUChunk
  , MUChunk(..)
  -- Chunk.Relation
  , RelationChunk, makeRC
  -- Chunk.Method
  , MethodChunk, fromEC
  -- Chunk.Module
  , ModuleChunk, makeImpModule, makeUnimpModule
  -- Chunk.Req
  , ReqChunk(..)
  -- Chunk.LC
  , LCChunk(..)
  -- Chunk.Other
  , AssumpChunk, UCChunk
  -- Spec
  , USymb(..), Sentence(..), Accent(..), sMap
  -- Document
  , LayoutObj(..), Document(..), DType(..), Section(..), Contents(..), 
    SecCons(..), ListType(..), ItemType (..)
  -- Reference
  , makeRef
  -- Symbol
  , Symbol(..), sub, sup, vec, hat
  -- SymbolAlphabet
  , cA, cB, cC, cD, cE, cF, cG, cH, cI, cJ, cK, cL, cM, cN, cO, cP, cQ, cR, cS, cT, cU, cV, cW, cX, cY, cZ
  , lA, lB, lC, lD, lE, lF, lG, lH, lI, lJ, lK, lL, lM, lN, lO, lP, lQ, lR, lS, lT, lU, lV, lW, lX, lY, lZ
  -- Misc
  , mkTable, unit'2Contents
  -- Printing.Helpers
  , capitalize, paren, sqbrac

  -- CCode.Import
  , toCodeModule
  -- CCode.AST
  , Lang(CLang), CodeType(Calc)
  -- Template.DD
  , makeDD
  -- Generate
  , gen
) where


import Language.Drasil.Expr (Expr(..), Relation, UFunc(..), Bound(..),DerivType(..))
import Language.Drasil.Output.Formats (DocType(SRS,MG,MIS,LPM,Website))
import Language.Drasil.Document (LayoutObj(..), Document(..), DType(..), 
                                 Section(..), Contents(..), SecCons(..),
                                 ListType(..),ItemType(..))
import Language.Drasil.Recipe (Recipe(..))
import Language.Drasil.Unicode -- all of it
import Language.Drasil.Unit -- all of it
import Language.Drasil.Chunk
import Language.Drasil.Chunk.Eq (EqChunk(..), fromEqn, fromEqn')
import Language.Drasil.Chunk.Constrained (Constrained(..), 
                                          ConstrainedMUC(..),fromMUC)
import Language.Drasil.Chunk.Unital(UnitalChunk(..), makeUC)
import Language.Drasil.Chunk.MUChunk (MUChunk(..))
import Language.Drasil.Chunk.Relation(RelationChunk, makeRC)
import Language.Drasil.Chunk.Req
import Language.Drasil.Chunk.LC
import Language.Drasil.Chunk.Method
import Language.Drasil.Chunk.Module
import Language.Drasil.Chunk.Other
import Language.Drasil.Spec (USymb(..), Sentence(..), Accent(..), sMap)
import Language.Drasil.Reference (makeRef)
import Language.Drasil.Symbol (Symbol(..), sub, sup, vec, hat)
import Language.Drasil.SymbolAlphabet
import Language.Drasil.Misc -- all of it
import Language.Drasil.Printing.Helpers (capitalize, paren, sqbrac)
import Language.Drasil.CCode.Import (toCodeModule)
import Language.Drasil.CCode.AST (Lang(CLang), CodeType(..))
import Language.Drasil.Template.DD
import Language.Drasil.Generate
