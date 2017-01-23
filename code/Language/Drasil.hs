{- re-export many things to simplify external use -}
module Language.Drasil (
  -- Output.Formats
    DocType(SRS,MG,MIS,LPM,Website)
  -- Recipe
  , Recipe(..)
  -- Expr
  , Expr(..), Relation, UFunc(..), BiFunc(..), Bound(..), DerivType(..)
  , log, abs, sin, cos, tan, sec, csc, cot
  -- all the stuff from Unicode
  , Greek(..), Special(..)
  -- Unit
  , Unit(..), UDefn(..), DerUChunk(..), FundUnit(..), UnitDefn(..)
  , from_udefn , makeDerU, unitCon
  , (^:), (/:), (*:), new_unit
  -- Chunk
  , Chunk(..), VarChunk(..), NamedChunk(..), ConceptChunk(..), makeCC, ncWDS, makeVC
  , makeVCObj, vcFromCC, nCC, makeDCC, SymbolForm(..), dcc, dccWDS, cv, ccStSS
  , Quantity(..), ConVar(..), cvR, NamedIdea(..)
  , Concept(..)
  -- Chunk.Constrained
  , Constrained(..)
  -- Chunk.Eq
  , QDefinition(..), fromEqn, fromEqn', getVC
  -- Chunk.Unital
  , UnitalChunk(..), makeUC, makeUCWDS, ucFromVC
  -- Chunk.Relation
  , NamedRelation, makeNR, RelationConcept, makeRC
  -- Chunk.Method
  , MethodChunk, fromEC, makeStdInputMethod, makeFileInputMethod
  , makeFileOutputMethod, makeMainMethod
  -- Chunk.Module
  , ModuleChunk, makeRecord, makeImpModule, makeImpModuleNoGen, makeUnimpModule
  -- Chunk.Req
  , ReqChunk(..)
  -- Chunk.LC
  , LCChunk(..)
  -- Chunk.Other
  , AssumpChunk, UCChunk
  --Chunk.Wrapper
  , cqs, qs, nw, CQSWrapper, QSWrapper, NWrapper
  -- Spec
  , USymb(..), Sentence(..), Accent(..), sMap, sLower, sParen, (+:+), (+:+.), sC
  -- Document
  , LayoutObj(..), Document(..), DType(..), Section(..), Contents(..), 
    SecCons(..), ListType(..), ItemType(..)
  -- Reference
  , makeRef
  -- Space
  , Space(..)
  -- Symbol
  , Symbol(..), sub, sup, vec, hat
  -- SymbolAlphabet
  , cA, cB, cC, cD, cE, cF, cG, cH, cI, cJ, cK, cL, cM, cN, cO, cP, cQ, cR, cS, cT, cU, cV, cW, cX, cY, cZ
  , lA, lB, lC, lD, lE, lF, lG, lH, lI, lJ, lK, lL, lM, lN, lO, lP, lQ, lR, lS, lT, lU, lV, lW, lX, lY, lZ
  -- Misc
  , mkTable, unit'2Contents
  -- Printing.Helpers
  , capitalize, paren, sqbrac
  -- Template.DD
  , makeDD
  -- Generate
  , gen, genCode

  -- exposing GOOL AST temporarily
  , Label
  , Body, Block(..), Statement(..), Pattern(..), StatePattern(..)
  , StratPattern(..), Strategies(..), ObserverPattern(..), Assignment(..)
  , Declaration(..), Conditional(..), Iteration(..), Exception(..), Jump(..)
  , Return(..), Value(..), Comment(..), Literal(..), Function(..)
  , Expression(..), BinaryOp(..), StateType(..)
  , Permanence(..), Scope(..), Parameter(..), StateVar(..)
  , Method(..), Enum(..), Class(..), Package(..), AbstractCode(..), bool,int
  ,float,char,string,infile,outfile,defaultValue,true,false
  ,pubClass,privClass,privMVar,pubMVar,pubGVar,privMethod,pubMethod
  ,(?!),(?<),(?<=),(?>),(?>=),(?==),(?!=),(#~),(#/^),(#|),(#+),(#-),(#*),(#/)
  ,(#%),(#^),(&=),(&.=),(&=.),(&+=),(&-=),(&++),(&~-),($->),($.),($:)
  ,alwaysDel,neverDel
  ,assign,at,binExpr,break,cast,constDecDef,extends,for,forEach,ifCond,ifExists
  ,listDec,listDecValues,listOf,litBool,litChar,litFloat,litInt,litObj
  ,litString,noElse,noParent,objDecDef,oneLiner,param,params,print,printLn
  ,printStr,printStrLn,printFile,printFileLn,printFileStr,printFileStrLn,return
  ,returnVar,switch,throw,tryCatch,varDec,varDecDef,while,zipBlockWith
  ,zipBlockWith4,addComments,comment,commentDelimit,endCommentDelimit
  ,prefixFirstBlock,getterName,setterName,convertToClass,convertToMethod
  ,bodyReplace,funcReplace,valListReplace
) where

import Prelude hiding (log, abs, sin, cos, tan, id, return, print, break)
import Language.Drasil.Expr (Expr(..), Relation, UFunc(..), BiFunc(..), 
               Bound(..),DerivType(..), log, abs, sin, cos, tan, sec, csc, cot)
import Language.Drasil.Output.Formats (DocType(SRS,MG,MIS,LPM,Website))
import Language.Drasil.Document (LayoutObj(..), Document(..), DType(..), 
                                 Section(..), Contents(..), SecCons(..),
                                 ListType(..),ItemType(..))
import Language.Drasil.Recipe (Recipe(..))
import Language.Drasil.Unicode -- all of it
import Language.Drasil.Unit -- all of it
import Language.Drasil.Chunk
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.Eq (QDefinition(..), fromEqn, fromEqn', getVC)
import Language.Drasil.Chunk.Constrained --INSTANCES TO BE IMPLEMENTED SOON
import Language.Drasil.Chunk.Unital(UnitalChunk(..), makeUC, makeUCWDS, ucFromVC)
import Language.Drasil.Chunk.Relation(NamedRelation, makeNR, RelationConcept, makeRC)
import Language.Drasil.Chunk.Req
import Language.Drasil.Chunk.LC
import Language.Drasil.Chunk.Method
import Language.Drasil.Chunk.Module
import Language.Drasil.Chunk.Other
import Language.Drasil.Chunk.Wrapper
import Language.Drasil.Space (Space(..))
import Language.Drasil.Spec (USymb(..), Sentence(..), Accent(..), 
                              sMap, sLower, sParen, sC, (+:+), (+:+.))
import Language.Drasil.Reference (makeRef)
import Language.Drasil.Symbol (Symbol(..), sub, sup, vec, hat)
import Language.Drasil.SymbolAlphabet
import Language.Drasil.Misc -- all of it
import Language.Drasil.Printing.Helpers (capitalize, paren, sqbrac)
import Language.Drasil.Template.DD
import Language.Drasil.Generate
import Language.Drasil.Code.Imperative.AST hiding (BaseType(..), typ)
