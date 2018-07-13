module Language.Drasil.Document.Extract (getDoc, egetDoc)where

import Control.Lens ((^.))
import Data.List(transpose)

import Language.Drasil.Document
import Language.Drasil.Expr
import Language.Drasil.Spec
import Language.Drasil.NounPhrase 
import Language.Drasil.NounPhrase.Core

import Language.Drasil.Chunk.AssumpChunk
import Language.Drasil.Chunk.ShortName
import Language.Drasil.Chunk.Change
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.Citation
import Language.Drasil.Chunk.ReqChunk
import Language.Drasil.Chunk.Eq (QDefinition)
import Language.Drasil.Chunk.References

import Language.Drasil.Development.Unit(UnitDefn)

import Language.Drasil.Classes (NamedIdea(term),
  ExprRelat(relat), HasDerivation(derivations), 
  HasReference(getReferences), Definition(defn))


egetDoc :: Document -> [Expr]
egetDoc (Document _ _ s) = concatMap egetSec s

-- If collected sentences are used for collecting symbols, 
-- section collection has to avoid Reference Material section (named "RefMat"),
-- because this section includes Table of symbol which would
-- cause loop in collecting.

-- Auxiliary Constants Section (named "AuxConstants") contains standard 
-- values (like min, max) that are used for defined basic Chunk.
-- These values should not appear in the basic Table of symbol.
egetSec :: Section -> [Expr]
egetSec (Section _ _ _) = []
egetSec (Section _ sc _) = concatMap egetSecCon sc

egetSecCon :: SecCons -> [Expr]
egetSecCon (Sub s) = egetSec s
egetSecCon (Con c) = egetCon c

egetCon :: Contents -> [Expr]
egetCon (EqnBlock e _) = [e]
egetCon (Definition d) = egetDtype d 
egetCon (Defnt dt (hd:tl) a) = concatMap egetCon (snd hd) ++ egetCon (Defnt dt tl a)
egetCon (Defnt dt [] _) = [] ++ egetDtype dt
egetCon _ = []

egetDtype :: DType -> [Expr]
egetDtype (Data q) = egetQDef q
egetDtype (Theory t) = [t ^. relat]
egetDtype _ = []

egetQDef :: QDefinition -> [Expr]
egetQDef a = [a ^. relat]


getDoc :: Document -> [Sentence]
getDoc (Document t a s) = t : a : concatMap getSec s

getSec :: Section -> [Sentence]
getSec (Section _ _ _) = []
getSec (Section _ _ _) = []
getSec (Section t sc _) = t : concatMap getSecCon sc

getSecCon :: SecCons -> [Sentence]
getSecCon (Sub s) = getSec s
getSecCon (Con c) = getCon c

-- This function is used in collecting sentence from table.
-- Since only the table's first Column titled "Var" should be collected,
-- this function is used to filter out only the first Column of Sentence. 
isVar :: ([Sentence], [[Sentence]]) -> [Sentence]
isVar (S "Var" : _, hd1 : _) = hd1
isVar (_ : tl, _ : tl1) = isVar (tl, tl1)
isVar ([], _) = []
isVar (_, []) = []

getCon :: Contents -> [Sentence]
getCon (Table s1 s2 t _ _) = isVar (s1, transpose s2) ++ [t]
getCon (Paragraph s)       = [s]
getCon (EqnBlock _ _)      = []
getCon (Definition d)      = getDtype d
getCon (Enumeration lst)   = getLT lst
getCon (Figure l _ _ _)    = [l]
getCon (Requirement reqc)  = getReq reqc
getCon (Assumption assc)   = getAss assc
getCon (Change chg)        = getChg chg
getCon (Bib bref)          = getBib bref
getCon (Graph [(s1, s2)] _ _ l _) = s1 : s2 : [l]
getCon (Defnt dt (hd:fs) a) = concatMap getCon (snd hd) ++ getCon (Defnt dt fs a)
getCon (Defnt _ [] _) = []
getCon  _ = []


getDtype :: DType -> [Sentence]
getDtype (Data q) = getQDef q
getDtype (Theory t) = getTerm t ++ getDefn t
getDtype _ = []


getQDef :: QDefinition -> [Sentence]
getQDef a = concatMap getRef (a ^. getReferences) ++ (a ^. derivations) ++ getTerm a ++ getUnitD (getUnit a)

getRef :: Reference -> [Sentence]
getRef (SourceRef s) = [s]

getNP :: NP -> [Sentence]
getNP (ProperNoun _ _) = []
getNP (CommonNoun _ _ c) = getCap c
getNP (Phrase s _ c1 c2) = s : getCap c1 ++ getCap c2

getCap :: CapitalizationRule -> [Sentence]
getCap (Replace s) = [s]
getCap  _ = []

getUnitD :: Maybe UnitDefn -> [Sentence]
getUnitD Nothing = []
getUnitD (Just a) = getTerm a ++ getDefn a

getTerm :: (NamedIdea a) => a -> [Sentence]
getTerm a  = getNP (a ^. term)

getDefn :: (Definition a) => a -> [Sentence]
getDefn a = [a ^. defn]

getReq :: ReqChunk -> [Sentence]
getReq a = [requires a]

getAss :: AssumpChunk -> [Sentence]
getAss a = [assuming a]

getChg :: Change -> [Sentence]
getChg a = [chng a]

getLT :: ListType -> [Sentence]
getLT (Bullet it) = concatMap getIL it
getLT (Numeric it) = concatMap getIL it
getLT (Simple lp) = concatMap getLP lp
getLT (Desc lp) = concatMap getLP lp
getLT (Definitions lp) = concatMap getLP lp


getIL :: ItemType -> [Sentence]
getIL (Flat s) = [s]
getIL (Nested h lt) = h : getLT lt

getLP :: ListPair -> [Sentence]
getLP (t, it) = t : getIL it

getBib :: (HasFields c) => [c] -> [Sentence]
getBib a = concatMap getField $ concatMap (^. getFields) a

getField :: CiteField -> [Sentence]
getField (Address s) = [s]
getField (BookTitle s) = [s]
getField (Institution s) = [s]
getField (Journal s) = [s]
getField (Note s) = [s]
getField (Organization s) = [s]
getField (Publisher s) = [s]
getField (School s) = [s]
getField (Series s) = [s]
getField (Title s) = [s]
getField (Type s) = [s]
getField _ = []
