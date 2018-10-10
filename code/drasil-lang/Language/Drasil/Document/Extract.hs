module Language.Drasil.Document.Extract (getDoc, egetDoc, egetSec, egetCon', egetLblCon, egetQDef) where

import Control.Lens ((^.))
import Data.List(transpose)

import Language.Drasil.Classes (DefiningExpr(defnExpr))
import Language.Drasil.Document (Document(Document),Section(Section), SecCons(..))
import Language.Drasil.Document.Core
import Language.Drasil.Expr
import Language.Drasil.Spec

import Language.Drasil.Chunk.AssumpChunk
import Language.Drasil.Chunk.ShortName
import Language.Drasil.Chunk.Change
import Language.Drasil.Chunk.Citation
import Language.Drasil.Chunk.ReqChunk
import Language.Drasil.Chunk.Eq (QDefinition)
import Language.Drasil.RefTypes(DType(..))

import Language.Drasil.Label (Label, mkLabelRASec)

egetLblCon :: LabelledContent -> [Expr]
egetLblCon a = egetCon (a ^. accessContents)

egetDoc :: Document -> [Expr]
egetDoc (Document _ _ s) = concatMap egetSec s

-- If collected sentences are used for collecting symbols,
-- section collection has to avoid Reference Material section (named "RefMat"),
-- because this section includes Table of symbol which would
-- cause loop in collecting.

-- Auxiliary Constants Section (named "AuxConstants") contains standard
-- values (like min, max) that are used for defined basic Chunk.
-- These values should not appear in the basic Table of symbol.
refLabel :: Label
refLabel = mkLabelRASec "RefMat" "Reference Material" -- FIXME: HACKED IN HERE
auxConsLabel :: Label
auxConsLabel = mkLabelRASec "AuxConstants" "Values of Auxiliary Constants" -- FIXME: HACKED IN HERE
--FIXME: Remove the above labels when we have a less fragile way of checking things.

egetSec :: Section -> [Expr]
egetSec (Section _ sc lb) = concatMap egetSecCon sc
  {--| lb ^. shortname == refLabel ^. shortname = []
  | otherwise = concatMap egetSecCon sc --}

egetSecCon :: SecCons -> [Expr]
egetSecCon (Sub s) = egetSec s
egetSecCon (Con c) = egetCon' c

egetCon' :: Contents -> [Expr]
egetCon' c = egetCon (c ^. accessContents)

egetCon :: RawContent -> [Expr]
egetCon (EqnBlock e) = [e]
egetCon (Definition dt (hd:tl)) = concatMap egetCon' (snd hd) ++ egetCon (Definition dt tl)
egetCon (Definition dt []) = [] ++ egetDtype dt
egetCon _ = []

egetDtype :: DType -> [Expr]
egetDtype _ = []

egetQDef :: QDefinition -> [Expr]
egetQDef q = [q ^. defnExpr]

getDoc :: Document -> [Sentence]
getDoc (Document t a s) = t : a : concatMap getSec s

getSec :: Section -> [Sentence]
getSec (Section t sc lb) = t : concatMap getSecCon sc
 {--| lb ^. shortname == refLabel ^. shortname = []
  | lb ^. shortname == auxConsLabel ^. shortname = []
  | otherwise = t : concatMap getSecCon sc--}

getSecCon :: SecCons -> [Sentence]
getSecCon (Sub s) = getSec s
getSecCon (Con c) = getCon' c

-- This function is used in collecting sentence from table.
-- Since only the table's first Column titled "Var" should be collected,
-- this function is used to filter out only the first Column of Sentence.
isVar :: ([Sentence], [[Sentence]]) -> [Sentence]
isVar (S "Var" : _, hd1 : _) = hd1
isVar (_ : tl, _ : tl1) = isVar (tl, tl1)
isVar ([], _) = []
isVar (_, []) = []

getCon' :: Contents -> [Sentence]
getCon' c = getCon (c ^. accessContents)

getCon :: RawContent -> [Sentence]
getCon (Table s1 s2 t _) = isVar (s1, transpose s2) ++ [t]
getCon (Paragraph s)       = [s]
getCon (EqnBlock _)      = []
getCon (Enumeration lst)   = getLT lst
getCon (Figure l _ _)    = [l]
getCon (Requirement reqc)  = getReq reqc
getCon (Assumption assc)   = getAss assc
getCon (Change chg)        = getChg chg
getCon (Bib bref)          = getBib bref
getCon (Graph [(s1, s2)] _ _ l) = s1 : s2 : [l]
getCon (Definition dt (hd:fs)) = concatMap getCon' (snd hd) ++ getCon (Definition dt fs)
getCon (Definition _ []) = []
getCon  _ = []

{-
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
-}

getReq :: ReqChunk -> [Sentence]
getReq a = [requires a]

getAss :: AssumpChunk -> [Sentence]
getAss a = [assuming a]

getChg :: Change -> [Sentence]
getChg a = [chng a]

getLT :: ListType -> [Sentence]
getLT (Bullet it) = concatMap getIL $ map fst it
getLT (Numeric it) = concatMap getIL $ map fst it
getLT (Simple lp) = concatMap getLP lp
getLT (Desc lp) = concatMap getLP lp
getLT (Definitions lp) = concatMap getLP lp


getIL :: ItemType -> [Sentence]
getIL (Flat s) = [s]
getIL (Nested h lt) = h : getLT lt

getLP :: ListTuple -> [Sentence]
getLP (t, it, _) = t : getIL it

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
