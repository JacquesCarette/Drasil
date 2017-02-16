{-# Language GADTs, Rank2Types #-}
module Language.Drasil.Chunk where

import Control.Lens (Simple,Lens,(^.), set)

import Language.Drasil.Symbol
import Language.Drasil.Spec
import Language.Drasil.Space
import Prelude hiding (id)

--FIXME: Rename smart constructors and whatnot
-- once data structures have been redesigned.
--    Names here are confusing and bad.


-------- BEGIN CLASSES --------

class Chunk c where
  id :: Simple Lens c String
  
class Chunk c => NamedIdea c where
  term :: Simple Lens c Sentence
  getA :: c -> Maybe Sentence
  --Get Abbreviation/Acronym? These might need to be separated 
  --depending on contexts, but for now I don't see a problem with it.
  
class Chunk c => SymbolForm c where
  symbol :: Simple Lens c Symbol
 
class NamedIdea c => Concept c where
  defn :: Simple Lens c Sentence



-------- BEGIN DATATYPES/INSTANCES --------

{-==============================
==== SF
==============================-}
data SF where 
  SF :: SymbolForm c => c -> SF
instance Chunk SF where
  id = sfl id
instance SymbolForm SF where
  symbol = sfl symbol
instance Eq SF where
  (SF s1) == (SF s2) = (s1 ^. id) == (s2 ^. id)

sfl :: (forall c. (SymbolForm c) => Simple Lens c a) -> Simple Lens SF a
sfl l f (SF a) = fmap (\x -> SF (set l x a)) (f (a ^. l))


{-==============================
==== NamedChunk
==============================-}

data NamedChunk = NC String Sentence (Maybe Sentence)
instance Eq NamedChunk where
  c1 == c2 = (c1 ^. id) == (c2 ^. id)
instance Chunk NamedChunk where
  id f (NC a b c) = fmap (\x -> NC x b c) (f a)
instance NamedIdea NamedChunk where
  term f (NC a b c) = fmap (\x -> NC a x c) (f b)
  getA (NC a b c) = c
  
nc :: String -> String -> NamedChunk
nc i des = NC i (S des) Nothing

nc' :: String -> String -> Sentence -> NamedChunk
nc' i t acc = NC i (S t) (Just acc)

--Currently only used by RelationChunk and EqChunk
ncWDS :: String -> Sentence -> NamedChunk
ncWDS n d = NC n d Nothing

ncWDS' :: String -> Sentence -> Sentence -> NamedChunk
ncWDS' i t a = NC i t (Just a)

{-==============================
==== ConceptChunk
==============================-}
data ConceptChunk = DCC String Sentence Sentence
instance Eq ConceptChunk where
  c1 == c2 = (c1 ^. id) == (c2 ^. id)
instance Chunk ConceptChunk where
  id f (DCC n t d) = fmap (\x -> DCC x t d) (f n)
instance NamedIdea ConceptChunk where
  term f (DCC n t d) = fmap (\x -> DCC n x d) (f t)
instance Concept ConceptChunk where
  defn f (DCC n t d) = fmap (\x -> DCC n t x) (f d)
  
makeDCC, dcc :: String -> String -> String -> ConceptChunk
makeDCC i ter des = DCC i (S ter) (S des)

dcc = makeDCC

dccWDS :: String -> String -> Sentence -> ConceptChunk
dccWDS i t d = DCC i (S t) d

ccStSS :: String -> Sentence -> Sentence -> ConceptChunk
ccStSS i t d = DCC i t d



{-==============================
==== VARCHUNK
==============================-}

-- the code generation system needs VC to have a type (for now)
-- I added vtyp so that it compiles
data VarChunk = VC { vid :: String
                   , vdesc :: Sentence
                   , vsymb :: Symbol
                   , vtyp  :: Space }

instance Eq VarChunk where
  c1 == c2 = (c1 ^. id) == (c2 ^. id)

instance Chunk VarChunk where
  id f (VC n d s t) = fmap (\x -> VC x d s t) (f n)

instance NamedIdea VarChunk where
  term f (VC n d s t) = fmap (\x -> VC n x s t) (f d)

instance SymbolForm VarChunk where
  symbol f (VC n d s t) = fmap (\x -> VC n d x t) (f s)
  
-- the code generation system needs VC to have a type (for now)
-- Setting all varchunks to have Rational type so it compiles
makeVC :: String -> String -> Symbol -> VarChunk
makeVC i des sym = VC i (S des) sym Rational

makeVCObj :: String -> String -> Symbol -> String -> VarChunk
makeVCObj i des sym s = VC i (S des) sym (Obj s)

vcFromCC :: NamedChunk -> Symbol -> VarChunk
vcFromCC cc sym = VC (cc ^. id) (cc ^. term) sym Rational

{-==============================
==== ConVar
==============================-}
  
--FIXME: This is a temporary data structure created to advance the chunk
--  hierarchy redesign. A full overhaul of datastructures is coming soon.

data ConVar = CV { _con :: ConceptChunk
                 , _symb :: Symbol
                 , _typ :: Space }
                     
instance Eq ConVar where
  c1 == c2 = (c1 ^. id) == (c2 ^. id)
instance Chunk ConVar where
  id = cvl . id
instance NamedIdea ConVar where
  term = cvl . term
instance Concept ConVar where
  defn = cvl . defn
instance SymbolForm ConVar where
  symbol f (CV c s t) = fmap (\x -> CV c x t) (f s)

--FIXME: This should not be exported.
cvl :: Simple Lens ConVar ConceptChunk
cvl f (CV c s t) = fmap (\x -> CV x s t) (f c)

cv :: ConceptChunk -> Symbol -> Space -> ConVar
cv = CV

--FIXME: Remove this hack
cvR :: ConceptChunk -> Symbol -> ConVar
cvR c s = CV c s Rational

----------------------
-- various combinators
compoundterm :: (NamedIdea c, NamedIdea d) => c -> d -> NamedChunk
compoundterm t1 t2 = NC (t1^.id ++ t2^.id) ((t1^.term) +:+ (t2^.term)) Nothing
