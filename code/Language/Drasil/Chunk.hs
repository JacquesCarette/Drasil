{-# OPTIONS -Wall #-}
module Language.Drasil.Chunk where

import Control.Lens

import Language.Drasil.Symbol
import Language.Drasil.Spec
import Language.Drasil.Space
import Prelude hiding (id)

-------- BEGIN CLASSES --------

class Chunk c where
  id :: Simple Lens c String
  
class Chunk c => NamedIdea c where
  term :: Simple Lens c Sentence

--FIXME: Will need to be a "Chunk" not "Concept" after Steven's work is
-- merged into the main branch.
class NamedIdea c => SymbolForm c where
  symbol :: Simple Lens c Symbol
  
-- Placeholder class until SymbolForm has been split from Quantity,
-- Then this will need to be renamed.
-- Necessary for any places which already exist where the 
--  "new" Quantity will be needed
class NamedIdea c => Quantity c where
  typ      :: Simple Lens c Space
--  get_symb :: SymbolForm s => Maybe s --FIXME: Placeholder, see below 
                                            -- (also, will not work as is)
--  get_unit :: Unit u => Maybe u --FIXME: Commented out for now until Steven's
                                      -- work has been merged in.

class NamedIdea c => Concept c where
  defn :: Simple Lens c Sentence
  
-- FIXME: Make this unnecessary
class NamedIdea c => ConceptDefinition' c where
  cdefn' :: Simple Lens c (Maybe Sentence)
-------- BEGIN DATATYPES/INSTANCES --------


data NamedChunk = CC String Sentence 
instance Eq NamedChunk where
  c1 == c2 = (c1 ^. id) == (c2 ^. id)
instance Chunk NamedChunk where
  id f (CC a b) = fmap (\x -> CC x b) (f a)
instance NamedIdea NamedChunk where
  term f (CC a b) = fmap (\x -> CC a x) (f b)

data ConceptChunk = DCC String Sentence Sentence
instance Eq ConceptChunk where
  c1 == c2 = (c1 ^. id) == (c2 ^. id)
instance Chunk ConceptChunk where
  id f (DCC n t d) = fmap (\x -> DCC x t d) (f n)
instance NamedIdea ConceptChunk where
  term f (DCC n t d) = fmap (\x -> DCC n x d) (f t)
instance Concept ConceptChunk where
  defn f (DCC n t d) = fmap (\x -> DCC n t x) (f d)


-- BEGIN VARCHUNK --

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
  
instance Quantity VarChunk where
  typ f (VC n d s t) = fmap (\x -> VC n d s x) (f t)

-- END VARCHUNK --

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
instance Quantity ConVar where
  typ    f (CV c s t) = fmap (\x -> CV c s x) (f t)

--FIXME: This should not be exported.
cvl :: Simple Lens ConVar ConceptChunk
cvl f (CV c s t) = fmap (\x -> CV x s t) (f c)
  
--Helper Function(s)--
--FIXME: Rename things once data structures have been redesigned.
--    Names here are confusing and bad.

makeCC :: String -> String -> NamedChunk
makeCC i des = CC i (S des)

makeDCC, dcc :: String -> String -> String -> ConceptChunk
makeDCC i ter des = DCC i (S ter) (S des)

dcc = makeDCC

--Currently only used by RelationChunk and EqChunk
ccWithDescrSent :: String -> Sentence -> NamedChunk
ccWithDescrSent n d = CC n d

dccWDS :: String -> String -> Sentence -> ConceptChunk
dccWDS i t d = DCC i (S t) d

-- For when name = descr (will likely become deprecated as the chunks become more descriptive).
nCC :: String -> NamedChunk 
nCC n = makeCC n n

-- the code generation system needs VC to have a type (for now)
-- Setting all varchunks to have Rational type so it compiles
makeVC :: String -> String -> Symbol -> VarChunk
makeVC i des sym = VC i (S des) sym Rational

vcFromCC :: NamedChunk -> Symbol -> VarChunk
vcFromCC cc sym = VC (cc ^. id) (cc ^. term) sym Rational

cv :: ConceptChunk -> Symbol -> Space -> ConVar
cv = CV

--FIXME: Remove this hack
cvR :: ConceptChunk -> Symbol -> ConVar
cvR c s = CV c s Rational