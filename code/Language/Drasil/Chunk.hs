{-# OPTIONS -Wall #-}
module Language.Drasil.Chunk where

import Control.Lens

import Language.Drasil.Symbol
import Language.Drasil.Spec
import Language.Drasil.Space

-------- BEGIN CLASSES --------

-- BEGIN CHUNK --
-- a chunk has a name
class Chunk c where
  name :: Simple Lens c String
-- END CHUNK --
  
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

-- BEGIN CONCEPTDEFINITION --
-- Used for so called "verbose" concepts which have both a short name (term)
-- And long description.
class NamedIdea c => ConceptDefinition c where
  cdefn :: Simple Lens c Sentence
  
class NamedIdea c => ConceptDefinition' c where
  cdefn' :: Simple Lens c (Maybe Sentence)
-------- BEGIN DATATYPES/INSTANCES --------

-- BEGIN CONCEPTCHUNK --
--Equivalent to a "term" concept
data ConceptChunk = CC String Sentence 
instance Eq ConceptChunk where
  c1 == c2 = (c1 ^. name) == (c2 ^. name)
instance Chunk ConceptChunk where
  name f (CC a b) = fmap (\x -> CC x b) (f a)
instance NamedIdea ConceptChunk where
  term f (CC a b) = fmap (\x -> CC a x) (f b)
-- END CONCEPTCHUNK --

-- BEGIN DEFINEDTERM --
-- DefinedTerm = DCC Name   Term    Definition
data DefinedTerm = DCC String Sentence Sentence
instance Eq DefinedTerm where
  c1 == c2 = (c1 ^. name) == (c2 ^. name)
instance Chunk DefinedTerm where
  name f (DCC n t d) = fmap (\x -> DCC x t d) (f n)
instance NamedIdea DefinedTerm where
  term f (DCC n t d) = fmap (\x -> DCC n x d) (f t)
instance ConceptDefinition DefinedTerm where
  cdefn f (DCC n t d) = fmap (\x -> DCC n t x) (f d)


-- BEGIN VARCHUNK --

-- the code generation system needs VC to have a type (for now)
-- I added vtyp so that it compiles
data VarChunk = VC { vname :: String
                   , vdesc :: Sentence
                   , vsymb :: Symbol
                   , vtyp  :: Space }

instance Eq VarChunk where
  c1 == c2 = (c1 ^. name) == (c2 ^. name)

instance Chunk VarChunk where
  name f (VC n d s t) = fmap (\x -> VC x d s t) (f n)

instance NamedIdea VarChunk where
  term f (VC n d s t) = fmap (\x -> VC n x s t) (f d)

instance SymbolForm VarChunk where
  symbol f (VC n d s t) = fmap (\x -> VC n d x t) (f s)
  
instance Quantity VarChunk where
  typ f (VC n d s t) = fmap (\x -> VC n d s x) (f t)

-- END VARCHUNK --


--Helper Function(s)--

makeCC :: String -> String -> ConceptChunk
makeCC nam des = CC nam (S des)

makeDCC :: String -> String -> String -> DefinedTerm
makeDCC nam ter des = DCC nam (S ter) (S des)

--Currently only used by RelationChunk and EqChunk
ccWithDescrSent :: String -> Sentence -> ConceptChunk
ccWithDescrSent n d = CC n d

-- For when name = descr (will likely become deprecated as the chunks become more descriptive).
nCC :: String -> ConceptChunk 
nCC n = makeCC n n

-- the code generation system needs VC to have a type (for now)
-- Setting all varchunks to have Rational type so it compiles
makeVC :: String -> String -> Symbol -> VarChunk
makeVC nam des sym = VC nam (S des) sym Rational

vcFromCC :: ConceptChunk -> Symbol -> VarChunk
vcFromCC cc sym = VC (cc ^. name) (cc ^. term) sym Rational