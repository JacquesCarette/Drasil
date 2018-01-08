{-# LANGUAGE GADTs,Rank2Types #-}
module Language.Drasil.Chunk.Attribute 
  ( Attribute(..), Attributes, HasAttributes(..)
  , getSource, getDerivation, getShortName
  ) where

import Control.Lens (Simple, Lens, (^.))
import Language.Drasil.Spec (Sentence(EmptyS))
import Language.Drasil.Chunk
import Language.Drasil.Chunk.Attribute.Derivation

import Prelude hiding (id)

-- | Attributes are just a list of 'Attribute'
type Attributes = [Attribute]

-- | An attribute can be a rationale, a reference to the source (we used) to find
-- this knowledge, or a derivation to show how we arrived 
-- at a given model/definition/etc.
data Attribute = Rationale Sentence
               | ShortName Sentence
               | SourceRef Sentence -- Source to reference for this knowledge chunk
                                    -- FIXME: Allow URLs/Citations here
               | D Derivation -- Makes sense for now (derivations are just document sections at the moment), but we may need to create a new
               -- representation for it in the future.
               -- To collapse Attributes into QDefinitions, can't use Contents

-- Should this get only the first one or all potential sources?
-- Should we change the source ref to have a list (to keep things clean in case
--    of multiple sources)?
-- | Get the source reference from the attributes (if it exists)
getSource :: HasAttributes c => c -> Sentence
getSource c = sourceRef $ c ^. attributes

sourceRef :: Attributes -> Sentence
sourceRef []                = EmptyS
sourceRef ((SourceRef x):_) = x
sourceRef (_:xs)            = sourceRef xs

getDerivation :: HasAttributes c => c -> Derivation
getDerivation c = derivation $ c ^. attributes

derivation :: Attributes -> Derivation
derivation []          = []
derivation ((D der):_) = der
derivation (_:xs)      = derivation xs

getShortName :: HasAttributes c => c -> Maybe Sentence
getShortName c = shortName $ c ^. attributes

shortName :: Attributes -> Maybe Sentence
shortName [] = Nothing
shortName ((ShortName s):_) = Just s
shortName (_:xs) = shortName xs

-- | Any chunk with 'Attributes' is part of the 'HasAttributes' class.
class Chunk c => HasAttributes c where
  attributes :: Simple Lens c Attributes

-- | A QDefinition (Quantity) with attributes is an 'AttribQDef'
-- This is a temporary data structure to be used until attributes are fully
-- functional, then it will be fused with QDefinition
{-
data AttribQDef where
  AQD :: QDefinition -> Attributes -> AttribQDef
  
instance Chunk AttribQDef where
  id = qdl . id
instance NamedIdea AttribQDef where
  term = qdl . term
  getA (AQD q _) = getA q
instance Quantity AttribQDef where
  typ = qdl . typ
  getSymb s (AQD q _) = getSymb s q
  getStagedS (AQD q _) = getStagedS q
  getUnit (AQD q _) = getUnit q
instance Eq AttribQDef where
  a == b = (a ^. id) == (b ^. id)

instance HasAttributes AttribQDef where
  attributes f (AQD a b) = fmap (\x -> AQD a x) (f b)
  
qdef :: AttribQDef -> QDefinition
qdef (AQD q _) = q

aqd :: QDefinition -> Attributes -> AttribQDef
aqd = AQD

-- DO NOT EXPORT
qdl :: Simple Lens AttribQDef QDefinition
qdl f (AQD a b) = fmap (\x -> AQD x b) (f a)
-}