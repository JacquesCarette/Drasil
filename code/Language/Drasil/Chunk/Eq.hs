{-# LANGUAGE GADTs, Rank2Types #-}
module Language.Drasil.Chunk.Eq 
  (QDefinition(..), fromEqn, fromEqn', fromEqn'', equat, getVC
  , ec, ec', aqd) where

import Control.Lens (Simple, Lens, set, (^.))
import Prelude hiding (id)
import Language.Drasil.Expr (Expr)
import Language.Drasil.Chunk
import Language.Drasil.Chunk.Attribute
import Language.Drasil.Chunk.NamedIdea (NamedIdea(..), Idea(..))
import Language.Drasil.Chunk.Concept
import Language.Drasil.Chunk.ConVar
import Language.Drasil.Chunk.Quantity (Quantity(..),HasSpace(typ))
import Language.Drasil.Chunk.ExprRelat
import Language.Drasil.Chunk.VarChunk (VarChunk, vcSt)
import Language.Drasil.Chunk.Unital (ucFromCV)
import Language.Drasil.Unit (Unit(..))
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Space

import Language.Drasil.NounPhrase (NP, phrase)
import Language.Drasil.Spec

-- BEGIN EQCHUNK --
-- | A QDefinition is a 'Quantity' with a defining equation.
data QDefinition where
  EC :: (Quantity c) => c -> Expr -> Attributes -> QDefinition

--Removed named record fields, so we want to not break things for now.
-- | Returns the defining equation of a 'QDefinition'
equat :: QDefinition -> Expr 
equat (EC _ b _) = b
  
-- this works because UnitalChunk is a Chunk
instance Chunk QDefinition where
  id = ul . id

instance NamedIdea QDefinition where
  term = ul . term

instance Idea QDefinition where
  getA c = getA $ c ^. ul

instance HasSpace QDefinition where
  typ = ul . typ
instance Quantity QDefinition where
  getSymb s (EC a _ _)  = getSymb s a
  getUnit (EC a _ _)    = getUnit a
  getStagedS (EC a _ _) = getStagedS a
  -- DO SOMETHING
  
instance ExprRelat QDefinition where
  relat f (EC a b c) = fmap (\x -> EC a x c) (f b)
  
instance HasAttributes QDefinition where
  attributes f (EC a b c) = fmap (\x -> EC a b x) (f c)
  
{-instance Unit' QDefinition where
  unit' = ul . unit'-}
-- END EQCHUNK --

-- don't export this
ul :: Simple Lens QDefinition H
ul f (EC a b c) = fmap (\(H x) -> EC x b c) (f (H a))

-- or this
elens :: (forall c. (Quantity c) => Simple Lens c a) 
  -> Simple Lens H a
elens l f (H a) = fmap (\x -> H (set l x a)) (f (a ^. l))

-- and especially not this
data H where
  H :: (Quantity c) => c -> H

instance Chunk H where
  id = elens id

instance NamedIdea H where
  term = elens term

instance Idea H where
  getA (H a) = getA a
  
instance HasSpace H where
  typ = elens typ
instance Quantity H where
  getSymb s  (H c) = getSymb s c
  getUnit    (H c) = getUnit c
  getStagedS (H c) = getStagedS c
  
-- useful: to be used for equations with units
--FIXME: Space hack
--TODO: Create a version which doesn't use ConVar, but instead only 
--     NamedIdeas if we decide the "new" unital needs only a named idea

-- | Create a 'QDefinition' with an id, noun phrase (term), symbol,
-- unit, and defining equation.
fromEqn :: Unit u => String -> NP -> Sentence -> Symbol -> u -> Expr -> QDefinition
fromEqn nm desc _ symb chunk eqn = 
  EC (ucFromCV (cv (dccWDS nm (desc) (phrase desc)) symb Rational) chunk) eqn 
    ([] :: Attributes)
  --EC (ucFromCV (cv (dccWDS nm (desc) (phrase desc +:+ extra)) symb Rational) chunk) eqn

-- and without
--FIXME: Space hack
-- | Same as fromEqn, but has no units.
fromEqn' :: String -> NP -> Sentence -> Symbol -> Expr -> QDefinition
fromEqn' nm desc _ symb eqn = 
  EC (cv (dccWDS nm desc (phrase desc)) symb Rational) eqn ([] :: Attributes)
  --EC (cv (dccWDS nm desc (phrase desc +:+ extra)) symb Rational) eqn

-- | Create a 'QDefinition' with an id, noun phrase (term), symbol,
-- abbreviation, unit, and defining equation.
fromEqn'' :: (Unit u) => String -> NP -> Sentence -> Symbol -> String -> Maybe u -> Expr -> QDefinition
fromEqn'' nm desc _ symb abb Nothing eqn = 
  EC (cv (dccWDS' nm (desc) (phrase desc) abb) symb Rational) eqn ([] :: Attributes)
fromEqn'' nm desc _ symb abb (Just chunk) eqn = 
  EC (ucFromCV (cv (dccWDS' nm (desc) (phrase desc) abb) symb Rational) chunk) eqn
    ([] :: Attributes)

-- | Smart constructor for QDefinitions. Requires a quantity, its defining 
-- equation, and a list of attributes
ec :: Quantity c => c -> Expr -> Attributes -> QDefinition
ec = EC 

-- | Smart constructor for QDefinitions. Requires a quantity and its defining
-- equation. Assumes no attributes.
ec' :: Quantity c => c -> Expr -> QDefinition
ec' e c = ec e c ([] :: Attributes)
  
-- | Returns a 'VarChunk' from a 'QDefinition'.
-- Currently only used in example /Modules/ which are being reworked.
getVC :: QDefinition -> VarChunk
getVC qd = vcSt (qd ^. id) (qd ^. term) (getStagedS qd) (qd ^. typ)

-- | For testing ONLY. Once all the chunks are updated for attributes this
-- should be removed and the other constructors should be updated to include
-- attributes
aqd :: QDefinition -> Attributes -> QDefinition
aqd (EC a b _) d = ec a b d

instance Eq QDefinition where
  a == b = (a ^. id) == (b ^. id)
