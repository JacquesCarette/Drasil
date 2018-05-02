{-# Language TemplateHaskell, TypeFamilies #-}

module Language.Drasil.Chunk.UncertainQuantity 
  ( UncertQ
  , UncertainQuantity(..)
  , UncertainChunk(..)
  , uq, uqNU
  , uqc, uqcNU
  , uqcND
  , uncrtnChunk, uvc
  , uncrtnw
  ) where
  
import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn),ConceptDomain(cdom,DOM),Concept,HasSymbol(symbol),IsUnit)
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.DefinedQuantity (cqs)
import Language.Drasil.Chunk.Constrained
import Language.Drasil.Chunk.Concept
-- import Language.Drasil.Unit
import Language.Drasil.Expr
import Language.Drasil.NounPhrase
import Language.Drasil.Space
import Language.Drasil.Symbol (Symbol)
import Control.Lens ((^.), Lens', makeLenses)

-- | An UncertainQuantity is just a Quantity with some uncertainty associated to it.
-- This uncertainty is represented as a decimal value between 0 and 1 (percentage).
class Quantity c => UncertainQuantity c where
  uncert :: Lens' c (Maybe Double)

{-- | UncertQ is a chunk which is an instance of UncertainQuantity. It takes a 
-- Quantity and a Double (from 0 to 1) which represents a percentage of uncertainty-}
-- UQ takes a constrained chunk, an uncertainty (between 0 and 1), and a typical value

data UncertQ = UQ { _coco :: ConstrConcept, _unc :: Maybe Double }
makeLenses ''UncertQ
  
instance Eq UncertQ where a == b = (a ^. uid) == (b ^. uid)
instance HasUID UncertQ where uid = coco . uid
instance NamedIdea UncertQ where term = coco . term
instance Idea UncertQ where getA (UQ q _) = getA q
instance HasSpace UncertQ where typ = coco . typ
instance HasSymbol UncertQ where symbol c = symbol (c^.coco)
instance Quantity UncertQ where getUnit    (UQ q _) = getUnit q
instance UncertainQuantity UncertQ where uncert = unc
instance Constrained UncertQ where constraints = coco . constraints
instance HasReasVal UncertQ where reasVal = coco . reasVal
instance Definition UncertQ where defn = coco . defn
instance ConceptDomain UncertQ where
  type DOM UncertQ = ConceptChunk
  cdom = coco . cdom
instance Concept UncertQ where

{-- Constructors --}
-- | The UncertainQuantity constructor. Requires a Quantity, a percentage, and a typical value
uq :: (Quantity c, Constrained c, Concept c, HasReasVal c, DOM c ~ ConceptChunk) => 
  c -> Double -> UncertQ
uq q u = UQ (ConstrConcept (cqs q) (q ^. constraints) (q ^. reasVal)) (Just u)

uqNU :: (Quantity c, Constrained c, Concept c, HasReasVal c, DOM c ~ ConceptChunk) => c -> UncertQ
uqNU q = UQ (ConstrConcept (cqs q) (q ^. constraints) (q ^. reasVal)) Nothing

-- this is kind of crazy and probably shouldn't be used!
uqc :: (IsUnit u, DOM u ~ ConceptChunk) => String -> NP -> String -> Symbol -> u -> Space -> [Constraint]
                -> Expr -> Double -> UncertQ
uqc nam trm desc sym un space cs val uncrt = uq
  (cuc' nam trm desc sym un space cs val) uncrt

--uncertainty quanity constraint no uncertainty
uqcNU :: (IsUnit u, DOM u ~ ConceptChunk) => String -> NP -> String -> Symbol -> u 
                  -> Space -> [Constraint]
                  -> Expr -> UncertQ
uqcNU nam trm desc sym un space cs val = uqNU $ cuc' nam trm desc sym un space cs val

--uncertainty quantity constraint no description
uqcND :: (IsUnit u, DOM u ~ ConceptChunk) => String -> NP -> Symbol -> u -> Space -> [Constraint]
                  -> Expr -> Double -> UncertQ
uqcND nam trm sym un space cs val uncrt = uq (cuc' nam trm "" sym un space cs val) uncrt

{--}

data UncertainChunk  = UCh { _conc :: ConstrainedChunk, _unc' :: Maybe Double }
makeLenses ''UncertainChunk

instance HasUID UncertainChunk where uid = conc . uid
instance Eq UncertainChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance NamedIdea UncertainChunk where term = conc . term
instance Idea UncertainChunk where getA (UCh n _) = getA n
instance HasSpace UncertainChunk where typ = conc . typ
instance HasSymbol UncertainChunk where symbol c = symbol (c^.conc)
instance Quantity UncertainChunk where getUnit    (UCh c _) = getUnit c
instance Constrained UncertainChunk where constraints = conc . constraints
instance HasReasVal UncertainChunk where reasVal = conc . reasVal
instance UncertainQuantity UncertainChunk where uncert = unc'

{-- Constructors --}
uncrtnChunk :: (Quantity c, Constrained c, HasReasVal c) => c -> Double -> UncertainChunk
uncrtnChunk q u = UCh (cnstrw q) (Just u)

-- | Creates an uncertain varchunk
uvc :: String -> NP -> Symbol -> Space -> [Constraint] -> Expr -> Double -> UncertainChunk
uvc nam trm sym space cs val uncrt = uncrtnChunk (cvc nam trm sym space cs val) uncrt

uncrtnw :: (UncertainQuantity c, Constrained c, HasReasVal c) => c -> UncertainChunk
uncrtnw c = UCh (cnstrw c) (c ^. uncert)
