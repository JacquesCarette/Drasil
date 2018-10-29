{-# Language TemplateHaskell #-}

module Language.Drasil.Chunk.UncertainQuantity 
  ( UncertQ, UncertainChunk(..) , uq, uqc
  , uqcND, uncrtnChunk, uvc, uncrtnw) where
 
import Language.Drasil.Chunk.DefinedQuantity (dqd')
import Language.Drasil.Chunk.Constrained.Core (Constraint)
import Language.Drasil.Chunk.Constrained (ConstrConcept(..), ConstrainedChunk, cuc', cnstrw, cvc)
import Language.Drasil.Chunk.Concept(cw) 
import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom), Concept, HasSymbol(symbol), Quantity, HasSpace(typ),
  IsUnit, Constrained(constraints), HasReasVal(reasVal), UncertainQuantity(uncert))
import Language.Drasil.Development.Unit (MayHaveUnit(getUnit))
import Language.Drasil.Expr (Expr)
import Language.Drasil.NounPhrase(NP)
import Language.Drasil.Space (Space)
import Language.Drasil.Symbol (Symbol)
import Control.Lens ((^.), makeLenses, view)

--make sure that it is between 0 and 1, and throw an error otherwise
bw0And1 :: (Num a, Ord a) => a -> a
bw0And1 u = if (0 < u) && (u < 1) then u
            else error "Uncertainty must be between 0 and 1."

{- The order of the following two implementations is the same as in Constrained -}

-- | UncertQ is a chunk which is an instance of UncertainQuantity. It takes a 
-- ConstrainedChunk and a Double (from 0 to 1) which represents a percentage of uncertainty
data UncertainChunk  = UCh { _conc :: ConstrainedChunk , _unc' :: Maybe Double }
makeLenses ''UncertainChunk

instance HasUID            UncertainChunk where uid = conc . uid
instance Eq                UncertainChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance NamedIdea         UncertainChunk where term = conc . term
instance Idea              UncertainChunk where getA (UCh n _) = getA n
instance HasSpace          UncertainChunk where typ = conc . typ
instance HasSymbol         UncertainChunk where symbol c = symbol (c^.conc)
instance Quantity          UncertainChunk where 
instance Constrained       UncertainChunk where constraints = conc . constraints
instance HasReasVal        UncertainChunk where reasVal = conc . reasVal
instance UncertainQuantity UncertainChunk where uncert = unc'
instance MayHaveUnit       UncertainChunk where getUnit = getUnit . view conc

{-- Constructors --}
uncrtnChunk :: (Quantity c, Constrained c, HasReasVal c, MayHaveUnit c) => 
  c -> Double -> UncertainChunk
uncrtnChunk q u = UCh (cnstrw q) (Just $ bw0And1 u)

-- | Creates an uncertain varchunk
uvc :: String -> NP -> Symbol -> Space -> [Constraint] -> Expr -> Double -> UncertainChunk
uvc nam trm sym space cs val uncrt = uncrtnChunk (cvc nam trm sym space cs (Just val)) uncrt

-- | projection
uncrtnw :: (UncertainQuantity c, Constrained c, HasReasVal c, MayHaveUnit c) => c -> UncertainChunk
uncrtnw c = UCh (cnstrw c) (c ^. uncert)

-- | UncertQ is a chunk which is an instance of UncertainQuantity. It takes a 
-- ConstrConcept and a Double (from 0 to 1) which represents a percentage of uncertainty

data UncertQ = UQ { _coco :: ConstrConcept , _unc :: Maybe Double }
makeLenses ''UncertQ
  
instance Eq                UncertQ where a == b = (a ^. uid) == (b ^. uid)
instance HasUID            UncertQ where uid = coco . uid
instance NamedIdea         UncertQ where term = coco . term
instance Idea              UncertQ where getA (UQ q _) = getA q
instance HasSpace          UncertQ where typ = coco . typ
instance HasSymbol         UncertQ where symbol c = symbol (c^.coco)
instance Quantity          UncertQ where 
instance UncertainQuantity UncertQ where uncert = unc
instance Constrained       UncertQ where constraints = coco . constraints
instance HasReasVal        UncertQ where reasVal = coco . reasVal
instance Definition        UncertQ where defn = coco . defn
instance ConceptDomain     UncertQ where cdom = coco . cdom
instance Concept           UncertQ where
instance MayHaveUnit       UncertQ where getUnit = getUnit . view coco

{-- Constructors --}
-- | The UncertainQuantity constructor. Requires a Quantity, a percentage, and a typical value
uq :: (Quantity c, Constrained c, Concept c, HasReasVal c, MayHaveUnit c) =>
  c -> Double -> UncertQ
uq q u = UQ (ConstrConcept (dqd' (cw q) (symbol q) (q ^. typ) (getUnit q)) (q ^. constraints) (q ^. reasVal)) (Just $ bw0And1 u)

--FIXME: this is kind of crazy and probably shouldn't be used!
uqc :: (IsUnit u, ConceptDomain u) => String -> NP -> String -> Symbol -> u -> Space
                -> [Constraint] -> Expr -> Double -> UncertQ
uqc nam trm desc sym un space cs val uncrt = uq (cuc' nam trm desc sym un space cs val) uncrt

--uncertainty quantity constraint no description
uqcND :: (IsUnit u, ConceptDomain u) => String -> NP -> Symbol -> u -> Space -> [Constraint]
                  -> Expr -> Double -> UncertQ
uqcND nam trm sym un space cs val uncrt = uq (cuc' nam trm "" sym un space cs val) uncrt
