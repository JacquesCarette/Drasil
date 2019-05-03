{-# Language TemplateHaskell #-}

module Language.Drasil.Chunk.UncertainQuantity 
  ( UncertQ, UncertainChunk(..) , uq, uqc
  , uqcND, uncrtnChunk, uvc, uncrtnw) where
 
import Language.Drasil.Chunk.DefinedQuantity (dqd')
import Language.Drasil.Chunk.Constrained (ConstrConcept(..), ConstrainedChunk, cuc', cnstrw, cvc)
import Language.Drasil.Chunk.Concept(cw) 
import Language.Drasil.Classes.Core (HasUID(uid), HasSymbol(symbol))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom), Concept, Quantity, HasSpace(typ),
  IsUnit, Constrained(constraints), HasReasVal(reasVal), UncertainQuantity(uncert))
import Language.Drasil.Constraint (Constraint)
import Language.Drasil.Chunk.UnitDefn (MayHaveUnit(getUnit))
import Language.Drasil.Expr (Expr)
import Language.Drasil.NounPhrase(NP)
import Language.Drasil.Space (Space)
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Uncertainty (Uncertainty, uncty)
import Control.Lens ((^.), makeLenses, view)

{- The order of the following two implementations is the same as in Constrained -}

-- | UncertQ is a chunk which is an instance of UncertainQuantity. It takes a 
-- ConstrainedChunk and a Double (from 0 to 1) which represents a percentage of uncertainty
data UncertainChunk  = UCh { _conc :: ConstrainedChunk , _unc' :: Maybe Uncertainty }
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
  c -> Uncertainty -> UncertainChunk
uncrtnChunk q u = UCh (cnstrw q) (Just u)

-- | Creates an uncertain varchunk
uvc :: String -> NP -> Symbol -> Space -> [Constraint] -> Expr -> Double -> Maybe Integer -> UncertainChunk
uvc nam trm sym space cs val uncrt prec = uncrtnChunk (cvc nam trm sym space cs (Just val)) (uncty uncrt prec)

-- | projection
uncrtnw :: (UncertainQuantity c, Constrained c, HasReasVal c, MayHaveUnit c) => c -> UncertainChunk
uncrtnw c = UCh (cnstrw c) (c ^. uncert)

-- | UncertQ is a chunk which is an instance of UncertainQuantity. It takes a 
-- ConstrConcept and a Double (from 0 to 1) which represents a percentage of uncertainty

data UncertQ = UQ { _coco :: ConstrConcept , _unc :: Maybe Uncertainty }
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
instance ConceptDomain     UncertQ where cdom = cdom . view coco
instance MayHaveUnit       UncertQ where getUnit = getUnit . view coco

{-- Constructors --}
-- | The UncertainQuantity constructor. Requires a Quantity, a percentage, and a typical value
uq :: (Quantity c, Constrained c, Concept c, HasReasVal c, MayHaveUnit c) =>
  c -> Uncertainty -> UncertQ
uq q u = UQ (ConstrConcept (dqd' (cw q) (symbol q) (q ^. typ) (getUnit q)) (q ^. constraints) (q ^. reasVal)) (Just u)

--FIXME: this is kind of crazy and probably shouldn't be used!
uqc :: (IsUnit u) => String -> NP -> String -> Symbol -> u -> Space
                -> [Constraint] -> Expr -> Double -> Maybe Integer -> UncertQ
uqc nam trm desc sym un space cs val uncrt prec = uq (cuc' nam trm desc sym un space cs val) (uncty uncrt prec)

--uncertainty quantity constraint no description
uqcND :: (IsUnit u) => String -> NP -> Symbol -> u -> Space -> [Constraint]
                  -> Expr -> Double -> Maybe Integer -> UncertQ
uqcND nam trm sym un space cs val uncrt prec = uq (cuc' nam trm "" sym un space cs val) (uncty uncrt prec)
