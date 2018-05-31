{-# Language TemplateHaskell, TypeFamilies #-}

module Language.Drasil.Chunk.UncertainQuantity 
  ( UncertQ
  , UncertainQuantity(..)
  , UncertainChunk(..)
  , uq, uqNU
  , uqc
  , uqcNU
  , uqcND
  , uncrtnChunk, uvc
  , uncrtnw
  ) where
  
import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom, DOM), Concept, HasSymbol(symbol),
  IsUnit, Constrained(constraints), HasReasVal(reasVal), HasAttributes(attributes))
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.DefinedQuantity (dqd')
import Language.Drasil.Chunk.Constrained.Core (Constraint)
import Language.Drasil.Chunk.Constrained (ConstrConcept(..), ConstrainedChunk,cuc',cnstrw,
  cvc)
--import Language.Drasil.Chunk.Attribute.Core (Attributes)
import Language.Drasil.Chunk.Concept
import Language.Drasil.Expr
import Language.Drasil.NounPhrase
import Language.Drasil.Space
import Language.Drasil.Symbol (Symbol)
import Control.Lens ((^.), Lens', makeLenses)

-- | An UncertainQuantity is just a Quantity with some uncertainty associated to it.
-- This uncertainty is represented as a decimal value between 0 and 1 (percentage).
class Quantity c => UncertainQuantity c where
  uncert :: Lens' c (Maybe Double)

{- The order of the following two implementations is the same as in Constrained -}

{--}

-- | UncertQ is a chunk which is an instance of UncertainQuantity. It takes a 
-- ConstrainedChunk and a Double (from 0 to 1) which represents a percentage of uncertainty
data UncertainChunk  = UCh { _conc :: ConstrainedChunk
                           , _unc' :: Maybe Double
                           }
makeLenses ''UncertainChunk

instance HasUID            UncertainChunk where uid = conc . uid
instance Eq                UncertainChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
instance NamedIdea         UncertainChunk where term = conc . term
instance Idea              UncertainChunk where getA (UCh n _) = getA n
instance HasSpace          UncertainChunk where typ = conc . typ
instance HasSymbol         UncertainChunk where symbol c = symbol (c^.conc)
instance Quantity          UncertainChunk where getUnit (UCh c _) = getUnit c
instance Constrained       UncertainChunk where constraints = conc . constraints
instance HasReasVal        UncertainChunk where reasVal = conc . reasVal
instance UncertainQuantity UncertainChunk where uncert = unc'
instance HasAttributes     UncertainChunk where attributes = conc . attributes

{-- Constructors --}
uncrtnChunk :: (HasAttributes c, Quantity c, Constrained c, HasReasVal c) => c -> Double -> UncertainChunk
uncrtnChunk q u = UCh (cnstrw q) (Just u)

-- | Creates an uncertain varchunk
uvc :: String -> NP -> Symbol -> Space -> [Constraint] -> Expr -> Double -> {-Attributes ->-} UncertainChunk
uvc nam trm sym space cs val uncrt {-atts-} = uncrtnChunk (cvc nam trm sym space cs val {-atts-}) uncrt

uncrtnw :: (HasAttributes c, UncertainQuantity c, Constrained c, HasReasVal c) => c -> UncertainChunk
uncrtnw c = UCh (cnstrw c) (c ^. uncert)

-- | UncertQ is a chunk which is an instance of UncertainQuantity. It takes a 
-- ConstrConcept and a Double (from 0 to 1) which represents a percentage of uncertainty

data UncertQ = UQ { _coco :: ConstrConcept
                  , _unc :: Maybe Double
                  }
makeLenses ''UncertQ
  
instance Eq                UncertQ where a == b = (a ^. uid) == (b ^. uid)
instance HasUID            UncertQ where uid = coco . uid
instance NamedIdea         UncertQ where term = coco . term
instance Idea              UncertQ where getA (UQ q _) = getA q
instance HasSpace          UncertQ where typ = coco . typ
instance HasSymbol         UncertQ where symbol c = symbol (c^.coco)
instance Quantity          UncertQ where getUnit (UQ q _) = getUnit q
instance UncertainQuantity UncertQ where uncert = unc
instance Constrained       UncertQ where constraints = coco . constraints
instance HasReasVal        UncertQ where reasVal = coco . reasVal
instance Definition        UncertQ where defn = coco . defn
instance ConceptDomain     UncertQ where
  type DOM UncertQ = ConceptChunk
  cdom = coco . cdom
instance Concept           UncertQ where
instance HasAttributes     UncertQ where attributes = coco . attributes

{-- Constructors --}
-- | The UncertainQuantity constructor. Requires a Quantity, a percentage, and a typical value
uq :: (HasAttributes c, Quantity c, Constrained c, Concept c, HasReasVal c, DOM c ~ ConceptChunk) => 
  c -> Double -> UncertQ
uq q u = UQ (ConstrConcept (dqd' (cw q) (symbol q) (q ^. typ) (getUnit q) (q ^. attributes)) (q ^. constraints) (q ^. reasVal)) (Just u) 

uqNU :: (HasAttributes c, Quantity c, Constrained c, Concept c, HasReasVal c, DOM c ~ ConceptChunk) =>
  c -> UncertQ
uqNU q = UQ (ConstrConcept (dqd' (cw q) (symbol q) (q ^. typ) (getUnit q) (q ^. attributes)) (q ^. constraints) (q ^. reasVal)) Nothing 

-- this is kind of crazy and probably shouldn't be used!
uqc :: (IsUnit u, DOM u ~ ConceptChunk) => String -> NP -> String -> Symbol -> u -> Space
                -> [Constraint] -> Expr -> Double -> UncertQ
uqc nam trm desc sym un space cs val uncrt = uq (cuc' nam trm desc sym un space cs [] val) uncrt

--uncertainty quanity constraint no uncertainty
uqcNU :: (IsUnit u, DOM u ~ ConceptChunk) => String -> NP -> String -> Symbol -> u 
                  -> Space -> [Constraint] -> Expr -> UncertQ
uqcNU nam trm desc sym un space cs val = uqNU (cuc' nam trm desc sym un space cs []{-atts-} val)

--uncertainty quantity constraint no description
uqcND :: (IsUnit u, DOM u ~ ConceptChunk) => String -> NP -> Symbol -> u -> Space -> [Constraint]
                  -> Expr -> Double -> UncertQ
uqcND nam trm sym un space cs val uncrt = uq (cuc' nam trm "" sym un space cs [] val) uncrt

