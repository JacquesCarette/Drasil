{-# Language TemplateHaskell, TypeFamilies #-}
module Language.Drasil.Unit (
    UnitDefn(..)
  , from_udefn, makeDerU, unitCon
  , (^:), (/:), (*:), (*$), (/$), new_unit
  , scale, shift, fshift, fscale
  , derUC, derUC', derUC'', unitWrapper
  , fund, comp_unitdefn
  ) where

import Control.Lens (Simple, Lens', Lens, (^.), makeLenses, view)
import Control.Arrow (second)

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom, DOM), HasUnitSymbol(usymb), IsUnit(udefn),
  UnitEq(uniteq))
import Language.Drasil.Chunk.Concept (ConceptChunk, dcc, cc')
import Language.Drasil.Symbol
import Language.Drasil.UnitLang

import Language.Drasil.NounPhrase (cn,cn',NP)

-- | for defining fundamental units

data UnitDefn = UD { _vc :: ConceptChunk, _u :: USymb, _ud :: Maybe UDefn}
makeLenses ''UnitDefn

instance HasUID        UnitDefn where uid = vc . uid
instance NamedIdea     UnitDefn where term   = vc . term
instance Idea          UnitDefn where getA c = getA (c ^. vc)
instance Definition    UnitDefn where defn = vc . defn
instance Eq            UnitDefn where a == b = (a ^. usymb) == (b ^. usymb)
instance ConceptDomain UnitDefn where
  type DOM UnitDefn = ConceptChunk
  cdom = vc . cdom
instance HasUnitSymbol UnitDefn where usymb f (UD a b c) = fmap (\x -> UD a x c) (f b)
instance IsUnit        UnitDefn where udefn = ud

-- | Create a derived unit chunk from a concept and a unit equation
makeDerU :: ConceptChunk -> UDefn -> UnitDefn
makeDerU concept eqn = UD concept (from_udefn eqn) (Just eqn)

-- | Create a derived unit chunk from an id, term (as 'String'), definition,
-- symbol, and unit equation
derUC, derUC' :: String -> String -> String -> Symbol -> UDefn -> UnitDefn
-- | Uses self-plural term
derUC  a b c s u = UD (dcc a (cn b) c) (US [(s,1)]) (Just u)
-- | Uses term that pluralizes by adding *s* to the end
derUC' a b c s u = UD (dcc a (cn' b) c) (US [(s,1)]) (Just u)

-- | Create a derived unit chunk from an id, term (as noun phrase), definition, 
-- symbol, and unit equation
derUC'' :: String -> NP -> String -> Symbol -> UDefn -> UnitDefn
derUC'' a b c s u = UD (dcc a b c) (US [(s,1)]) (Just u)

--FIXME: Make this use a meaningful identifier.
-- | Helper for fundamental unit concept chunk creation. Uses the same string
-- for the identifier, term, and definition.
unitCon :: String -> ConceptChunk
unitCon s = dcc s (cn' s) s
---------------------------------------------------------


-- | For allowing lists to mix the two, thus forgetting
-- the definition part
unitWrapper :: (IsUnit u) => u -> UnitDefn
unitWrapper u = UD (cc' u (u ^. defn)) (u ^. usymb) (u ^. udefn)

--- These conveniences go here, because we need the class
-- | Combinator for raising a unit to a power
(^:) :: IsUnit u => u -> Integer -> USymb
u ^: i = upow (u ^. usymb)
  where
    upow (US l) = US $ map (second (* i)) l

-- | Combinator for dividing one unit by another
(/:) :: (IsUnit u1, IsUnit u2) => u1 -> u2 -> USymb
u1 /: u2 = let US l1 = u1 ^. usymb
               US l2 = u2 ^. usymb in
  US $ l1 ++ map (second negate) l2

-- | Combinator for multiplying two units together
(*:) :: (IsUnit u1, IsUnit u2) => u1 -> u2 -> USymb
u1 *: u2 = let US l1 = u1 ^. usymb
               US l2 = u2 ^. usymb in
  US $ l1 ++ l2

-- | Combinator for multiplying a unit and a symbol
(*$) :: (IsUnit u1) => u1 -> USymb -> USymb
u1 *$ u2 = let US l1 = u1 ^. usymb
               US l2 = u2 in
  US $ l1 ++ l2

-- | Combinator for dividing a unit and a symbol
(/$) :: (IsUnit u1) => u1 -> USymb -> USymb
u1 /$ u2 = let US l1 = u1 ^. usymb
               US l2 = u2 in
  US $ l1 ++ map (second negate) l2

-- | Combinator for scaling one unit by some number
scale :: IsUnit s => Double -> s -> UDefn
scale a b = UScale a (b ^. usymb)

fscale :: IsUnit s => Double -> s -> UDefn
fscale a b = FUScale a (b ^. usymb)

-- | Combinator for shifting one unit by some number
shift :: IsUnit s => Double -> s -> UDefn
shift a b = UShift a (b ^. usymb)

fshift :: IsUnit s => Double -> s -> UDefn
fshift a b = FUShift a (b ^. usymb)
-- | Smart constructor for new derived units from existing units.
new_unit :: String -> USymb -> UnitDefn
new_unit s u = makeDerU (unitCon s) (USynonym u)

fund :: String -> String -> String -> UnitDefn
fund nam desc sym = UD (dcc nam (cn' nam) desc) (US [(Atomic sym, 1)]) Nothing

comp_unitdefn :: UnitDefn -> UnitDefn -> Ordering
comp_unitdefn a b = comp_usymb (a ^. usymb) (b ^. usymb)
