{-# Language TemplateHaskell #-}
-- | For defining units built from a concept.
module Language.Drasil.Chunk.UnitDefn (
  -- * Classes
  MayHaveUnit(getUnit),
  IsUnit(getUnits),
  -- * Chunk Type
  UnitDefn(..),
  -- * Constructors
  makeDerU, newUnit,
  derUC, derUC', derUC'',
  fund, fund', derCUC, derCUC', derCUC'',
  -- * Unit Combinators ('UnitEquation's)
  (^:), (/:), (*:), (*$), (/$), (^$),
  -- * Unit Relation Functions
  scale, shift,
  -- * Helpers
  fromUDefn, unitCon, getCu, compUnitDefn, unitSymbol
) where

import Control.Lens ((^.), makeLenses, view)
import Control.Arrow (second)

import Drasil.Database (HasChunkRefs(..), UID, HasUID(..), mkUid, nsUid)

import Language.Drasil.Chunk.Concept (ConceptChunk, cncpt''')
import Language.Drasil.Sentence (Sentence(..))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
  Definition(defn), HasUnitSymbol(usymb), IsUnit(udefn, getUnits))
import Language.Drasil.NaturalLanguage.English.NounPhrase (cn,cn',NP)
import Language.Drasil.Symbol (Symbol(Label))
import Language.Drasil.UnitLang (USymb(US), UDefn(UScale, USynonym, UShift),
  compUSymb, fromUDefn, getUSymb, getDefn, UnitSymbol(BaseSI, DerivedSI, Defined))

-- | For defining units.
-- It has a 'ConceptChunk' (that defines what kind of unit it is),
-- a unit symbol, maybe another (when it is a synonym),
-- perhaps a definition, and a list of 'UID' of the units that make up
-- the definition.
--
-- Ex. Meter is a unit of length defined by the symbol (m).
data UnitDefn = UD { _vc :: ConceptChunk
                   , _cas :: UnitSymbol
                   , _cu :: [UID] }
makeLenses ''UnitDefn

instance HasChunkRefs UnitDefn where
  chunkRefs ud = chunkRefs (ud ^. vc)
  {-# INLINABLE chunkRefs #-}

-- | Finds 'UID' of the 'ConceptChunk' used to make the 'UnitDefn'.
instance HasUID        UnitDefn where uid = vc . uid
-- | Finds term ('NP') of the 'ConceptChunk' used to make the 'UnitDefn'.
instance NamedIdea     UnitDefn where term   = vc . term
-- | Finds the idea contained in the 'ConceptChunk' used to make the 'UnitDefn'.
instance Idea          UnitDefn where getA c = getA (c ^. vc)
-- | Finds definition of the 'ConceptChunk' used to make the 'UnitDefn'.
instance Definition    UnitDefn where defn = vc . defn
-- | Equal if 'Symbol's are equal.
instance Eq            UnitDefn where a == b = usymb a == usymb b
-- | Finds unit symbol of the 'ConceptChunk' used to make the 'UnitDefn'.
instance HasUnitSymbol UnitDefn where usymb = getUSymb . view cas
-- | Gets the UnitDefn and contributing units.
instance IsUnit        UnitDefn where
  udefn = getDefn . view cas  -- Finds unit definition of UnitDefn.
  getUnits = view cu  -- Finds list of contributing units through UIDs from a UnitDefn.

-- | Types may contain a unit ('UnitDefn').
class MayHaveUnit u where
   getUnit :: u -> Maybe UnitDefn

-- | Takes a contributing unit (['UID']) and a symbol ('USymb').
data UnitEquation = UE {_contributingUnit :: [UID]
                       , _us :: USymb}
makeLenses ''UnitEquation
instance HasUnitSymbol UnitEquation where usymb u = u ^. us
-- ^ Finds the unit symbol ('USymb') for a 'UnitEquation'.

-- | Extract the 'UnitSymbol' of a 'UnitDefn'.
unitSymbol :: UnitDefn -> UnitSymbol
unitSymbol = (^. cas)

-- | Get a list of 'UID' of the units that make up the 'UnitEquation'.
getCu :: UnitEquation -> [UID]
getCu = view contributingUnit

-- | Create a derived unit chunk from a concept and a unit equation.
makeDerU :: ConceptChunk -> UnitEquation -> UnitDefn
makeDerU concept eqn = UD concept (Defined (usymb eqn) (USynonym $ usymb eqn)) (getCu eqn)

-- FIXME: Shouldn't need to use the UID constructor here.
derCUC, derCUC' :: String -> String -> String -> Symbol -> UnitEquation -> UnitDefn
-- | Create a 'SI_Unit' with two 'Symbol' representations. The created 'NP' is self-plural.
derCUC a b c s ue = UD (cncpt''' (mkUid a) (cn b) (S c)) (DerivedSI (US [(s,1)]) (usymb ue) (USynonym $ usymb ue)) (getCu ue)
-- | Similar to 'derCUC', but the created 'NP' has the 'AddS' plural rule.
derCUC' a b c s ue = UD (cncpt''' (mkUid a) (cn' b) (S c)) (DerivedSI (US [(s,1)]) (usymb ue) (USynonym $ usymb ue)) (getCu ue)

-- | Create a unit from a 'UID', term ('String'), definition, 'Symbol', a
-- scale/shift relation ('UDefn'), and the unit that relation is based on.
derUC, derUC' :: String -> String -> String -> Symbol -> UDefn -> UnitDefn -> UnitDefn
-- | Derived unit with its own special symbol (e.g. °C). Uses self-plural term.
derUC  a b c s u from = UD (cncpt''' (mkUid a) (cn b) (S c)) (DerivedSI (US [(s,1)]) (fromUDefn u) u) [from ^. uid]
-- | Compound unit that is a scaled version of another (e.g. mm, kPa). Uses
-- term that pluralizes by adding "s" to the end.
derUC' a b c s u from = UD (cncpt''' (mkUid a) (cn' b) (S c)) (Defined (US [(s,1)]) u) [from ^. uid]

-- | Create a derived unit chunk from a 'UID', term ('NP'), definition,
-- 'Symbol', and unit equation.
derCUC'' :: String -> NP -> String -> Symbol -> UnitEquation -> UnitDefn
derCUC'' a b c s ue = UD (cncpt''' (mkUid a) b (S c)) (DerivedSI (US [(s,1)]) (usymb ue) (USynonym $ usymb ue)) (getCu ue)
-- | Create a derived unit chunk from a 'UID', term ('NP'), definition,
-- 'Symbol', and unit equation.
derUC'' :: String -> NP -> String -> Symbol -> UDefn -> UnitDefn -> UnitDefn
derUC'' a b c s u from = UD (cncpt''' (mkUid a) b (S c)) (DerivedSI (US [(s,1)]) (fromUDefn u) u) [from ^. uid]

--FIXME: Make this use a meaningful identifier.
-- | Helper for fundamental unit concept chunk creation. Uses the same 'String'
-- for the identifier, term, and definition.
unitCon :: String -> ConceptChunk
unitCon s = cncpt''' (mkUid s) (cn' s) (S s)
---------------------------------------------------------

-- | The units a unit contributes when used inside a unit equation. Base and
-- derived units contribute themselves; compound units contribute the units
-- they are made of.
helperUnit :: UnitDefn -> [UID]
helperUnit ud = case ud ^. cas of
  Defined{} -> getUnits ud
  _         -> [ud ^. uid]

--- These conveniences go here, because we need the class
-- | Combinator for raising a unit to a power.
(^:) :: UnitDefn -> Integer -> UnitEquation
u ^: i = UE (helperUnit u) (upow (usymb u))
--u ^: i = UE ((helperUnit u) ^. uid) (upow (u ^. usymb))
  where
    upow (US l) = US $ map (second (* i)) l

-- | Combinator for dividing one unit by another.
(/:) :: UnitDefn -> UnitDefn -> UnitEquation
u1 /: u2 = let US l1 = usymb u1
               US l2 = usymb u2 in
  UE (helperUnit u1 ++ helperUnit u2) (US $ l1 ++ map (second negate) l2)

-- | Combinator for multiplying two units together.
(*:) :: UnitDefn -> UnitDefn -> UnitEquation
u1 *: u2 = let US l1 = usymb u1
               US l2 = usymb u2 in
  UE (helperUnit u1 ++ helperUnit u2) (US $ l1 ++ l2)

-- | Combinator for multiplying a unit and a symbol.
(*$) :: UnitDefn -> UnitEquation -> UnitEquation
u1 *$ u2 = let US l1 = usymb u1
               US l2 = usymb u2 in
  UE (helperUnit u1 ++ getCu u2) (US $ l1 ++ l2)

-- | Combinator for dividing a unit and a symbol.
(/$) :: UnitDefn -> UnitEquation -> UnitEquation
u1 /$ u2 = let US l1 = usymb u1
               US l2 = usymb u2 in
  UE (helperUnit u1 ++ getCu u2) (US $ l1 ++ map (second negate) l2)

-- | Combinator for mulitiplying two unit equations.
(^$) :: UnitEquation -> UnitEquation -> UnitEquation
u1 ^$ u2 = let US l1 = usymb u1
               US l2 = usymb u2 in
  UE (getCu u1 ++ getCu u2) (US $ l1 ++ l2)

-- | Combinator for scaling one unit by some number.
scale :: IsUnit s => Double -> s -> UDefn
scale a b = UScale a (usymb b)

-- | Combinator for shifting one unit by some number.
shift :: IsUnit s => Double -> s -> UDefn
shift a b = UShift a (usymb b)

-- | Smart constructor for new derived units from existing units.
newUnit :: String -> UnitEquation -> UnitDefn
newUnit s = makeDerU (unitCon s)

-- | Smart constructor for a "fundamental" unit.
fund :: String -> String -> String -> UnitDefn
fund nam desc sym = UD (cncpt''' u (cn' nam) (S desc)) (BaseSI $ US [(Label sym, 1)]) []
  where u = nsUid "unit" (mkUid nam)

-- | Variant of the 'fund', useful for degree.
fund' :: String -> String -> Symbol -> UnitDefn
fund' nam desc sym = UD (cncpt''' u (cn' nam) (S desc)) (BaseSI $ US [(sym, 1)]) []
  where u = nsUid "unit" (mkUid nam)

-- | We don't want an Ord on units, but this still allows us to compare them.
compUnitDefn :: UnitDefn -> UnitDefn -> Ordering
compUnitDefn a b = compUSymb (usymb a) (usymb b)
