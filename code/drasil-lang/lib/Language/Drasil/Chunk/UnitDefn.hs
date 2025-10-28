{-# Language TemplateHaskell #-}
-- | For defining units built from a concept.
module Language.Drasil.Chunk.UnitDefn (
  -- * Classes
  MayHaveUnit(getUnit),
  IsUnit(getUnits),
  TempHasUnit(findUnit),
  -- * Chunk Type
  UnitDefn(..),
  -- * Constructors
  makeDerU, newUnit,
  derUC, derUC', derUC'',
  fund, fund', derCUC, derCUC', derCUC'',
  unitWrapper,
  -- * Unit Combinators ('UnitEquation's)
  (^:), (/:), (*:), (*$), (/$), (^$),
  -- * Unit Relation Functions
  scale, shift,
  -- * Helpers
  fromUDefn, unitCon, getCu, compUnitDefn
  ) where

import Control.Lens ((^.), makeLenses, view)
import Control.Arrow (second)
import qualified Data.Set as S

import Language.Drasil.Chunk.Concept (ConceptChunk, dcc, cc')
import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom), HasUnitSymbol(usymb), IsUnit(udefn, getUnits))
import Language.Drasil.NounPhrase (cn,cn',NP)
import Language.Drasil.Symbol (Symbol(Label))
import Language.Drasil.UnitLang (USymb(US), UDefn(UScale, USynonym, UShift), 
  compUSymb, fromUDefn, getUSymb, getDefn, UnitSymbol(BaseSI, DerivedSI, Defined))
import Drasil.Database.Chunk (HasChunkRefs(..))
import Drasil.Database.UID (UID, HasUID(..), mkUid)

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
  chunkRefs u =
    let conceptRefs      = chunkRefs (u ^. vc)
        contributorRefs  = S.delete (u ^. uid) $ S.fromList (u ^. cu)
    in conceptRefs `S.union` contributorRefs

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
-- | Finds the domain contained in the 'ConceptChunk' used to make the 'UnitDefn'.
instance ConceptDomain UnitDefn where cdom = cdom . view vc
-- | Finds unit symbol of the 'ConceptChunk' used to make the 'UnitDefn'.
instance HasUnitSymbol UnitDefn where usymb = getUSymb . view cas
-- | Gets the UnitDefn and contributing units. 
instance IsUnit        UnitDefn where 
  udefn = getDefn . view cas  -- Finds unit definition of UnitDefn.
  getUnits = view cu  -- Finds list of contributing units through UIDs from a UnitDefn.

-- | Types may contain a unit ('UnitDefn').
class MayHaveUnit u where
   getUnit :: u -> Maybe UnitDefn

-- | Temporary class to make sure chunks have a unit (in order to eventually get rid of 'MayHaveUnit').
class TempHasUnit u where
   findUnit :: u -> UnitDefn

-- | Takes a contributing unit (['UID']) and a symbol ('USymb').
data UnitEquation = UE {_contributingUnit :: [UID]
                       , _us :: USymb}
makeLenses ''UnitEquation
instance HasUnitSymbol UnitEquation where usymb u = u ^. us
-- ^ Finds the unit symbol ('USymb') for a 'UnitEquation'.

-- | Get a list of 'UID' of the units that make up the 'UnitEquation'.
getCu :: UnitEquation -> [UID]
getCu = view contributingUnit

-- | Create a derived unit chunk from a concept and a unit equation.
makeDerU :: ConceptChunk -> UnitEquation -> UnitDefn
makeDerU concept eqn = UD concept (Defined (usymb eqn) (USynonym $ usymb eqn)) (getCu eqn)

-- FIXME: Shouldn't need to use the UID constructor here.
derCUC, derCUC' :: String -> String -> String -> Symbol -> UnitEquation -> UnitDefn
-- | Create a 'SI_Unit' with two 'Symbol' representations. The created 'NP' is self-plural.
derCUC a b c s ue = UD (dcc a (cn b) c) (DerivedSI (US [(s,1)]) (usymb ue) (USynonym $ usymb ue)) [mkUid a]
-- | Similar to 'derCUC', but the created 'NP' has the 'AddS' plural rule.
derCUC' a b c s ue = UD (dcc a (cn' b) c) (DerivedSI (US [(s,1)]) (usymb ue) (USynonym $ usymb ue)) [mkUid a]
 
-- | Create a derived unit chunk from a 'UID', term ('String'), definition,
-- 'Symbol', and unit equation.
derUC, derUC' :: String -> String -> String -> Symbol -> UDefn -> UnitDefn
-- | Uses self-plural term.
derUC  a b c s u = UD (dcc a (cn b) c) (DerivedSI (US [(s,1)]) (fromUDefn u) u) []
-- | Uses term that pluralizes by adding "s" to the end.
derUC' a b c s u = UD (dcc a (cn' b) c) (DerivedSI (US [(s,1)]) (fromUDefn u) u) []

-- | Create a derived unit chunk from a 'UID', term ('NP'), definition, 
-- 'Symbol', and unit equation.
derCUC'' :: String -> NP -> String -> Symbol -> UnitEquation -> UnitDefn
derCUC'' a b c s ue = UD (dcc a b c) (DerivedSI (US [(s,1)]) (usymb ue) (USynonym $ usymb ue)) (getCu ue)
-- | Create a derived unit chunk from a 'UID', term ('NP'), definition, 
-- 'Symbol', and unit equation.
derUC'' :: String -> NP -> String -> Symbol -> UDefn -> UnitDefn
derUC'' a b c s u = UD (dcc a b c) (DerivedSI (US [(s,1)]) (fromUDefn u) u) []

--FIXME: Make this use a meaningful identifier.
-- | Helper for fundamental unit concept chunk creation. Uses the same 'String'
-- for the identifier, term, and definition.
unitCon :: String -> ConceptChunk
unitCon s = dcc s (cn' s) s
---------------------------------------------------------

-- | For allowing lists to mix together chunks that are units by projecting them into a 'UnitDefn'.
-- For now, this only works on 'UnitDefn's. 
unitWrapper :: (IsUnit u)  => u -> UnitDefn
unitWrapper u = UD (cc' u (u ^. defn)) (Defined (usymb u) (USynonym $ usymb u)) (getUnits u)

-- | Helper to get derived units if they exist.
getSecondSymb :: UnitDefn -> Maybe USymb
getSecondSymb c = get_symb2 $ view cas c
  where
    get_symb2 :: UnitSymbol -> Maybe USymb
    get_symb2 (BaseSI _) = Nothing
    get_symb2 (DerivedSI _ v _) = Just v
    get_symb2 (Defined _ _) = Nothing

-- | Helper to break down unit symbols into 'BaseSI' units.
helperUnit :: UnitDefn -> [UID]
helperUnit a = case getSecondSymb a of
  Just _ -> [a ^. uid]
  Nothing -> getUnits a

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
fund nam desc sym = UD (dcc name (cn' nam) desc) (BaseSI $ US [(Label sym, 1)]) [mkUid name]
  where name = "unit:" ++ nam

-- | Variant of the 'fund', useful for degree.
fund' :: String -> String -> Symbol -> UnitDefn
fund' nam desc sym = UD (dcc name (cn' nam) desc) (BaseSI $ US [(sym, 1)]) [mkUid name]
  where name = "unit:" ++ nam

-- | We don't want an Ord on units, but this still allows us to compare them.
compUnitDefn :: UnitDefn -> UnitDefn -> Ordering
compUnitDefn a b = compUSymb (usymb a) (usymb b)
