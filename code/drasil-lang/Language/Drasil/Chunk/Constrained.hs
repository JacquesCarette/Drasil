{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.Constrained (ConstrReasQDef(..), ConstrainedChunk(..), ConstrainedQDef(..), ConstrConcept(..), ReasonableValueQDef(..),
  cnstrw, cnstrw', constrained', constrainedNRV', cuc, cuc', cuc'', cvc) where

import Control.Lens ((^.), makeLenses, view)

import Language.Drasil.Chunk.Concept (cw, dcc)
import Language.Drasil.Chunk.DefinedQuantity (DefinedQuantityDict, dqd, dqd', dqdWr)
import Language.Drasil.Chunk.Quantity (QuantityDict, qw, vc)
import Language.Drasil.Chunk.Unital (ucs)
import Language.Drasil.Chunk.Unitary (unitary)
import Language.Drasil.Classes.Core (HasUID(uid), HasSymbol(symbol))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom), Concept, Quantity, HasSpace(typ),
  IsUnit, Constrained(constraints), MayHaveReasVal(maybeReasVal), HasReasVal(reasVal),)
import Language.Drasil.Constraint (Constraint(..))
import Language.Drasil.Chunk.UnitDefn (unitWrapper, MayHaveUnit(getUnit))
import Language.Drasil.Expr (Expr(..))
import Language.Drasil.NounPhrase (NP)
import Language.Drasil.Space (Space)
import Language.Drasil.Stages (Stage)
import Language.Drasil.Symbol (Symbol)

-- | ConstrainedChunks are symbolic quantities ('QuantityDict')
-- with 'Constraint's and maybe a typical value ('Maybe' 'Expr').
data ConstrainedChunk = ConstrainedChunk { _qd :: QuantityDict
                                         , _constr :: [Constraint]
                                         , _reasV :: Expr
                                         }
makeLenses ''ConstrainedChunk

-- ^ Finds 'UID' of the 'QuantityDict' used to make the 'ConstrainedChunk'.
instance HasUID        ConstrainedChunk where uid = qd . uid
-- ^ Finds term ('NP') of the 'QuantityDict' used to make the 'ConstrainedChunk'.
instance NamedIdea     ConstrainedChunk where term = qd . term
-- ^ Finds the idea contained in the 'QuantityDict' used to make the 'ConstrainedChunk'.
instance Idea          ConstrainedChunk where getA = getA . view qd
-- ^ Finds the 'Space' of the 'QuantityDict' used to make the 'ConstrainedChunk'.
instance HasSpace      ConstrainedChunk where typ = qd . typ
-- ^ Finds the 'Symbol' of the 'QuantityDict' used to make the 'ConstrainedChunk'.
instance HasSymbol     ConstrainedChunk where symbol c = symbol (c^.qd)
-- ^ 'ConstrainedChunk's have a 'Quantity'. 
instance Quantity      ConstrainedChunk where
-- ^ Finds the 'Constraint's of a 'ConstrainedChunk'.
instance Constrained   ConstrainedChunk where constraints = constr
-- ^ Finds a reasonable value for the 'ConstrainedChunk'.
instance HasReasVal    ConstrainedChunk where reasVal     = reasV
-- ^ Equal if 'UID's are equal.
instance Eq            ConstrainedChunk where c1 == c2 = (c1 ^. qd . uid) == (c2 ^. qd . uid)
-- ^ Finds units contained in the 'QuantityDict' used to make the 'ConstrainedChunk'.
instance MayHaveUnit   ConstrainedChunk where getUnit = getUnit . view qd


data ConstrainedQDef = ConstrainedQDef { _qd' :: QuantityDict
                                       , _constr' :: [Constraint] -- should really be NEList
                                       }
makeLenses ''ConstrainedQDef                          

-- ^ Finds 'UID' of the 'QuantityDict' used to make the 'ConstrainedQDef  '.
instance HasUID        ConstrainedQDef   where uid = qd' . uid
-- ^ Finds term ('NP') of the 'QuantityDict' used to make the 'ConstrainedQDef'.
instance NamedIdea     ConstrainedQDef   where term = qd' . term
-- ^ Finds the idea contained in the 'QuantityDict' used to make the 'ConstrainedQDef '.
instance Idea          ConstrainedQDef   where getA = getA . view qd'
-- ^ Finds the 'Space' of the 'QuantityDict' used to make the 'ConstrainedQDef  '.
instance HasSpace      ConstrainedQDef   where typ = qd' . typ
-- ^ Finds the 'Symbol' of the 'QuantityDict' used to make the 'ConstrainedQDef  '.
instance HasSymbol     ConstrainedQDef   where symbol c = symbol (c^.qd')
-- ^ 'ConstrReasQDef  's have a 'Quantity'. 
instance Quantity      ConstrainedQDef   where
-- ^ Finds a reasonable value for the 'ConstrainedQDef '.
instance Constrained   ConstrainedQDef   where constraints = constr'
-- ^ Equal if 'UID's are equal.
instance Eq            ConstrainedQDef   where c1 == c2 = (c1 ^. qd' . uid) == (c2 ^. qd' . uid)
-- ^ Finds units contained in the 'QuantityDict' used to make the 'ConstrainedQDef  '.
instance MayHaveUnit   ConstrainedQDef   where getUnit = getUnit . view qd'

data ReasonableValueQDef = RVQD { _qd'' :: QuantityDict
                                , reasV' :: Expr
                                }
makeLenses ''ReasonableValueQDef

-- ^ Finds 'UID' of the 'QuantityDict' used to make the 'ReasonableValueQDef'.
instance HasUID        ReasonableValueQDef where uid = qd'' . uid
-- ^ Finds term ('NP') of the 'QuantityDict' used to make the 'ReasonableValueQDef'.
instance NamedIdea     ReasonableValueQDef where term = qd'' . term
-- ^ Finds the idea contained in the 'QuantityDict' used to make the 'ReasonableValueQDef'.
instance Idea          ReasonableValueQDef where getA = getA . view qd''
-- ^ Finds the 'Space' of the 'QuantityDict' used to make the 'ReasonableValueQDef'.
instance HasSpace      ReasonableValueQDef where typ = qd'' . typ
-- ^ Finds the 'Symbol' of the 'QuantityDict' used to make the 'ReasonableValueQDef'.
instance HasSymbol     ReasonableValueQDef where symbol c = symbol (c^.qd'')
-- ^ 'ReasonableValueQDef's have a 'Quantity'. 
instance Quantity      ReasonableValueQDef where
-- ^ Finds a reasonable value for the 'ReasonableValueQDef'.
--instance HasReasVal    ReasonableValueQDef where reasVal     = reasV' --commented out till I sort out the Expr / Maybe Expr situation
-- ^ Equal if 'UID's are equal.
instance Eq            ReasonableValueQDef where c1 == c2 = (c1 ^. qd'' . uid) == (c2 ^. qd'' . uid)
-- ^ Finds units contained in the 'QuantityDict' used to make the 'ReasonableValueQDef'.
instance MayHaveUnit   ReasonableValueQDef where getUnit = getUnit . view qd''


data ConstrReasQDef = CRQD { _qd''' :: QuantityDict
                           , _constr'' :: [Constraint]
                           , reasV'' :: Expr
                           }
makeLenses ''ConstrReasQDef

-- ^ Finds 'UID' of the 'QuantityDict' used to make the 'ConstrReasQDef'.
instance HasUID        ConstrReasQDef where uid = qd''' . uid
-- ^ Finds term ('NP') of the 'QuantityDict' used to make the 'ConstrReasQDef'.
instance NamedIdea     ConstrReasQDef where term = qd''' . term
-- ^ Finds the idea contained in the 'QuantityDict' used to make the 'ConstrReasQDef.
instance Idea          ConstrReasQDef where getA = getA . view qd'''
-- ^ Finds the 'Space' of the 'QuantityDict' used to make the 'ConstrReasQDef'.
instance HasSpace      ConstrReasQDef where typ = qd''' . typ
-- ^ Finds the 'Symbol' of the 'QuantityDict' used to make the 'ConstrReasQDef'.
instance HasSymbol     ConstrReasQDef where symbol c = symbol (c^.qd''')
-- ^ 'ConstrainedChunk's have a 'Quantity'. 
instance Quantity      ConstrReasQDef where
-- ^ Finds the 'Constraint's of a 'ConstrainedChunk'.
instance Constrained   ConstrReasQDef where constraints = constr''
-- ^ Finds a reasonable value for the 'ConstrReasQDef'.
instance HasReasVal    ConstrReasQDef where reasVal     = reasV''    --couldn't match type `Expr` with `ConstrReasQDef -> f ConstrReasQDef`
-- ^ Equal if 'UID's are equal.
instance Eq            ConstrReasQDef where c1 == c2 = (c1 ^. qd''' . uid) == (c2 ^. qd''' . uid)
-- ^ Finds units contained in the 'QuantityDict' used to make the 'ConstrReasQDef'.
instance MayHaveUnit   ConstrReasQDef where getUnit = getUnit . view qd'''


-- | Creates a constrained unitary chunk from a 'UID', term ('NP'), 'Symbol', unit, 'Space', 'Constraint's, and an 'Expr'.
cuc :: (IsUnit u) => String -> NP -> Symbol -> u
  -> Space -> [Constraint] -> ConstrainedQDef
cuc i t s u space = ConstrainedQDef (qw (unitary i t s u space))

-- | Creates a constrained unitary chunk from a 'UID', term ('NP'), 'Symbol', 'Space', 'Constraint's, and a 'Maybe' 'Expr' (Similar to 'cuc' but no units).
cvc :: String -> NP -> Symbol -> Space -> Expr -> ReasonableValueQDef
cvc i des sym space = RVQD (qw (vc i des sym space))

-- | Creates a new ConstrReasQDef from either a 'ConstrainedChunk', 'ConstrConcept', 'UncertainChunk', or an 'UncertQ'.
cnstrw :: (Quantity c, Constrained c, HasReasVal c, MayHaveUnit c) => c -> ConstrReasQDef  
cnstrw c = CRQD   (qw c) (c ^. constraints) (c ^. reasVal)




-- | ConstrConcepts are conceptual symbolic quantities ('DefinedQuantityDict')
-- with 'Constraint's and maybe a reasonable value (no units!).
data ConstrConcept = ConstrConcept { _defq :: DefinedQuantityDict
                                   , _constr''' :: [Constraint]
                                   , _reasV''' :: Maybe Expr
                                   }
makeLenses ''ConstrConcept

-- ^ Finds 'UID' of the 'DefinedQuantityDict' used to make the 'ConstrConcept'.
instance HasUID        ConstrConcept where uid = defq . uid
-- ^ Finds term ('NP') of the 'DefinedQuantityDict' used to make the 'ConstrConcept'.
instance NamedIdea     ConstrConcept where term = defq . term
-- ^ Finds the idea contained in the 'DefinedQuantityDict' used to make the 'ConstrConcept'.
instance Idea          ConstrConcept where getA = getA . view defq
-- ^ Finds the 'Space' of the 'DefinedQuantityDict' used to make the 'ConstrConcept'.
instance HasSpace      ConstrConcept where typ = defq . typ
-- ^ Finds the 'Symbol' of the 'DefinedQuantityDict' used to make the 'ConstrConcept'.
instance HasSymbol     ConstrConcept where symbol c = symbol (c^.defq)
-- ^ 'ConstrConcept's have a 'Quantity'. 
instance Quantity      ConstrConcept where
-- ^ Finds definition of the 'DefinedQuantityDict' used to make the 'ConstrConcept'.
instance Definition    ConstrConcept where defn = defq . defn
-- ^ Finds the domain contained in the 'DefinedQuantityDict' used to make the 'ConstrConcept'.
instance ConceptDomain ConstrConcept where cdom = cdom . view defq
-- ^ Finds the 'Constraint's of a 'ConstrConcept'.
instance Constrained   ConstrConcept where constraints  = constr'''
-- ^ Finds a reasonable value for the 'ConstrConcept'.
instance MayHaveReasVal    ConstrConcept where maybeReasVal      = reasV'''
-- ^ Equal if 'UID's are equal.
instance Eq            ConstrConcept where c1 == c2 = (c1 ^.defq.uid) == (c2 ^.defq.uid)
-- ^ Finds the units of the 'DefinedQuantityDict' used to make the 'ConstrConcept'.
instance MayHaveUnit   ConstrConcept where getUnit = getUnit . view defq


-- | Creates a 'ConstrConcept' with a quantitative concept, a list of 'Constraint's and an 'Expr'.
constrained' :: (Concept c, MayHaveUnit c, Quantity c) =>
  c -> [Constraint] -> Expr -> ConstrConcept
constrained' q cs rv = ConstrConcept (dqdWr q) cs (Just rv)

-- | Similar to 'constrained'', but defaults 'Maybe' 'Expr' to 'Nothing'.
constrainedNRV' :: (Concept c, MayHaveUnit c, Quantity c) =>
  c -> [Constraint] -> ConstrConcept
constrainedNRV' q cs = ConstrConcept (dqdWr q) cs Nothing

-- | Creates a constrained unitary chunk from a 'UID', term ('NP'), description ('String'), 'Symbol', unit, 'Space', 'Constraint's, and an 'Expr'.
cuc' :: (IsUnit u) => String -> NP -> String -> Symbol -> u
            -> Space -> [Constraint] -> Expr -> ConstrConcept
cuc' nam trm desc sym un space cs rv =
  ConstrConcept (dqd (cw (ucs nam trm desc sym space un)) sym space uu) cs (Just rv)
  where uu = unitWrapper un

-- | Similar to 'cuc'', but 'Symbol' is dependent on 'Stage'.
cuc'' :: (IsUnit u) => String -> NP -> String -> (Stage -> Symbol) -> u
            -> Space -> [Constraint] -> Expr -> ConstrConcept
cuc'' nam trm desc sym un space cs rv =
  ConstrConcept (dqd' (dcc nam trm desc) sym space (Just uu)) cs (Just rv)
  where uu = unitWrapper un

-- | Similar to 'cnstrw', but types must also have a 'Concept'.
cnstrw' :: (Quantity c, Concept c, Constrained c, MayHaveReasVal c, MayHaveUnit c) => c -> ConstrConcept
cnstrw' c = ConstrConcept (dqdWr c) (c ^. constraints) (c ^. maybeReasVal)
