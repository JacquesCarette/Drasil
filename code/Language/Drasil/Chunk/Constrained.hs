{-# Language GADTs, Rank2Types #-}

module Language.Drasil.Chunk.Constrained (
    Constrained(..)
  , Constraint(..), ConstraintOrigin(..), isPhys, isSfwr, getConstraint
  , getPhys, getSfwr
  , ConstrainedChunk(..)
  , ConstrConcept(..)
  , physc, sfwrc, constrained, cuc, cvc, constrained', cuc', constrainedNRV'
  , ConstrWrapper(..), cnstrw
  , createCnstrnts
  , Reason(..), TheoryConstraint(..)
  ) where

import Control.Lens (Simple, Lens, (^.), set)
import Language.Drasil.Expr (Expr(..), Relation, Variable, ($=))
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Unitary
import Language.Drasil.Chunk.VarChunk
import Language.Drasil.Chunk.Unital (ucs)
import Language.Drasil.Chunk.Concept
import Language.Drasil.Unit
import Language.Drasil.NounPhrase
import Language.Drasil.Space
import Language.Drasil.Symbol
import Language.Drasil.Chunk

import Prelude hiding (id)

{-
The lists of Constraints below can be implemented in multiple ways.
For ease of writing I think having a list of functions that take
a chunk as an expression would be the easiest to write. 
ie. [:> 7] or [:> 0, :< C someChunk]
Then we would infer the chunk.
This could also be written as [\c -> C c :> 7] which would have a
slightly different type [c -> Relation], but that is less clear.
-}


-- | A Constrained is a 'Quantity' that has value constraints
-- and maybe reasonable value
class Quantity c => Constrained c where
  constraints :: Simple Lens c [Constraint]
  reasVal     :: Simple Lens c (Maybe Expr)

data ConstraintOrigin = Physical | Software

data Constraint =
  Dummy ConstraintOrigin (Expr -> Relation)
  
data Reason = Invariant | AssumedCon
data TheoryConstraint = 
  TCon Reason Relation -- AssumedCon are constraints that come from assumptions
                       -- as opposed to theory invariants.
                       -- This might be an artificial distinction as they may be "the same"

physc :: (Expr -> Relation) -> Constraint
physc = Dummy Physical

sfwrc :: (Expr -> Relation) -> Constraint
sfwrc = Dummy Software
 
isPhys, isSfwr :: Constraint -> Bool
isPhys (Dummy Physical _) = True
isPhys (Dummy _ _)        = False
isSfwr (Dummy Software _) = True
isSfwr (Dummy _ _)        = False

getConstraint :: Constraint -> (Expr -> Relation)
getConstraint (Dummy _ c) = c

getPhys :: [Constraint] -> [(Expr -> Relation)]
getPhys = map getConstraint . filter isPhys 

getSfwr :: [Constraint] -> [(Expr -> Relation)]
getSfwr = map getConstraint . filter isSfwr

-- | ConstrainedChunks are 'Symbolic Quantities' 
-- with 'Constraints' and maybe typical value
data ConstrainedChunk where
  ConstrainedChunk :: (Quantity c) => c 
                        -> [Constraint] -> Maybe Expr -> ConstrainedChunk

instance Chunk ConstrainedChunk where
  id = qslens id
instance NamedIdea ConstrainedChunk where
  term = qslens term
  getA (ConstrainedChunk n _ _) = getA n
instance Quantity ConstrainedChunk where
  typ = qslens typ
  getSymb s (ConstrainedChunk c _ _) = getSymb s c
  getUnit (ConstrainedChunk c _ _) = getUnit c
  getStagedS (ConstrainedChunk c _ _) = getStagedS c
instance Constrained ConstrainedChunk where
  constraints f (ConstrainedChunk a b c) = 
    fmap (\x -> ConstrainedChunk a x c) (f b)
  reasVal f (ConstrainedChunk a b c) = 
    fmap (\x -> ConstrainedChunk a b x) (f c)
instance Eq ConstrainedChunk where
  (ConstrainedChunk c1 _ _) == (ConstrainedChunk c2 _ _) = 
    (c1 ^. id) == (c2 ^. id)

qslens :: (forall c. (Quantity c) => Simple Lens c a) 
           -> Simple Lens ConstrainedChunk a
qslens l f (ConstrainedChunk a b c) = 
  fmap (\x -> ConstrainedChunk (set l x a) b c) (f (a ^. l))
  
  
-- | Creates a constrained chunk from a symbolic quantity
constrained :: (Quantity c) => c 
                -> [Constraint] -> Expr -> ConstrainedChunk
constrained q cs ex = ConstrainedChunk q cs (Just ex)
  
-- | Creates a constrained unitary  
cuc :: (Unit u) => String -> NP -> Symbol -> u 
                -> Space -> [Constraint] -> Expr -> ConstrainedChunk
cuc i t s u space cs rv = 
  ConstrainedChunk (unitary i t s u space) cs (Just rv)

-- | Creates a constrained varchunk
cvc :: String -> NP -> Symbol -> Space 
       -> [Constraint] -> Expr -> ConstrainedChunk
cvc i des sym space cs rv = 
  ConstrainedChunk (vc i des sym space) cs (Just rv)
  
  
  
-- | ConstrConcepts are 'Conceptual Symbolic Quantities' 
-- with 'Constraints' and maybe a reasonable value
data ConstrConcept where
  ConstrConcept :: (Quantity c, Concept c) => c 
                        -> [Constraint] -> Maybe Expr -> ConstrConcept

instance Chunk ConstrConcept where
  id = cqslens id
instance NamedIdea ConstrConcept where
  term = cqslens term
  getA (ConstrConcept n _ _) = getA n
instance Quantity ConstrConcept where
  typ = cqslens typ
  getSymb s (ConstrConcept c _ _) = getSymb s c
  getUnit (ConstrConcept c _ _) = getUnit c
  getStagedS (ConstrConcept c _ _) = getStagedS c
instance Concept ConstrConcept where
  defn = cqslens defn
  cdom = cqslens cdom
instance Constrained ConstrConcept where
  constraints f (ConstrConcept a b c) = 
    fmap (\x -> ConstrConcept a x c) (f b)
  reasVal f (ConstrConcept a b c) = 
    fmap (\x -> ConstrConcept a b x) (f c)
instance Eq ConstrConcept where
  (ConstrConcept c1 _ _) == (ConstrConcept c2 _ _) = 
    (c1 ^. id) == (c2 ^. id)

cqslens :: (forall c. (Quantity c, Concept c) => Simple Lens c a)
           -> Simple Lens ConstrConcept a
cqslens l f (ConstrConcept a b c) = 
  fmap (\x -> ConstrConcept (set l x a) b c) (f (a ^. l))
  
constrained' :: (Quantity c, Concept c) => c 
                 -> [Constraint] -> Expr -> ConstrConcept
constrained' q cs rv = ConstrConcept q cs (Just rv)

constrainedNRV' :: (Quantity c, Concept c) => c 
                 -> [Constraint] -> ConstrConcept
constrainedNRV' q cs = ConstrConcept q cs Nothing
  
cuc' :: (Unit u) => String -> NP -> String -> Symbol -> u 
                  -> Space -> [Constraint] -> Expr -> ConstrConcept
cuc' nam trm desc sym un space cs rv = 
  ConstrConcept (ucs nam trm desc sym un space) cs (Just rv)

-- ConstraintWrapper for wrapping anything that is constrained
data ConstrWrapper where
  CnstrW :: (Constrained c) => c -> ConstrWrapper

instance Chunk ConstrWrapper where
  id = cwlens id
instance Eq ConstrWrapper where
  a == b = (a ^. id) == (b ^. id)
instance Constrained ConstrWrapper where
  constraints = cwlens constraints
  reasVal = cwlens reasVal
instance NamedIdea ConstrWrapper where
  term = cwlens term
  getA (CnstrW a) = getA a
instance Quantity ConstrWrapper where
  getSymb s (CnstrW a) = getSymb s a
  getUnit (CnstrW a) = getUnit a
  typ = cwlens typ
  getStagedS (CnstrW a) = getStagedS a

cnstrw :: (Constrained c) => c -> ConstrWrapper
cnstrw = CnstrW

-- do not export
cwlens :: (forall c. (Constrained c) => 
  Simple Lens c a) -> Simple Lens ConstrWrapper a
cwlens l f (CnstrW a) = fmap (\x -> CnstrW (set l x a)) (f (a ^. l))

--Helper Functions--
--Helper function that takes a chunk and list of possible values to create a list of constraints
createCnstrnts :: Expr -> [Language.Drasil.Expr.Variable] -> Expr
createCnstrnts c [x] = (c $= (V x))
createCnstrnts c [x, y] = (c $= (V x)) :|| (c $= (V y))
createCnstrnts c (x:y:xs) = createCnstrnts c [x, y] :|| (createCnstrnts c (xs))
createCnstrnts _ [] = error "Constraint Error"
--FIXME: in correct file?
