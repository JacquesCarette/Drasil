{-# Language TemplateHaskell, TypeFamilies #-}
module Language.Drasil.Chunk.Theory 
  ( tc',
   Theory(..), TheoryChunk, TheoryModel, tm, tm'
  )where

import Language.Drasil.UID (UID)
import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom), Concept, HasReference(getReferences),
  HasAdditionalNotes(getNotes), HasLabel(getLabel))
import Language.Drasil.Chunk.Concept (ConceptChunk, cw)
import Language.Drasil.Chunk.Constrained.Core (TheoryConstraint)
import Language.Drasil.Chunk.Eq (QDefinition)
import Language.Drasil.Chunk.Quantity (Quantity, QuantityDict, qw)
import Language.Drasil.Chunk.References (Reference)
import Language.Drasil.ShortName (HasShortName(shortname))
import Language.Drasil.Spec (Sentence)
import Language.Drasil.Label.Core (Label)

import Control.Lens (Lens', view, makeLenses)

class HasUID t => Theory t where
  valid_context :: Lens' t [TheoryChunk]
  spaces        :: Lens' t [SpaceDefn] 
  quantities    :: Lens' t [QuantityDict]
  operations    :: Lens' t [ConceptChunk] -- FIXME: Should not be Concept
  defined_quant :: Lens' t [QDefinition]
  invariants    :: Lens' t [TheoryConstraint]
  defined_fun   :: Lens' t [QDefinition]

data SpaceDefn -- FIXME: This should be defined.

data TheoryChunk = TC { _tid :: UID
                      , _vctx :: [TheoryChunk]
                      , _spc  :: [SpaceDefn]
                      , _quan :: [QuantityDict]
                      , _ops  :: [ConceptChunk]
                      , _defq :: [QDefinition]
                      , _invs :: [TheoryConstraint]
                      , _dfun :: [QDefinition]
                      , _ref :: [Reference]
                      }
makeLenses ''TheoryChunk

instance Theory        TheoryChunk where
  valid_context = vctx
  spaces        = spc
  quantities    = quan
  operations    = ops
  defined_quant = defq
  invariants    = invs
  defined_fun   = dfun
instance HasUID        TheoryChunk where uid = tid
instance HasReference  TheoryChunk where getReferences = ref


-- use the id of the TheoryModel as the uid. FIXME ?
data TheoryModel = TM { _con :: ConceptChunk
                      , _thy :: TheoryChunk
                      , _lb :: Label
                      , _notes :: Maybe [Sentence]
                      }
makeLenses ''TheoryModel

instance HasUID             TheoryModel where uid = con . uid
instance NamedIdea          TheoryModel where term = con . term
instance Idea               TheoryModel where getA = getA . view con
instance Definition         TheoryModel where defn = con . defn
instance HasReference       TheoryModel where getReferences = thy . getReferences
instance ConceptDomain      TheoryModel where cdom = con . cdom
instance HasAdditionalNotes TheoryModel where getNotes = notes
instance Concept            TheoryModel where
instance Theory             TheoryModel where
  valid_context = thy . valid_context
  spaces        = thy . spaces
  quantities    = thy . quantities
  operations    = thy . operations
  defined_quant = thy . defined_quant
  invariants    = thy . invariants
  defined_fun   = thy . defined_fun
instance HasLabel           TheoryModel where getLabel = lb
instance HasShortName       TheoryModel where shortname = lb . shortname

tc :: (Concept c, Quantity q) =>
    String -> [TheoryChunk] -> [SpaceDefn] -> [q] -> [c] -> 
    [QDefinition] -> [TheoryConstraint] -> [QDefinition] -> [Reference] -> TheoryChunk
tc cid t s q c dq inv dfn r = TC cid t s (map qw q) (map cw c) dq inv dfn r

tc' :: (Quantity q, Concept c) =>
    String -> [q] -> [c] -> [QDefinition] -> 
    [TheoryConstraint] -> [QDefinition] -> [Reference] -> TheoryChunk
tc' cid q c r = tc cid ([] :: [TheoryChunk]) [] q c r

tm :: Concept c => c -> TheoryChunk -> Label -> TheoryModel
tm c t lbe = TM (cw c) t lbe Nothing

-- Same as tm, but with the additional notes argument passed in
tm' :: Concept c => c -> TheoryChunk -> Label -> [Sentence] -> TheoryModel
tm' c t lbe nts = TM (cw c) t lbe (Just nts)
