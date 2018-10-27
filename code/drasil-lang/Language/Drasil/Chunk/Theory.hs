{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.Theory
  ( tc', TheoryModel, tm', Theory(..))where

import Language.Drasil.Chunk.Concept (ConceptChunk, cw)
import Language.Drasil.Chunk.Eq (QDefinition)
import Language.Drasil.Chunk.Quantity (Quantity, QuantityDict, qw)
import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom), Concept, HasReference(getReferences),
  HasAdditionalNotes(getNotes), HasLabel(getLabel), HasShortName(shortname))
import Language.Drasil.Expr (Relation)
import Language.Drasil.Label.Core (Label)
import Language.Drasil.RefTypes (Reference)
import Language.Drasil.Spec (Sentence)

import Control.Lens (Lens', view, makeLenses)

class Theory t where
  valid_context :: Lens' t [TheoryChunk]
  spaces        :: Lens' t [SpaceDefn]
  quantities    :: Lens' t [QuantityDict]
  operations    :: Lens' t [ConceptChunk] -- FIXME: Should not be Concept
  defined_quant :: Lens' t [QDefinition]
  invariants    :: Lens' t [Relation]
  defined_fun   :: Lens' t [QDefinition]

data SpaceDefn -- FIXME: This should be defined.

{-
A Theory is a collection of type definitions (spc),
quantities (quan), operations (ops), definitions (defq),
invariants (invs), defined functions (dfun) and
accompanying reference (ref).
-}
data TheoryChunk = TC
  { _vctx :: [TheoryChunk]
  , _spc  :: [SpaceDefn]
  , _quan :: [QuantityDict]
  , _ops  :: [ConceptChunk]
  , _defq :: [QDefinition]
  , _invs :: [Relation]
  , _dfun :: [QDefinition]
  , _ref  :: [Reference]
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
instance HasReference  TheoryChunk where getReferences = ref

data TheoryModel = TM { _con :: ConceptChunk
                      , _thy :: TheoryChunk
                      , _lb :: Label
                      , _notes :: [Sentence]
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

tc' :: (Quantity q, Concept c) =>
    [q] -> [c] -> [QDefinition] ->
    [Relation] -> [QDefinition] -> [Reference] -> TheoryChunk
tc' q c dq inv dfn r = TC [] [] (map qw q) (map cw c) dq inv dfn r

tm' :: Concept c => c -> TheoryChunk -> Label -> [Sentence] -> TheoryModel
tm' c t lbe nts = TM (cw c) t lbe nts
