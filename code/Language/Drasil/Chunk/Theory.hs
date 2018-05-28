{-# Language TemplateHaskell, TypeFamilies #-}
module Language.Drasil.Chunk.Theory 
  ( tc',
   Theory(..), TheoryChunk, TheoryModel, tm,
  )where

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom,DOM), Concept, HasAttributes(attributes),
  HasShortName(shortname), HasReference(getReferences))

import Language.Drasil.Chunk.Concept
import Language.Drasil.Chunk.Constrained.Core (TheoryConstraint)
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.Attribute.Core (Attributes)
import Language.Drasil.Chunk.Attribute.References (References)
import Language.Drasil.Chunk.Attribute.ShortName

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

data TheoryChunk = TC { _tid :: String
                      , _vctx :: [TheoryChunk]
                      , _spc  :: [SpaceDefn]
                      , _quan :: [QuantityDict]
                      , _ops  :: [ConceptChunk]
                      , _defq :: [QDefinition]
                      , _invs :: [TheoryConstraint]
                      , _dfun :: [QDefinition]
                      , _ref :: References
                      , _attribs :: Attributes
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
instance HasAttributes TheoryChunk where attributes = attribs
instance HasUID        TheoryChunk where uid = tid
instance HasReference  TheoryChunk where getReferences = ref


-- use the id of the TheoryModel as the uid. FIXME ?
data TheoryModel = TM { _con :: ConceptChunk
                      , _thy :: TheoryChunk
                      , _refname :: ShortNm
                      }
makeLenses ''TheoryModel

instance HasUID        TheoryModel where uid = con . uid
instance NamedIdea     TheoryModel where term = con . term
instance Idea          TheoryModel where getA = getA . view con
instance Definition    TheoryModel where defn = con . defn
instance HasAttributes TheoryModel where attributes = thy . attributes
instance HasShortName  TheoryModel where shortname = refname
instance HasReference  TheoryModel where getReferences = thy . getReferences
instance ConceptDomain TheoryModel where
  type DOM TheoryModel = ConceptChunk
  cdom = con . cdom
instance Concept       TheoryModel where
instance Theory        TheoryModel where
  valid_context = thy . valid_context
  spaces        = thy . spaces
  quantities    = thy . quantities
  operations    = thy . operations
  defined_quant = thy . defined_quant
  invariants    = thy . invariants
  defined_fun   = thy . defined_fun

tc :: (DOM c ~ ConceptChunk, Concept c, Quantity q, HasAttributes q) =>
    String -> [TheoryChunk] -> [SpaceDefn] -> [q] -> [c] -> Attributes ->
    [QDefinition] -> [TheoryConstraint] -> [QDefinition] -> TheoryChunk
tc cid t s q c atts = \dq inv dfn -> TC cid t s (map qw q) (map cw c) dq inv dfn [] atts

tc' :: (HasAttributes q, Quantity q, Concept c, DOM c ~ ConceptChunk) =>
    String -> [q] -> [c] -> Attributes -> [QDefinition] -> 
    [TheoryConstraint] -> [QDefinition] -> TheoryChunk
tc' cid q c atts = tc cid ([] :: [TheoryChunk]) [] q c atts

tm :: (Concept c, DOM c ~ ConceptChunk) => c -> TheoryChunk -> TheoryModel
tm c t = TM (cw c) t (shortname' "")