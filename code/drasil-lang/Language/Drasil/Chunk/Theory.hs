{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.Theory (TheoryModel, tm, Theory(..))where

import Language.Drasil.Chunk.Concept (ConceptChunk, cw)
import Language.Drasil.Chunk.Eq (QDefinition)
import Language.Drasil.Chunk.Quantity (QuantityDict, qw)
import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA), Quantity,
  Definition(defn), ConceptDomain(cdom), Concept, HasReference2(getReferences2),
  HasAdditionalNotes(getNotes), HasLabel(getLabel), HasShortName(shortname), CommonIdea(abrv))
import Language.Drasil.Development.Unit (MayHaveUnit)
import Language.Drasil.Expr (Relation)
import Language.Drasil.Label.Core (Label)
import Language.Drasil.RefProg (Reference2)
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.Chunk.CommonIdea (CI, commonIdeaWithDict)
import Language.Drasil.Chunk.NamedIdea (IdeaDict, mkIdea)
import Language.Drasil.NounPhrase (cn')

import Control.Lens (Lens', view, makeLenses)

class Theory t where
  valid_context :: Lens' t [TheoryModel]
  spaces        :: Lens' t [SpaceDefn]
  quantities    :: Lens' t [QuantityDict]
  operations    :: Lens' t [ConceptChunk] -- FIXME: Should not be Concept
  defined_quant :: Lens' t [QDefinition]
  invariants    :: Lens' t [Relation]
  defined_fun   :: Lens' t [QDefinition]

data SpaceDefn -- FIXME: This should be defined.

{-
A TheoryModel is a collection of type definitions (spc),
quantities (quan), operations (ops), definitions (defq),
invariants (invs), defined functions (dfun),
accompanying reference (ref), label and notes.

Right now, neither the definition context (vctx) nor the
spaces (spc) are ever defined.
-}
data TheoryModel = TM 
  { _con :: ConceptChunk
  , _vctx :: [TheoryModel]
  , _spc  :: [SpaceDefn]
  , _quan :: [QuantityDict]
  , _ops  :: [ConceptChunk]
  , _defq :: [QDefinition]
  , _invs :: [Relation]
  , _dfun :: [QDefinition]
  , _ref  :: [Reference2]
  , _lb :: Label
  , _notes :: [Sentence]
  , _ci :: CI
  }
makeLenses ''TheoryModel

instance HasUID             TheoryModel where uid = con . uid
instance NamedIdea          TheoryModel where term = con . term
instance Idea               TheoryModel where getA = getA . view con
instance Definition         TheoryModel where defn = con . defn
instance HasReference2      TheoryModel where getReferences2 = ref
instance ConceptDomain      TheoryModel where cdom = con . cdom
instance HasAdditionalNotes TheoryModel where getNotes = notes
instance Concept            TheoryModel where
instance Theory             TheoryModel where
  valid_context = vctx
  spaces        = spc
  quantities    = quan
  operations    = ops
  defined_quant = defq
  invariants    = invs
  defined_fun   = dfun
instance HasLabel           TheoryModel where getLabel = lb
instance HasShortName       TheoryModel where shortname = lb . shortname
instance CommonIdea         TheoryModel where abrv = abrv . view ci

softEng :: IdeaDict
softEng      = mkIdea  "softEng"        (cn' "Software Engineering")  (Just "SE")

theoryMod :: CI
theoryMod    = commonIdeaWithDict "theoryMod"    (cn' "Theory Model")                    "TM"        [softEng]

-- This "smart" constructor is really quite awful, it takes way too many arguments.
-- This should likely be re-arranged somehow. Especially since since of the arguments
-- have the same type!
tm :: (Concept c0, Quantity q, MayHaveUnit q, Concept c1) => c0 ->
    [q] -> [c1] -> [QDefinition] ->
    [Relation] -> [QDefinition] -> [Reference2] ->
    Label -> [Sentence] -> TheoryModel
tm c0 q c1 dq inv dfn r lbe nts = TM (cw c0) [] [] (map qw q) (map cw c1) dq inv dfn r lbe nts theoryMod 
