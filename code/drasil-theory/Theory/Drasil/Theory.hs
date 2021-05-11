{-# Language TemplateHaskell #-}
module Theory.Drasil.Theory (Theory(..), TheoryModel, tm, tmNoRefs) where

import Theory.Drasil.ModelKinds (ModelKinds)

import Language.Drasil
import Data.Drasil.TheoryConcepts (thModel)

import Control.Lens (Lens', view, makeLenses, (^.))

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
  , _ref  :: [Reference]
  ,  lb   :: ShortName
  ,  ra   :: String
  , _notes :: [Sentence]
  }
makeLenses ''TheoryModel

instance HasUID             TheoryModel where uid = con . uid
instance NamedIdea          TheoryModel where term = con . term
instance Idea               TheoryModel where getA = getA . view con
instance Definition         TheoryModel where defn = con . defn
instance HasReference       TheoryModel where getReferences = ref
instance ConceptDomain      TheoryModel where cdom = cdom . view con
instance HasAdditionalNotes TheoryModel where getNotes = notes
instance Theory             TheoryModel where
  valid_context = vctx
  spaces        = spc
  quantities    = quan
  operations    = ops
  defined_quant = defq
  invariants    = invs
  defined_fun   = dfun
instance HasShortName       TheoryModel where shortname = lb
instance HasRefAddress      TheoryModel where getRefAdd = ra
instance CommonIdea         TheoryModel where abrv _ = abrv thModel
instance Referable TheoryModel where
  refAdd      = getRefAdd
  renderRef l = RP (prepend $ abrv l) (getRefAdd l)

-- This "smart" constructor is really quite awful, it takes way too many arguments.
-- This should likely be re-arranged somehow. Especially since since of the arguments
-- have the same type!
tm :: (Quantity q, MayHaveUnit q, Concept c) => ModelKinds ->
    [q] -> [c] -> [QDefinition] ->
    [Relation] -> [QDefinition] -> [Reference] ->
    String -> [Sentence] -> TheoryModel
tm mk _ _ _ _ _ [] _         = error $ "Source field of " ++ mk ^. uid ++ " is empty"
tm mk q c dq inv dfn r lbe = 
  TM (cw mk) [] [] (map qw q) (map cw c) dq inv dfn r (shortname' lbe)
      (prependAbrv thModel lbe)

tmNoRefs :: (Quantity q, MayHaveUnit q, Concept c) => ModelKinds ->
    [q] -> [c] -> [QDefinition] -> [Relation] -> [QDefinition] -> 
    String -> [Sentence] -> TheoryModel
tmNoRefs mk q c dq inv dfn lbe = 
  TM (cw mk) [] [] (map qw q) (map cw c) dq inv dfn [] (shortname' lbe)
      (prependAbrv thModel lbe)
