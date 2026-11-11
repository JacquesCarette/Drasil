{-# Language TemplateHaskell, RankNTypes #-}
-- | Defines types and functions for Theoretical Models.
module Theory.Drasil.Theory (
  -- * Types
  TheoryModel,
  -- * Constructors
  tm, tmNoRefs) where

import Control.Lens (view, makeLenses, (^.))

import Drasil.Database (HasUID(..), showUID, declareHasChunkRefs, Generically(..))
import Language.Drasil
import Language.Drasil.Document
import Drasil.Metadata.TheoryConcepts (thModel)

import Theory.Drasil.ModelKinds

-- | A TheoryModel is a collection of:
--
--      * mk - the model ('ModelKind'), which also provides its 'UID', term
--             and definition,
--      * rf - accompanying references ('DecRef's),
--      * lbl - a label ('String'),
--      * sn - its 'ShortName',
--      * notes - additional notes ('Sentence's).
data TheoryModel = TM
  { _mk    :: ModelKind ModelExpr
  , _rf    :: [DecRef]
  ,  lbl   :: String
  ,  sn    :: ShortName
  , _notes :: [Sentence]
  }
makeLenses ''TheoryModel
declareHasChunkRefs ''TheoryModel

-- | Finds the 'UID' of a 'TheoryModel'.
instance HasUID             TheoryModel where uid = mk . uid
-- | Finds the term ('NP') of the 'TheoryModel'.
instance NamedIdea          TheoryModel where term = mk . term
-- | Finds the idea of the 'ConceptChunk' contained in the 'TheoryModel'.
instance Idea               TheoryModel where getA = getA . view mk
-- | Finds the definition of the 'ConceptChunk' contained in a 'TheoryModel'.
instance Definition         TheoryModel where defn = mk . defn
-- | Finds 'DecRef's contained in the 'TheoryModel'.
instance HasDecRef          TheoryModel where getDecRefs = rf
-- | Finds any additional notes for the 'TheoryModel'.
instance HasAdditionalNotes TheoryModel where getNotes = notes

-- | Finds the 'ShortName' of the 'TheoryModel'.
instance HasShortName       TheoryModel where shortname = sn
-- | Finds the reference address of the 'TheoryModel'.
instance HasRefAddress      TheoryModel where
  getRefAdd l = RP (prepend $ abrv l) (prependAbrv l $ lbl l)
-- | Finds the idea of a 'TheoryModel' (abbreviation).
instance CommonIdea         TheoryModel where abrv _ = abrv thModel
instance Express            TheoryModel where mexpress = mexpress . (^. mk)
-- | Finds the reference address of a 'TheoryModel'.
instance Referable TheoryModel where
  refAdd    = getAdd . getRefAdd
  renderRef = getRefAdd

-- TODO: Theory Models should generally be using their own UID, instead of
--       having their UIDs derived by the model kind.

-- | Constructor for theory models. Must have a source. Uses the shortname of the reference address.
tm :: ModelKind ModelExpr -> [DecRef] -> String -> [Sentence] -> TheoryModel
tm mkind [] _   = error $ "Source field of " <> showUID mkind <> " is empty"
tm mkind r  lbe = TM mkind r lbe (shortname' $ S lbe)

-- | Constructor for theory models. Uses the shortname of the reference address.
tmNoRefs :: ModelKind ModelExpr -> String -> [Sentence] -> TheoryModel
tmNoRefs mkind lbe = TM mkind [] lbe (shortname' $ S lbe)
