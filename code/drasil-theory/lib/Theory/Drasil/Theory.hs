{-# Language TemplateHaskell, RankNTypes #-}
-- | Defines types and functions for Theoretical Models.
module Theory.Drasil.Theory (
  -- * Types
  TheoryModel,
  -- * Constructors
  tm, tmNoRefs) where

import Control.Lens (view, makeLenses, (^.))

import Drasil.Database (HasUID(..), showUID, HasChunkRefs(..))
import Language.Drasil
import Drasil.Metadata.TheoryConcepts (thModel)

import Theory.Drasil.ModelKinds

-- | A TheoryModel is a collection of:
--
--      * tUid - a UID,
--      * con - a ConceptChunk,
--      * vctx - definition context ('TheoryModel's),
--      * spc - type definitions ('SpaceDefn's),
--      * quan - quantities ('DefinedQuantityDict's),
--      * ops - operations ('ConceptChunk's),
--      * defq - definitions ('QDefinition's),
--      * invs - invariants ('ModelExpr's),
--      * dfun - defined functions ('QDefinition's),
--      * ref - accompanying references ('DecRef's),
--      * lb - a label ('SpaceDefn'),
--      * ra - reference address ('SpaceDefn'),
--      * notes - additional notes ('Sentence's).
--
-- Right now, neither the definition context (vctx) nor the
-- spaces (spc) are ever defined.
data TheoryModel = TM
  { _mk    :: ModelKind ModelExpr
  , _rf    :: [DecRef]
  ,  lb    :: ShortName
  ,  ra    :: String
  , _notes :: [Sentence]
  }
makeLenses ''TheoryModel

instance HasChunkRefs TheoryModel where
  chunkRefs tm' = mconcat
    [ chunkRefs (tm' ^. mk)
    , chunkRefs (lb tm')
    , chunkRefs (tm' ^. notes)
    ]
  {-# INLINABLE chunkRefs #-}

-- | Finds the 'UID' of a 'TheoryModel'.
instance HasUID             TheoryModel where uid = mk . uid
-- | Finds the term ('NP') of the 'TheoryModel'.
instance NamedIdea          TheoryModel where term = mk . term
-- | Finds the idea of the 'ConceptChunk' contained in the 'TheoryModel'.
instance Idea               TheoryModel where getA = getA . view mk
-- | Finds the definition of the 'ConceptChunk' contained in a 'TheoryModel'.
instance Definition         TheoryModel where defn = mk . defn
{-- | Finds 'Reference's contained in the 'TheoryModel'.
instance HasReference       TheoryModel where getReferences l = map ref $ rf l-}
-- | Finds 'DecRef's contained in the 'TheoryModel'.
instance HasDecRef          TheoryModel where getDecRefs = rf
-- | Finds the domain of the 'ConceptChunk' contained in a 'TheoryModel'.
instance ConceptDomain      TheoryModel where cdom = cdom . view mk
-- | Finds any additional notes for the 'TheoryModel'.
instance HasAdditionalNotes TheoryModel where getNotes = notes

-- TODO: I think we should be gathering these from the ModelKinds of the TheoryModel.
--       If we need "more than 1 ModelKind" in the TheoryModel, we may need to create
--       a "stacked model" that allows for composing them.

-- | Finds the 'ShortName' of the 'TheoryModel'.
instance HasShortName       TheoryModel where shortname = lb
-- | Finds the reference address of the 'TheoryModel'.
instance HasRefAddress      TheoryModel where getRefAdd l = RP (prepend $ abrv l) (ra l)
-- | Finds the idea of a 'TheoryModel' (abbreviation).
instance CommonIdea         TheoryModel where abrv _ = abrv thModel
instance Express            TheoryModel where mexpress = mexpress . (^. mk)
-- | Finds the reference address of a 'TheoryModel'.
instance Referable TheoryModel where
  refAdd      = ra
  renderRef l = RP (prepend $ abrv l) (refAdd l)

-- TODO: Theory Models should generally be using their own UID, instead of
--       having their UIDs derived by the model kind.

-- This "smart" constructor is really quite awful, it takes way too many arguments.
-- This should likely be re-arranged somehow. Especially since since of the arguments
-- have the same type!
-- | Constructor for theory models. Must have a source. Uses the shortname of the reference address.
tm :: ModelKind ModelExpr -> [DecRef] -> String -> [Sentence] -> TheoryModel
tm mkind [] _   = error $ "Source field of " ++ showUID mkind ++ " is empty"
tm mkind r  lbe = TM mkind r (shortname' $ S lbe) (prependAbrv thModel lbe)

-- | Constructor for theory models. Uses the shortname of the reference address.
tmNoRefs :: ModelKind ModelExpr -> String -> [Sentence] -> TheoryModel
tmNoRefs mkind lbe = TM mkind [] (shortname' $ S lbe) (prependAbrv thModel lbe)
