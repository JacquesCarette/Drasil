module Language.Drasil.Reference(makeRef2, makeRef2S, makeCite,
  makeCiteS, Referable(..), sDom
  ) where

import Control.Lens ((^.))

import Language.Drasil.Chunk.AssumpChunk as A (AssumpChunk)
import Language.Drasil.Chunk.Citation as Ci (citeID, Citation)
import Language.Drasil.Chunk.Concept (ConceptInstance)
import Language.Drasil.Chunk.DataDefinition (DataDefinition)
import Language.Drasil.Chunk.GenDefn (GenDefn)
import Language.Drasil.Chunk.InstanceModel (InstanceModel)
import Language.Drasil.Chunk.Theory (TheoryModel)
import Language.Drasil.Classes.Core (HasUID(uid), HasRefAddress(getRefAdd),
  HasShortName(shortname))
import Language.Drasil.Classes (ConceptDomain(cdom), abrv)
import Language.Drasil.Document (Section(Section))
import Language.Drasil.Document.Core (LabelledContent(..), RawContent(..))
import Language.Drasil.Label.Type (LblType(RP,Citation), IRefProg,
  prepend, name, raw, (+::+), defer)
import Language.Drasil.RefProg (Reference(Reference))
import Language.Drasil.Sentence (Sentence(Ref))
import Language.Drasil.UID (UID)

class HasUID s => Referable s where
  refAdd    :: s -> String  -- The referencing address (what we're linking to).
                            -- Only visible in the source (tex/html).
  renderRef :: s -> LblType -- alternate

instance Referable AssumpChunk where
  refAdd    x = getRefAdd x
  renderRef l = RP (prepend $ abrv l) (refAdd l)

instance Referable Section where
  refAdd    (Section _ _ lb ) = getRefAdd lb
  renderRef (Section _ _ lb)  = RP (raw "Section: " +::+ name) (getRefAdd lb)

instance Referable Citation where
  refAdd    c = c ^. citeID -- citeID should be unique.
  renderRef c = Citation $ refAdd c

instance Referable TheoryModel where
  refAdd    t = getRefAdd t
  renderRef l = RP (prepend $ abrv l) (getRefAdd l)

instance Referable GenDefn where
  refAdd    g = getRefAdd g
  renderRef l = RP (prepend $ abrv l) (getRefAdd l)

instance Referable DataDefinition where
  refAdd    d = getRefAdd d
  renderRef l = RP (prepend $ abrv l) (getRefAdd l)

instance Referable InstanceModel where
  refAdd    i = getRefAdd i
  renderRef l = RP (prepend $ abrv l) (getRefAdd l)

instance Referable ConceptInstance where
  refAdd l    = l ^. uid
  renderRef l = RP ((defer $ sDom $ cdom l) +::+ raw ": " +::+ name) (l ^. uid)

instance Referable LabelledContent where
  refAdd     (LblC lb _) = getRefAdd lb
  renderRef  (LblC lb c) = RP (refLabelledCon c) (getRefAdd lb)

refLabelledCon :: RawContent -> IRefProg
refLabelledCon (Table _ _ _ _)       = raw "Table:" +::+ name 
refLabelledCon (Figure _ _ _)        = raw "Fig:" +::+ name
refLabelledCon (Graph _ _ _ _)       = raw "Fig:" +::+ name
refLabelledCon (Defini _ _)          = raw "Def:" +::+ name
refLabelledCon (Assumption _ _)      = raw "Assump:" +::+ name
refLabelledCon (EqnBlock _)          = raw "EqnB:" +::+ name
refLabelledCon (Enumeration _)       = raw "Lst:" +::+ name 
refLabelledCon (Paragraph _)         = error "Shouldn't reference paragraphs"
refLabelledCon (Bib _)               = error $ 
    "Bibliography list of references cannot be referenced. " ++
    "You must reference the Section or an individual citation."

sDom :: [UID] -> UID
sDom [d] = d
sDom d = error $ "Expected ConceptDomain to have a single domain, found " ++
  show (length d) ++ " instead."

makeRef2 :: (Referable l, HasShortName l) => l -> Reference
makeRef2 l = Reference (l ^. uid) (renderRef l) (shortname l)

makeRef2S :: (Referable l, HasShortName l) => l -> Sentence
makeRef2S = Ref . makeRef2

-- Here we don't use the Lenses as constraints, we really do want a Citation.
makeCite :: Citation -> Reference
makeCite l = Reference (l ^. uid) (renderRef l) (shortname l)

makeCiteS :: Citation -> Sentence
makeCiteS = Ref . makeCite

