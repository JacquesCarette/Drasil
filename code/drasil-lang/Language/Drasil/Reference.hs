module Language.Drasil.Reference(makeRef2, makeRef2S, makeCite,
  makeCiteS, Referable(..)
  ) where

import Control.Lens ((^.))

import Language.Drasil.Chunk.Citation (Citation)
import Language.Drasil.Classes.Core (HasUID(uid), HasRefAddress(getRefAdd),
  HasShortName(shortname))
import Language.Drasil.Classes (Referable(refAdd, renderRef))
import Language.Drasil.Document.Core (LabelledContent(..), RawContent(..))
import Language.Drasil.Label.Type (LblType(RP), IRefProg, name, raw, (+::+))
import Language.Drasil.RefProg (Reference(Reference))
import Language.Drasil.Sentence (Sentence(Ref))

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

makeRef2 :: (Referable l, HasShortName l) => l -> Reference
makeRef2 l = Reference (l ^. uid) (renderRef l) (shortname l)

makeRef2S :: (Referable l, HasShortName l) => l -> Sentence
makeRef2S = Ref . makeRef2

-- Here we don't use the Lenses as constraints, we really do want a Citation.
makeCite :: Citation -> Reference
makeCite l = Reference (l ^. uid) (renderRef l) (shortname l)

makeCiteS :: Citation -> Sentence
makeCiteS = Ref . makeCite

