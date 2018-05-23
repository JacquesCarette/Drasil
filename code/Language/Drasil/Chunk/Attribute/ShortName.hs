{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.Attribute.ShortName where

import Language.Drasil.Classes (HasUID(uid))
import Language.Drasil.Chunk.AssumpChunk as A
import Language.Drasil.Chunk.Change as Ch
import Language.Drasil.Chunk.Citation as Ci
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.GenDefn
import Language.Drasil.Chunk.Goal as G
import Language.Drasil.Chunk.InstanceModel
import Language.Drasil.Chunk.PhysSystDesc as PD
import Language.Drasil.Chunk.ReqChunk as R
import Language.Drasil.Chunk.Theory
import Language.Drasil.Document
import Control.Lens ((^.))
import Language.Drasil.Spec (Sentence(..), RefName)


class HasShortName  s where
  shortname :: s -> RefName -- Sentence; The text to be displayed for the link.

instance HasShortName  Goal where
  shortname g = S $ g ^. G.refAddr

instance HasShortName  PhysSystDesc where
  shortname p = S $ p ^. PD.refAddr

instance HasShortName  AssumpChunk where
  shortname (AC _ _ sn _) = sn

instance HasShortName  ReqChunk where
  shortname (RC _ _ _ sn _)   = sn

instance HasShortName  Change where
  shortname (ChC _ _ _ sn _)     = sn

instance HasShortName  Section where
  shortname (Section t _ _) = t

instance HasShortName  Citation where
  shortname c = S $ citeID c

-- error used below is on purpose. These shortnames should be made explicit as necessary
instance HasShortName  TheoryModel where
  shortname _ = error "No explicit name given for theory model -- build a custom Ref"

instance HasShortName  GenDefn where
  shortname _ = error "No explicit name given for theory model -- build a custom Ref"

instance HasShortName  QDefinition where -- FIXME: This could lead to trouble; need
                                     -- to ensure sanity checking when building
                                     -- Refs. Double-check QDef is a DD before allowing
  shortname _ = error "No explicit name given for theory model -- build a custom Ref"

instance HasShortName  InstanceModel where
  shortname _ = error "No explicit name given for theory model -- build a custom Ref"

instance HasShortName  Contents where
  shortname (Table _ _ _ _ r)     = S "Table:" :+: S r
  shortname (Figure _ _ _ r)      = S "Figure:" :+: S r
  shortname (Graph _ _ _ _ r)     = S "Figure:" :+: S r
  shortname (EqnBlock _ r)        = S "Equation:" :+: S r
  shortname (Definition d)        = S $ getDefName d
  shortname (Defnt _ _ r)         = S r
  shortname (Requirement rc)      = shortname rc
  shortname (Assumption ca)       = shortname ca
  shortname (Change lcc)          = shortname lcc
  shortname (Enumeration _)       = error "Can't reference lists"
  shortname (Paragraph _)         = error "Can't reference paragraphs"
  shortname (Bib _)               = error $
    "Bibliography list of references cannot be referenced. " ++
    "You must reference the Section or an individual citation."

-- | Automatically create the label for a definition
getDefName :: DType -> String
getDefName (Data c)   = "DD:" ++ concatMap repUnd (c ^. uid) -- FIXME: To be removed
getDefName (Theory c) = "T:" ++ concatMap repUnd (c ^. uid) -- FIXME: To be removed
getDefName TM         = "T:"
getDefName DD         = "DD:"
getDefName Instance   = "IM:"
getDefName General    = "GD:"

repUnd :: Char -> String
repUnd '_' = "."
repUnd c = c : []
