{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.Attribute.ShortName where

import Language.Drasil.Chunk.Change as Ch
import Language.Drasil.Chunk.Citation as Ci
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.GenDefn
import Language.Drasil.Chunk.Goal as G
import Language.Drasil.Chunk.InstanceModel
import Language.Drasil.Chunk.PhysSystDesc as PD
import Language.Drasil.Chunk.ReqChunk as R
import Language.Drasil.Chunk.Theory
import Control.Lens ((^.))

--hack to think of ShortName as a Strings
type ShortName = String

class HasShortName  s where
  shortname :: s -> ShortName -- String; The text to be displayed for the link.
                            -- A short name used for referencing within a document that can 
                            -- include symbols and whatnot if required.
                            -- Visible in the typeset documents (pdf)
                            

instance HasShortName  Goal where
  shortname g = g ^. G.refAddr

instance HasShortName  PhysSystDesc where
  shortname p = p ^. PD.refAddr

instance HasShortName  ReqChunk where
  shortname (RC _ _ _ sn _)   = sn

instance HasShortName  Change where
  shortname (ChC _ _ _ sn _)     = sn

instance HasShortName  Citation where
  shortname c = citeID c

-- error used below is on purpose. These shortnames should be made explicit as necessary
instance HasShortName  TheoryModel where
  shortname _ = error "No explicit name given for theory model -- build a custom Ref"

instance HasShortName  GenDefn where
  shortname _ = error "No explicit name given for general definition -- build a custom Ref"

instance HasShortName  QDefinition where -- FIXME: This could lead to trouble; need
                                     -- to ensure sanity checking when building
                                     -- Refs. Double-check QDef is a DD before allowing
  shortname _ = error "No explicit name given for data definition -- build a custom Ref"

instance HasShortName  InstanceModel where
  shortname _ = error "No explicit name given for instance model -- build a custom Ref"
