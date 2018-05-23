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

class HasShortName c where
  shortname  :: c -> String  -- The reference address (what we're linking to).
                             -- Should be string with no spaces/special chars.
                             -- FIXME: merge this with `shortname` (.Reference.hs)? (#537)

instance HasShortName Goal where
  shortname g = "GS:" ++ g ^. G.refAddr

instance HasShortName PhysSystDesc where
  shortname p = "PS:" ++ p ^. PD.refAddr

instance HasShortName AssumpChunk where
  shortname  x             = "A:" ++ concatMap repUnd (x ^. uid)

instance HasShortName ReqChunk where
  shortname  r@(RC _ rt _ _ _) = show rt ++ ":" ++ concatMap repUnd (r ^. uid)

instance HasShortName Change where
  shortname r@(ChC _ rt _ _ _)    = show rt ++ ":" ++ concatMap repUnd (r ^. uid)

instance HasShortName Section where
  shortname  (Section _ _ r) = "Sec:" ++ r

instance HasShortName Citation where
  shortname c = concatMap repUnd $ citeID c -- citeID should be unique.

-- error used below is on purpose. These refNames should be made explicit as necessary
instance HasShortName TheoryModel where
  shortname  t = "T:" ++ t^.uid

instance HasShortName GenDefn where
  shortname  g = "GD:" ++ g^.uid

instance HasShortName QDefinition where -- FIXME: This could lead to trouble; need
                                     -- to ensure sanity checking when building
                                     -- Refs. Double-check QDef is a DD before allowing
  shortname  d = "DD:" ++ d^.uid

instance HasShortName InstanceModel where
  shortname  i = "IM:" ++ i^.uid

instance HasShortName Contents where
  shortname (Table _ _ _ _ r)      = "Table:" ++ r
  shortname (Figure _ _ _ r)       = "Figure:" ++ r
  shortname (Graph _ _ _ _ r)      = "Figure:" ++ r
  shortname (EqnBlock _ r)         = "Equation:" ++ r
  shortname (Definition d)         = getDefName d
  shortname (Defnt _ _ r)          = r
  shortname (Requirement rc)       = shortname rc
  shortname (Assumption ca)        = shortname ca
  shortname (Change lcc)           = shortname lcc
  shortname (Enumeration _)        = error "Can't reference lists"
  shortname (Paragraph _)          = error "Can't reference paragraphs"
  shortname (Bib _)                = error $
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
