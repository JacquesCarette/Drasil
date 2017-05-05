module Drasil.SRS (doc, doc', intro) where
--Temporary file for keeping the "srs" document constructor until I figure out
-- a better place for it. Maybe Data.Drasil or Language.Drasil.Template?

--May want to combine SRS-specific functions into this file as well (ie. OrganizationOfSRS) to make it more Recipe-like.

import Language.Drasil

import Data.Drasil.Concepts.Documentation

doc, doc' :: NamedIdea c => c -> Sentence -> [Section] -> Document
doc sys authors secs = Document (srs `for` sys) authors secs

doc' sys authors secs = Document (srs `for'''` sys) authors secs

intro :: [SecCons] -> Section
intro l = Section (titleize introduction) l
