module Drasil.SRS (doc, doc', intro) where
--Temporary file for keeping the "srs" document constructor until I figure out
-- a better place for it. Maybe Data.Drasil or Language.Drasil.Template?

--May want to combine SRS-specific functions into this file as well (ie. OrganizationOfSRS) to make it more Recipe-like.

import Language.Drasil

import Data.Drasil.Concepts.Documentation

-- Local function to keep things looking clean, not exported.
forTT' :: (NamedIdea c, NamedIdea d) => c -> d -> Sentence
forTT' = for'' titleize titleize'

doc, doc' :: NamedIdea c => c -> Sentence -> [Section] -> Document
doc sys authors secs = Document (srs `for` sys) authors secs


doc' sys authors secs = Document (srs `forTT'` sys) authors secs

intro :: [Contents] -> [Section] -> Section
intro conts sects = section (titleize introduction) conts sects
