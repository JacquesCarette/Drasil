module Drasil.SRS (srsDoc) where
--Temporary file for keeping the "srs" document constructor until I figure out
-- a better place for it. Maybe Data.Drasil or Language.Drasil.Template?

--May want to combine SRS-specific functions into this file as well (ie. OrganizationOfSRS) to make it more Recipe-like.

import Language.Drasil

import Data.Drasil.Concepts.Documentation

srsDoc :: NamedIdea c => c -> Sentence -> [Section] -> Document
srsDoc sys authors secs = Document (srs `for` sys) authors secs
