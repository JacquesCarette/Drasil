-- | Defines helper functions for the Stakeholders section.
module Drasil.Sections.Stakeholders (stakeholderIntro, tClientF, tCustomerF) where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S
import qualified Language.Drasil.Development as D

import qualified Drasil.DocLang.SRS as SRS
import Drasil.Document.Contents (foldlSP)
import Data.Drasil.Concepts.Documentation (client, customer, endUser, interest,
  product_, section_, stakeholder)

-- | General stakeholders introduction.
stakeholderIntro :: Contents
stakeholderIntro = foldlSP [S "This", phrase section_,
            S "describes the" +: plural stakeholder, S "the people who have an",
            phrase interest `S.in_` D.toSent (phraseNP (the product_))]

-- | Constructor for making a client. Takes in the system name and details regarding the client for the specific program.
tClientF :: (Idea a) => a -> Sentence ->  Section
tClientF kWord details = SRS.theClient [clientIntro kWord details] []

-- | General clients introduction. Takes in the system name and details regarding the client for the specific program.
clientIntro :: (Idea a) => a -> Sentence -> Contents
clientIntro kWord  details = foldlSP [D.toSent $ atStartNP $ the client,
  S "for", short kWord, S "is" +:+. details,
  D.toSent $ atStartNP $ the client, S "has the final say on acceptance of the",
  phrase product_]

-- | Constructor for making a customer. Takes in the system name.
tCustomerF :: (Idea a) => a -> Section
tCustomerF kWord = SRS.theCustomer [customerIntro kWord] []

-- | General customer introduction. Takes in the system name.
customerIntro :: (Idea a) => a -> Contents
customerIntro kWord = foldlSP [D.toSent $ atStartNP' $ the customer,
  S "are the", phrase endUser `S.of_` short kWord]
