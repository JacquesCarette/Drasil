module Drasil.Stakeholders 
  ( stakehldrGeneral) where

import Language.Drasil
import qualified Drasil.SRS as SRS
import Data.Drasil.SentenceStructures (foldlSP)
import Data.Drasil.Concepts.Documentation

stakehldrGeneral :: CI -> Sentence -> Section
stakehldrGeneral kWord clientDetails = (SRS.stakeholder) [stakeholderIntro] subs
  where subs = [(tClientF kWord clientDetails), (tCustomerF kWord)]

-- general stakeholders introduction
stakeholderIntro :: Contents
stakeholderIntro = foldlSP [S "This", (phrase section_),
            S "describes the" +: (titleize' stakeholder), S "the people who have an",
            (phrase interest), S "in", (phrase $ the product_)]

tClientF :: CI -> Sentence ->  Section
tClientF kWord details = SRS.theClient [clientIntro kWord details] []

clientIntro :: CI -> Sentence -> Contents
clientIntro kWord  details = foldlSP [(at_start $ the client),
  S "for", (short kWord), S "is a", (phrase company), S "named" +:+. details,
  (at_start $ the client), S "has the final say on acceptance of the", 
  (phrase product_)]

tCustomerF :: CI -> Section
tCustomerF kWord = SRS.theCustomer [customerIntro kWord] []

customerIntro :: CI -> Contents
customerIntro kWord = foldlSP [(at_start' $ the customer), 
  S "are the", (phrase endUser), S "of", (short kWord)]