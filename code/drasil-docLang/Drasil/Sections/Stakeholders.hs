module Drasil.Sections.Stakeholders 
  ( stakehldrGeneral
    , tClientF
    , tCustomerF
    , stakeholderIntro
  ) where

import Language.Drasil
import qualified Drasil.DocLang.SRS as SRS
import Data.Drasil.SentenceStructures (foldlSP)
import Data.Drasil.Concepts.Documentation (section_, stakeholder, interest, product_, client,
  customer, endUser)
import Data.Drasil.Phrase (the)

stakehldrGeneral :: (Idea a) => a -> Sentence -> Section
stakehldrGeneral kWord clientDetails = (SRS.stakeholder) [stakeholderIntro] subs
  where subs = [(tClientF kWord clientDetails), (tCustomerF kWord)]

-- general stakeholders introduction
stakeholderIntro :: Contents
stakeholderIntro = foldlSP [S "This", (phrase section_),
            S "describes the" +: (plural stakeholder), S "the people who have an",
            (phrase interest), S "in", (phrase $ the product_)]

tClientF :: (Idea a) => a -> Sentence ->  Section
tClientF kWord details = SRS.theClient [clientIntro kWord details] []

clientIntro :: (Idea a) => a -> Sentence -> Contents
clientIntro kWord  details = foldlSP [(at_start $ the client),
  S "for", (short kWord), S "is" +:+. details,
  (at_start $ the client), S "has the final say on acceptance of the", 
  (phrase product_)]

tCustomerF :: (Idea a) => a -> Section
tCustomerF kWord = SRS.theCustomer [customerIntro kWord] []

customerIntro :: (Idea a) => a -> Contents
customerIntro kWord = foldlSP [(at_start' $ the customer), 
  S "are the", (phrase endUser), S "of", (short kWord)]
