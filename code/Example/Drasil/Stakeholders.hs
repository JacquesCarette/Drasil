module Drasil.Stakeholders 
  ( stakehldrGeneral) where

import Language.Drasil
import qualified Drasil.SRS as SRS
import Data.Drasil.SentenceStructures (foldlSent)
import Data.Drasil.Concepts.Documentation

stakehldrGeneral :: CINP -> Sentence -> Section
stakehldrGeneral kWord clientDetails = (SRS.stakeholder) [stakeholderIntro] subs
  where subs = [(tClientF kWord clientDetails), (tCustomerF kWord)]

-- general stakeholders introduction
stakeholderIntro :: Contents
stakeholderIntro = Paragraph $ foldlSent [S "This", phrase section_,
            S "describes the" +: titleize' stakeholder, S "the people who have an",
            phrase interest, S "in", (phrase $ the product_)]

tClientF :: CINP -> Sentence ->  Section
tClientF kWord details = SRS.theClient [clientIntro kWord details] []

clientIntro :: CINP -> Sentence -> Contents
clientIntro kWord  details = Paragraph $ foldlSent [(at_start $ the client), S "for",
  (short kWord), S "is a", phrase company, S "named" +:+. details,
  (at_start $ the client), S "has the final say on acceptance of the", 
  phrase product_]

tCustomerF :: CINP -> Section
tCustomerF kWord = SRS.theCustomer [customerIntro kWord] []

customerIntro :: CINP -> Contents
customerIntro kWord = Paragraph $ foldlSent [(at_start' $ the customer), 
  S "are the", phrase endUser, S "of", (short kWord)]