module Drasil.Stakeholders 
  ( stakehldrGeneral) where

import Language.Drasil
import qualified Drasil.SRS as SRS
import Data.Drasil.SentenceStructures (foldlSent)
import Data.Drasil.Concepts.Documentation
import Control.Lens ((^.))

stakehldrGeneral :: CI -> Sentence -> Section
stakehldrGeneral kWord clientDetails = (SRS.stakeholder) [stakeholderIntro] subs
  where subs = [(tClientF kWord clientDetails), (tCustomerF kWord)]

-- general stakeholders introduction
stakeholderIntro :: Contents
stakeholderIntro = Paragraph $ foldlSent [S "This", (phrase $ section_ ^. term),
            S "describes the" +: (titleize' $ stakeholder ^. term), S "the people who have an",
            (phrase $ interest ^. term), S "in the product"{-, (phrase $ the product_)-}]

tClientF :: CI -> Sentence ->  Section
tClientF kWord details = SRS.theClient [clientIntro kWord details] []

clientIntro :: CI -> Sentence -> Contents
clientIntro kWord  details = Paragraph $ foldlSent [--(at_start $ the client),
  S "The client for", (short kWord), S "is a", (phrase $ company ^. term), S "named" +:+. details,
  {-(at_start $ the client),-} S "The client has the final say on acceptance of the", 
  (phrase $ product_ ^. term)]

tCustomerF :: CI -> Section
tCustomerF kWord = SRS.theCustomer [customerIntro kWord] []

customerIntro :: CI -> Contents
customerIntro kWord = Paragraph $ foldlSent [--(at_start' $ the customer), 
  S "The customers are the", (phrase $ endUser ^. term), S "of", (short kWord)]