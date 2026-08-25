-- | Defines helper functions for the Stakeholders section.
module Drasil.SRS.Sections.Stakeholders (stakeholderIntro, tClientF, tCustomerF) where

-- Generic Drasil
import Language.Drasil
import Language.Drasil.Document
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S
import qualified Language.Drasil.Development as D
import Drasil.System (ProjectName, projAbrvS)

-- Vocabulary
import Drasil.Metadata.Documentation (client, customer, endUser, interest,
  product_, section_, stakeholder)

-- other docLang
import qualified Drasil.SRS.Concepts as SRS

-- | General stakeholders introduction.
stakeholderIntro :: Contents
stakeholderIntro = foldlSP [S "This", phrase section_,
            S "describes the" +: plural stakeholder, S "the people who have an",
            phrase interest `S.in_` D.toSent (phraseNP (the product_))]

-- | Constructor for making a client. Takes in the system name and details regarding the client for the specific program.
tClientF :: ProjectName -> Sentence ->  Section
tClientF projNm details = SRS.theClient [clientIntro projNm details] []

-- | General clients introduction. Takes in the system name and details regarding the client for the specific program.
clientIntro :: ProjectName -> Sentence -> Contents
clientIntro projNm  details = foldlSP [D.toSent $ atStartNP $ the client,
  S "for", projAbrvS projNm, S "is" +:+. details,
  D.toSent $ atStartNP $ the client, S "has the final say on acceptance of the",
  phrase product_]

-- | Constructor for making a customer. Takes in the system name.
tCustomerF :: ProjectName -> Section
tCustomerF projNm = SRS.theCustomer [customerIntro projNm] []

-- | General customer introduction. Takes in the system name.
customerIntro :: ProjectName -> Contents
customerIntro projNm = foldlSP [D.toSent $ atStartNP' $ the customer,
  S "are the", phrase endUser `S.of_` projAbrvS projNm]
