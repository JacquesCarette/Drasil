module Drasil.GamePhysics.References (
  citations, uriReferences, chaslesWiki, accelGravitySrc, impulseSrc,
  gamePhysicsOriginal
) where

import Language.Drasil
import Language.Drasil.Document

import Data.Drasil.Citations (parnasClements1986, dampingSource, hibbeler2004)
import Data.Drasil.People (alex, dParnas, luthfi, olu, wikiAuthors)
import qualified Language.Drasil.Sentence.Combinators as S

chaslesWiki, gamePhysicsOriginal, parnas1978 :: Citation

citations :: BibRef
citations = [parnas1978, chaslesWiki, parnasClements1986,
  dampingSource, hibbeler2004, gamePhysicsOriginal]

--FIXME: check for references made within document

-- | The original, manually created version of GamePhysics.
gamePhysicsOriginal = cMisc
  [ author [alex, luthfi, olu]
  , title "Software Requirements Specification for TamiasMini2D"
  , howPublishedU "https://github.com/smiths/caseStudies/blob/master/CaseStudies/gamephys"
  ]
  "gamePhysicsOriginal"

chaslesWiki = cMisc [author [wikiAuthors],
  title "Chasles' theorem (kinematics)",
  howPublishedU "https://en.wikipedia.org/wiki/Chasles'_theorem_(kinematics)",
  month Nov, year 2018] "chaslesWiki"

parnas1978 = cInProceedings [dParnas]
    "Designing Software for Ease of Extension and Contraction"
    "ICSE '78: Proceedings of the 3rd international conference on Software engineering"
    1978 [pages [264..277]] "parnas1978"

uriReferences :: [Reference]
uriReferences = [accelGravitySrc, impulseSrc]

accelGravitySrc :: Reference
accelGravitySrc = makeURI "accelGravitySrc" "https://en.wikipedia.org/wiki/Gravitational_acceleration" $
  shortname' $ S "Definition" `S.of_` S "Gravitational Acceleration"

impulseSrc :: Reference
impulseSrc = makeURI "impulseSrc" "http://www.chrishecker.com/images/e/e7/Gdmphys3.pdf" $
  shortname' $ S "Impulse for Collision Ref"
