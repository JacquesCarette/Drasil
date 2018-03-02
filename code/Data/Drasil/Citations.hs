module Data.Drasil.Citations (
  koothoor2013, parnas1986, smithLai2005,
  journalCGJ
  ) where

import Language.Drasil --(S,(:+:),(+:+),sC,phrase,F,Accent(..),Citation(..),CiteField(..))
import Data.Drasil.People (spencerSmith, dParnas, nKoothoor, lLai,
  pjAgerfalk, nKraiem, jRalyte, pcClements)
import Data.Drasil.Software.Products (sciCompS)

---------------
-- CITATIONS --
---------------

koothoor2013, parnas1986, smithLai2005 :: Citation

koothoor2013 = Citation MThesis [
  Author [nKoothoor],
  Title $ S "A document drive approach to certifying"
          +:+ phrase sciCompS,
  School $ S "McMaster University",
  Place (S "Hamilton", S "Canada"),
  Year 2013]
--FIXME: Place isn't displayed in TeX?
  
parnas1986 = Citation Article [
  Author [dParnas, pcClements],
  Title $ S "A rational design process: How and why to fake it",
  Journal $ S "IEEE Transactions on Software Engineering",
  Volume 12,
  Issue 2,
  Pages (251,257),
  Year 1986, --February, but day unknown
  Place (S "Washington", S "USA")]

smithLai2005 = Citation Article 
  [
  Author [spencerSmith, lLai],
  Title (S "A new requirements template for scientific computing"),
  Journal (S "Proceedings of the First International Workshop on" +:+
  S "Situational Requirements Engineering Processes - Methods," +:+
  S "Techniques and Tools to Support Situation-Specific Requirements" +:+
  S "Engineering Processes, SREP'05"),
  Editor [pjAgerfalk, nKraiem, jRalyte],
  Place (S "Paris", S "France"),
  Pages (107, 121),
  Note (S "In conjunction with 13th IEEE International Requirements" +:+
  S "Engineering Conference,"),
  Year 2005
  ] 

------------------------
-- COMMON CITE-FIELDS --
------------------------

jnlCGJ :: CiteField
jnlCGJ = Journal $ S "Canadian Geotechnical Journal"

journalCGJ :: Integer -> Integer -> [CiteField]
journalCGJ vol issue = [jnlCGJ, Volume vol, Issue issue]
