module Data.Drasil.Citations (
  koothoor2013, parnas1986, smith2005,
  journalCGJ
  ) where

import Language.Drasil --(S,(:+:),(+:+),sC,phrase,F,Accent(..),Citation(..),CiteField(..))
import Data.Drasil.People (spencerSmith, dParnas, nKoothoor)
import Data.Drasil.Software.Products (sciCompS)

---------------
-- CITATIONS --
---------------

koothoor2013, parnas1986, smith2005 :: Citation

koothoor2013 = MThesis [
  Author [nKoothoor],
  Title $ S "A document drive approach to certifying"
          +:+ phrase sciCompS,
  School $ S "McMaster University",
  Place (S "Hamilton", S "Canada"),
  Year 2013]
  
parnas1986 = Article [
  Author [dParnas,
          personWM "Paul"  ["C"] "Clements"],
  Title $ S "A rational design process:" +:+
          S "How and why to fake it",
  Journal $ S "IEEE Transactions on Software Engineering",
  Volume 12,
  Issue 2,
  Pages (251,257),
  Year 1986, --February, but day unknown
  Place (S "Washington", S "USA")]

smith2005 = Article [
  Author [spencerSmith,
          person "Lei" "Lai"],
  Title $ S "A new requirements template for" +:+
          S "scientific computing",
  Journal $ S "SREP",
  Pages (107,121),
  Year 2005,
  Place (S "Paris", S "France"),
  Note (S "In J. Ralyt" :+: (F Acute 'e') `sC` S "P. Agerfalk, and N. Kraiem" `sC`
        S "editors, Proceedings of the First International Workshopon" +:+
        S "Situational Requirements Engineering Processes - Methods," +:+
        S "Techniques and Tools to Support Situation-Specific Requirements" +:+
        S "Engineering Processes." +:+
        --FIXME: The above section is part of the citation but not the note
        S "In conjunction with 13th IEEE International" +:+.
        S "Requirements Engineering Conference")]

------------------------
-- COMMON CITE-FIELDS --
------------------------

jnlCGJ :: CiteField
jnlCGJ = Journal $ S $ "Canadian Geotechnical Journal"

journalCGJ :: Integer -> Integer -> [CiteField]
journalCGJ vol issue = [jnlCGJ, Volume vol, Issue issue]