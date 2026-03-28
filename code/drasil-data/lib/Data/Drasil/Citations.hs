-- | Defines citations used in Drasil.
module Data.Drasil.Citations (
    module Data.Drasil.Citations
  , module Drasil.Metadata.Citations
  ) where

import Language.Drasil --(S,(:+:),(+:+),sC,phrase,F,Accent(..),Citation(..),CiteField(..))
import Data.Drasil.People (dParnas, pcClements, mCampidelli, dmWiess, rodPierce,
  wikiAuthors, rcHibbeler, sRobertson, jRobertson)
import Drasil.Metadata.Citations

-- * Citations

-- ** Papers

campidelli, parnas1972, parnasClements1984,
  rbrtsn2012, lineSource, pointSource,
  hibbeler2004 :: Citation

campidelli = cBooklet
  "Glass-BR Software for the design and risk assessment of glass facades subjected to blast loading"
  [author [mCampidelli]] "campidelli"

parnas1972 = cArticle [dParnas]
  "On the Criteria To Be Used in Decomposing Systems into Modules"
  "Communications of the ACM" 1972
  [pages [1053..1058]] "dParnas1972"

parnasClements1984 = cInProceedings
  [dParnas, pcClements, dmWiess]
  "The Modular Structure of Complex Systems"
  "ICSE '84: Proceedings of the 7th international conference on Software engineering"
  1984 [pages [408..417]] "parnasClements1984"

rbrtsn2012 = cMisc [author [jRobertson, sRobertson], title
  "Volere requirements specification template edition 16",
  howPublishedU "https://pdfs.semanticscholar.org/cf57/27a59801086cbd3d14e587e09880561dbe22.pdf"
  , year 2012]
  "rbrtsn2012"

lineSource = cMisc
  [author [mononym "The Editors of Encyclopaedia Britannica"], title "Line",
  howPublishedU "https://www.britannica.com/science/line-mathematics",
  month Jun, year 2019]
  "lineSource"

pointSource = cMisc
  [author [rodPierce], title "Point",
  howPublishedU "https://www.mathsisfun.com/geometry/point.html",
  month May, year 2017]
  "pointSource"

hibbeler2004 = cBookA [rcHibbeler]
  "Engineering Mechanics: Dynamics"
  "Pearson Prentice Hall"
  2004 [] "hibbeler2004"

-- ** Wikipedia

dampingSource, accelerationWiki, velocityWiki, cartesianWiki, laplaceWiki, pidWiki :: Citation

dampingSource = cMisc
  [author [wikiAuthors], title "Damping",
  howPublishedU "https://en.wikipedia.org/wiki/Damping_ratio",
  month Jul, year 2019]
  "dampingSource"

accelerationWiki = cMisc [author [wikiAuthors],
  title "Acceleration", howPublishedU "https://en.wikipedia.org/wiki/Acceleration",
  month Jun, year 2019]
  "accelerationWiki"

velocityWiki = cMisc [author [wikiAuthors],
  title "Velocity", howPublishedU "https://en.wikipedia.org/wiki/Velocity",
  month Jun, year 2019]
  "velocityWiki"

cartesianWiki = cMisc
  [author [wikiAuthors], title "Cartesian coordinate system",
  howPublishedU "https://en.wikipedia.org/wiki/Cartesian_coordinate_system",
  month Jun, year 2019]
  "cartesianWiki"

laplaceWiki
  = cMisc
      [author [wikiAuthors], title "Laplace transform",
       howPublishedU "https://en.wikipedia.org/wiki/Laplace_transform",
       month Nov, year 2020]
      "laplaceWiki"

pidWiki
  = cMisc
      [author [wikiAuthors], title "PID controller",
       howPublishedU "https://en.wikipedia.org/wiki/PID_controller", month Oct,
       year 2020]
      "pidWiki"

-- * Common Cite Fields

jnlCGJ :: String
jnlCGJ = "Canadian Geotechnical Journal"
