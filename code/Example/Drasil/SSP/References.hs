module Drasil.SSP.References where

import Language.Drasil
import Drasil.SSP.Defs (ssa, crtSlpSrf)

import Data.Drasil.Concepts.Documentation (analysis)
import Data.Drasil.Software.Products (sciCompS)
import Data.Drasil.Utils (mkRefsList)

sspCitations :: BibRef
sspCitations = [chen2005, parnas1986]

chen2005, parnas1986 :: Citation
--See Language.Drasil.People for all person constructors
chen2005 = Article [
  Author [personWM' "Q." ["H."] "Qian",
          personWM' "D." ["Y."] "Zhu",
          personWM' "C." ["F."] "Lee",
          personWM' "G." ["R."] "Chen"],
  Title (S "A concise algorithm for computing the" +:+
         S "factor of safety using the morgensternprice method"),
  Journal (S "Can. Geotech. J."),
  Issue 42,
  Date 19 Feb 2005,
  Pages (272,278)]
  
parnas1986 = Article [
  Author [personWM "David" ["L."] "Parnas",
          personWM "Paul"  ["C."] "Clements"],
  Title $ S "A rational design process:" +:+
          S "How and why to fake it",
  Journal $ S "IEEE Transactions on Software Engineering",
  Volume 12,
  Issue 2,
  Pages (251,257),
  Year 1986,
  Place (S "Washington", S "USA")
  ]

sspReferences :: Contents
sspReferences = mkRefsList 1 [ --FIXME: names should be in italics
  S "Q.H. Qian D.Y. Zhu, C.F. Lee and G.R. Chen. A concise algorithm for computing" +:+
            S "the factor of safety using the morgensternprice method. Can. Geotech. J.," +:+.
            S "(42):272-278, 19 February 2005",
  S "D.G. Fredlund and J.Krahn. Comparison of slope stability methods of" +:+.
            phrase analysis +:+. S "Can. Geotech. J., (14):429-439, 4 April 1977",
  S "Nirmitha Koothoor. A document drive approach to certifying" +:+.
            phrase sciCompS +:+ S "Master's thesis, McMaster University," +:+.
            S "Hamilton, Ontario, Canada, 2013",
  S "David L. Parnas and P.C. Clements. A rational design process: How" +:+
            S "and why to fake it. IEEE Transactions on Software Engineering," +:+.
            S "12(2):251-257, February 1986",
  S "W. Spencer Smith and Lei Lai. A new requirements template for" +:+
            S "scientific computing. In J. Ralyt" :+: (F Acute 'e') `sC` S "P. Agerfalk, and N. Kraiem" `sC`
            S "editors, Proceedings of the First International Workshopon" +:+
            S "Situational Requirements Engineering Processes - Methods," +:+
            S "Techniques and Tools to Support Situation-Specific Requirements" +:+
            S "Engineering Processes, SREP'05, pages 107-121, Paris, France" `sC`
            S "2005. In conjunction with 13th IEEE International Requirements" +:+.
            S "Engineering Conference",
  S "Dieter Stolle and Peijun Guo. Limit equilibrum" +:+ phrase ssa +:+.
            S "using rigid finite elements. Can. Geotech. J., (45):653-662, 20 May 2008",
  S "Tony L.T Zhan Dao-Sheng Ling Yu-Chao Li, Yun-Min Chen and" +:+
            S "Peter John Cleall. An efficient approach for locating the" +:+
            phrase crtSlpSrf +:+ S "in" +:+ plural ssa +:+ S "using a" +:+
            S "real-coded genetic algorithm. Can. Geotech. J., (47):806-820," +:+.
            S "25 June 2010"]