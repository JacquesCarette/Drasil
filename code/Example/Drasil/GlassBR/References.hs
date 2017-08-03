module Drasil.GlassBR.References where

import Language.Drasil
import Data.Drasil.Concepts.Documentation (document, specification)
import Data.Drasil.Software.Products (sciCompS)
import Data.Drasil.Concepts.Thermodynamics (heat)
import Data.Drasil.Utils (mkRefsList)
import Data.Drasil.SentenceStructures (foldlsC, sAnd)

s11_list :: Contents
s11_list = mkRefsList 1 (map (foldlsC) references)

s11_ref2, s11_ref5 :: [Sentence]

references :: [[Sentence]]
references = [s11_ref2, s11_ref5]

koothoor2013, smithLai2005, rbrtsn2012, astm_LR2009, {-astm_C1036,-} astm_C1048, glThick1998 :: Citation

gbCitations :: BibRef
gbCitations = [koothoor2013, smithLai2005, rbrtsn2012, astm_LR2009, {-astm_C1036,-} astm_C1048, glThick1998]

--FIXME: check for references made by GlassBR

---

--ref1
koothoor2013 = MThesis [Author [personWM "Nirmitha" [] "Koothoor"],
  Title (S "A" +:+ phrase document +:+ S "drive approach to certifying" +:+ phrase sciCompS),
  School (S "McMaster University"),
  Place (S "Hamilton", S "Canada"),
  Year (2013)
  ]

---

s11_ref2 = [S "W. S. Smith and L. Lai", 
  Quote (S "A new requirements template for scientific computing,")
  +:+ S "in Proceedings of the First International Workshop on Situational Requirements" +:+ 
  S "Engineering Processes - Methods, Techniques and Tools to Support Situation-Specific Requirements" +:+
  S "Engineering Processes, SREP'05"
  +:+ sParen (S "J.Ralyt" :+: (F Acute 'e') `sC` S "P.Agerfalk, and N.Kraiem, eds."),
  sParen (S "Paris, France"), S "pp. 107-121", 
  S "In conjunction with 13th IEEE International Requirements Engineering Conference",
  S "2005."]

--ref2
smithLai2005 = Article [Author [personWM "W." ["S."] "Smith",
                                personWM "L." [] "Lai"],
                        Title (S "A new requirements template for scientific computing"),
                        Place (S "Paris", S "France"),
                        Pages (107, 121),
                        Year (2005)
                        ]

---

--ref3
rbrtsn2012 = Article [Author [personWM "J." [] "Robertson",
                             personWM "S." [] "Robertson"],
                    Title (S "Volere requirements specification template edition 16"),
                    --URL (S "www.cs.uic.edu/ i442/VolereMaterials/templateArchive16/c Volere template16.pdf"), BROKEN LINK
                    URL (S "https://pdfs.semanticscholar.org/cf57/27a59801086cbd3d14e587e09880561dbe22.pdf"),
                    Year (2012)
                    ]

---

--ref4
astm_LR2009 = Article [Author [mononym "ASTM Standards Committee"],
                    Title (S "Standard Practice for Determining Load Resistance of Glass in Buildings"),
                    Year (2009)]
--FIXME: Categorize --> S "Standard E1300-09a"
--FIXME: Categorize --> S "American Society for Testing and Material (ASTM)"

---

--ref5
s11_ref5 = [S "ASTM", S "developed by subcommittee C1408", S "Book of standards 15.02",
  Quote (S "Standard" +:+ phrase specification +:+. S "for flat glass, C1036")]

--astm_C1036 = Article [Author [mononym "ASTM developed by subcommittee C1408"],
--                    Title (S "Standard" +:+ phrase specification +:+. S "for flat glass, C1036")]
--FIXME: Categorize --> S "Book of standards 15.02"
--FIXME: Year? Find source...

---

--ref6
astm_C1048 = Article [Author [mononym "ASTM developed by subcommittee C14.08"],
                    Title (at_start specification +:+ S "for" +:+ plural heat +:+.
                           S "treated flat glass-Kind HS, kind FT coated" `sAnd`
                           S "uncoated glass, C1048"),
                    Year (2009)]
--FIXME: Categorize --> S "Book of standards 15.02"

---

--ref7
glThick1998 = Article [Author [personWM "L" [] "Beason",
                             personWM "T" ["L"] "Kohutek",
                             personWM "J" ["M"] "Bracci"],
                    Title (S "Basis for ASTME E 1300 Annealed Glass Thickness Selection Charts"),
                    Year (1998)]
--FIXME: check whether citation format is correct