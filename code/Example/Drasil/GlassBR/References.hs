module Drasil.GlassBR.References where

import Language.Drasil
import Data.Drasil.Software.Products (sciCompS)

koothoor2013, smithLai2005, rbrtsn2012, astm_LR2009, astm_C1036, astm_C1048, glThick1998 :: Citation

gbCitations :: BibRef
gbCitations = [koothoor2013, smithLai2005, rbrtsn2012, astm_LR2009, astm_C1036, astm_C1048, glThick1998]

--FIXME: check for references made by documents
--FIXME: all references "Print"?

---

--ref1
koothoor2013 = MThesis [Author [personWM "Nirmitha" [] "Koothoor"],
  Title (S "A document drive approach to certifying" +:+ phrase sciCompS),
  School (S "McMaster University"),
  Place (S "Hamilton", S "Canada"),
  Year (2013)
  ]

---

--ref2
smithLai2005 = Article [
  Author [personWM' "W" ["Spencer"] "Smith",
          personWM' "L" [] "Lai"],
  Title (S "A new requirements template for scientific computing"),
  Journal (S "Proceedings of the First International Workshop on" +:+
  S "Situational Requirements Engineering Processes - Methods," +:+
  S "Techniques and Tools to Support Situation-Specific Requirements" +:+
  S "Engineering Processes, SREP'05"),
  Note (S "J. Ralyt" :+: (F Acute 'e') `sC` S "P. Agerfalk" `sC`
  S "and N. Kraiem" `sC` S "editors"), --FIXME: add editors field
  Place (S "Paris", S "France"),
  Pages (107, 121),
  Note (S "In conjunction with 13th IEEE International Requirements" +:+
  S "Engineering Conference"),
  Year 2005]

---

--ref3
rbrtsn2012 = Article [Author [personWM "J" [] "Robertson",
                             personWM "S" [] "Robertson"],
                    Title (S "Volere requirements specification template edition 16"),
                    --URL (S "www.cs.uic.edu/ i442/VolereMaterials/templateArchive16/c Volere template16.pdf"), BROKEN LINK
                    URL (S "https://pdfs.semanticscholar.org/cf57/27a59801086cbd3d14e587e09880561dbe22.pdf"),
                    Year (2012)
                    ]

---

--ref4
astm_LR2009 = Article [Author [mononym "ASTM_Standards_Committee"],
                    Title (S "Standard Practice for Determining Load Resistance of Glass in Buildings"),
                    Year (2009)]
--FIXME: Categorize --> S "Standard E1300-09a"
--FIXME: Categorize --> S "American Society for Testing and Material (ASTM)"

---

--ref5
astm_C1036 = Article [Author [mononym "ASTM_developed_by_subcommittee_C1408"],
                    Note (S "Book of standards 15.02"),
                    Title (S "Standard specification for flat glass, C1036"),
                    Place (S "West Conshohocken", S "PA"),
                    Year (2016)]
--ASTM C1036-16, Standard Specification for Flat Glass, ASTM International, West Conshohocken, PA, 2016, www.astm.org
--FIXME: Correct Source Year? Find source...
--FIXME: Categorize --> S "Book of standards 15.02"

---

--ref6
astm_C1048 = Article [Author [mononym "ASTM_developed_by_subcommittee_C14.08"],
                    Title (S "Specification for  heat treated flat glass-Kind" +:+
                           S "HS, kind FT coated and uncoated glass, C1048"),
                    Year (2009)]
--FIXME: Categorize --> S "Book of standards 15.02"

---

--ref7
--FIXME: check whether citation format is correct
glThick1998 = Article [Author [personWM "L" [] "Beason",
                             personWM "T" ["L"] "Kohutek",
                             personWM "J" ["M"] "Bracci"],
                    Title (S "Basis for ASTME E 1300 Annealed Glass Thickness Selection Charts"),
                    Year (1998)]
