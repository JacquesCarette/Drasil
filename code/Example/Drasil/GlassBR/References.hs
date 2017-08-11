module Drasil.GlassBR.References where

import Language.Drasil

koothoor2013, smithLai2005, rbrtsn2012, astm_LR2009, astm_C1036, astm_C1048, glThick1998 :: Citation

gbCitations :: BibRef
gbCitations = [koothoor2013, smithLai2005, rbrtsn2012, astm_LR2009, astm_C1036, astm_C1048, glThick1998]

--FIXME: check for references made within document

---

--ref1
koothoor2013 = MThesis [Author [personWM "N." [] "Koothoor"],
  Title (S "A document drive approach to certifying scientific computing software"),
  School (S "McMaster University"),
  Place (S "Hamilton", S "Canada"),
  Year (2013)
  ]

--FIXME: Place isn't displayed in TeX?

---

--ref2
smithLai2005 = Article [
  Author [personWM "W" ["Spencer"] "Smith",
          personWM "L" [] "Lai"],
  Title (S "A new requirements template for scientific computing"),
  Journal (S "Proceedings of the First International Workshop on" +:+
  S "Situational Requirements Engineering Processes - Methods," +:+
  S "Techniques and Tools to Support Situation-Specific Requirements" +:+
  S "Engineering Processes, SREP'05"),
  Editor [personWM "J." [] "Ralyte" {-(extrctStrng (S "Ralyt" :+: (F Acute 'e')))-},
          personWM "P." [] "Agerfalk",
          personWM "N." [] "Kraiem"],
  Place (S "Paris", S "France"),
  Pages (107, 121),
  Note (S "In conjunction with 13th IEEE International Requirements" +:+
  S "Engineering Conference,"),
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
astm_LR2009 = Misc [Author [mononym "ASTM Standards Committee"],
                    Title (S "Standard Practice for Determining Load Resistance of Glass in Buildings"),
                    Collection (S "Standard E1300-09a"),
                    Publisher (S "American Society for Testing and Material (ASTM)"),
                    Year (2009)]

--FIXME: Proper category for "Standard E1300-09a"?

---

--ref5
astm_C1036 = Misc [Author [mononym "ASTM developed by subcommittee C14.08"],
                    Title (S "Standard specification for flat glass, C1036"),
                    Collection (S "Book of standards 15.02"),
                    Place (S "West Conshohocken", S "PA"),
                    Year (2016)]

--FIXME: Categorize --> S "Book of standards 15.02"

---

--ref6
astm_C1048 = Misc [Author [mononym "ASTM developed by subcommittee C14.08"],
                    Title (S "Specification for  heat treated flat glass-Kind" +:+
                           S "HS, kind FT coated and uncoated glass, C1048"),
                    Collection (S "Standard E1300-09a"),
                    Year (2009)]

--FIXME: Proper category for "Standard E1300-09a"?

---

--ref7
glThick1998 = Article [Author [personWM "L" [] "Beason",
                             personWM "T" ["L"] "Kohutek",
                             personWM "J" ["M"] "Bracci"],
                    Title (S "Basis for ASTME E 1300 Annealed Glass Thickness Selection Charts"),
                    Year (1998)]

--FIXME: check whether citation format is correct
--FIXME: first listed source in TeX (incorrect alphabetized list)
