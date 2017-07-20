module Drasil.GlassBR.References where

import Language.Drasil
import Data.Drasil.Concepts.Documentation (document, specification)
import Data.Drasil.Software.Products (sciCompS)
import Data.Drasil.Concepts.Thermodynamics (heat)
import Data.Drasil.Utils (mkRefsList)
import Data.Drasil.SentenceStructures (foldlsC)

s11_list :: Contents
s11_list = mkRefsList 1 (map (foldlsC) references)

s11_ref1, s11_ref2, s11_ref3, s11_ref4, s11_ref5, s11_ref6 :: [Sentence]

references :: [[Sentence]]
references = [s11_ref1, s11_ref2, s11_ref3, s11_ref4, s11_ref5, s11_ref6]

s11_ref1 = [S "N. Koothoor",
  Quote (S "A" +:+ phrase document +:+ S "drive approach to certifying" 
  +:+ phrase sciCompS :+: S ",") +:+ S "Master's thesis", 
  S "McMaster University, Hamilton, Ontario, Canada", S "2013."]

s11_ref2 = [S "W. S. Smith and L. Lai", 
  Quote (S "A new requirements template for scientific computing,")
  +:+ S "in Proceedings of the First International Workshop on Situational Requirements" +:+ 
  S "Engineering Processes - Methods, Techniques and Tools to Support Situation-Specific Requirements" +:+
  S "Engineering Processes, SREP'05"
  +:+ sParen (S "J.Ralyt" :+: (F Acute 'e') `sC` S "P.Agerfalk, and N.Kraiem, eds."),
  sParen (S "Paris, France"), S "pp. 107-121", 
  S "In conjunction with 13th IEEE International Requirements Engineering Conference",
  S "2005."]
  --FIXME:Make a compoundNC "requirement template"?

s11_ref3 = [S "J. Robertson and S. Robertson", 
  Quote (S "Volere requirements specification template edition 16.") +:+
  Quote (S "www.cs.uic.edu/ i442/VolereMaterials/templateArchive16/c Volere template16.pdf"),
  S "2012."]
  --FIXME:Make a compoundNC "requirement specification template"?

s11_ref4 = [S "ASTM Standards Committee",
  Quote (S "Standard practice for determining load resistance of glass in buildings,")
  +:+ S "Standard E1300-09a", S "American Society for Testing and Material (ASTM)",
  S "2009."]

s11_ref5 = [S "ASTM", S "developed by subcommittee C1408", S "Book of standards 15.02",
  Quote (S "Standard" +:+ phrase specification +:+. S "for flat glass, C1036")]

s11_ref6 = [S "ASTM", S "developed by subcommittee C14.08", S "Book of standards 15.02",
  Quote (at_start specification +:+ S "for" +:+ plural heat +:+.
  S "treated flat glass-Kind HS, kind FT coated and uncoated glass, C1048")]