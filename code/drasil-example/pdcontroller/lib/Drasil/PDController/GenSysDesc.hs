module Drasil.PDController.GenSysDesc where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Development as D
import Drasil.Document.Contents (foldlSP, foldlSPCol)
import Drasil.Sentence.Combinators (bulletFlat, bulletNested)

import Data.Drasil.Concepts.Documentation
       (environment, software, softwareSys, sysCont, system, user)

import Drasil.PDController.LabelledContent (gsdSysContextFig)
import Drasil.PDController.Concepts

gsdSysContextP1, gsdSysContextP2 :: Contents
gsdSysContextP1
  = foldlSP
      [refS gsdSysContextFig, S "shows the" +:+. phrase sysCont,
       S "The circle represents an external entity outside the",
         phrase software `sC` D.toSent (phraseNP (the user)) +:+. S "in this case",
       S "The rectangle represents the",
        phrase softwareSys, S "itself" `sC` phrase pidC +:+. S "in this case",
       S "Arrows are used to show the data flow between the",
        D.toSent (phraseNP (system `andIts` environment))]

gsdSysContextP2
  = foldlSPCol
      [phrase pidC, S "is self-contained. The only external interaction is"
         +:+. S "with the user",
       S "The responsibilities of the", D.toSent (phraseNP (user `andThe` system)),
         S "are as follows"]

gsdTitle :: [Sentence]
gsdTitle
  = [titleize user +:+ S "Responsibilities",
     phrase pidC +:+ S "Responsibilities"]

gsdUsrResp :: [Sentence]
gsdUsrResp
  = [S "Feed inputs to the model",
     S "Review the response of the" +:+ phrase powerPlant,
     S "Tune the controller gains"]

gsdSysResp :: [Sentence]
gsdSysResp
  = [S "Check the validity of the inputs",
     S "Calculate the outputs of the" +:+ D.toSent (phraseNP (pidC `and_` powerPlant))]

gsdSysContextList :: Contents
gsdSysContextList
  = UlC $
      ulcc $
        Enumeration $
          bulletNested gsdTitle $ map bulletFlat [gsdUsrResp, gsdSysResp]

gsduserCharacteristics :: Contents
gsduserCharacteristics
  = foldlSP
      [S "The end-user of", phrase pidC,
         S "is expected to have taken a course on Control Systems at an",
         S "undergraduate level"]
