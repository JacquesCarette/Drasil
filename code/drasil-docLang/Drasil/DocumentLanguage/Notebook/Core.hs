module Drasil.DocumentLanguage.Notebook.Core where

type DocDesc = [DocSection]


data DocSection = IntroSec IntroSec
                | BodySec BodySec
                | SmmrySec SmmrySec
                | Bibliography
                | AppndxSec AppndxSec

{--}

-- | Introduction section. Contents are top level followed by a list of
-- subsections.
data IntroSec = IntroProg Sentence Sentence [IntroSub]
  -- ^ Temporary, will be modified once we've figured out more about the section.

-- | Introduction subsections
data IntroSub where
  IPurpose :: [Sentence] -> IntroSub
  IScope   :: Sentence -> IntroSub

{--}

newtype BodySec = BodyProg [BodySub]

data BodySub where
  Review       :: [Contents] -> [Section] -> BodySub 
  MethsAndAnls :: [Contents] -> [Section] -> BodySub

{--}

newtype AppndxSec = AppndxProg [Contents]

{--}