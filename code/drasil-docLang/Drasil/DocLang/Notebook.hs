module Drasil.DocLang.Notebook where

import Language.Drasil

import qualified Data.Drasil.Concepts.Documentation as Doc (notebook, introduction, 
  review, body, procForAnls, summary, method_, example, result, reference)

-- | Notebook constructor. 
-- Create the notebook from given system name, authors, and sections
doc :: NamedIdea c => c -> Sentence -> [Section] -> Document
doc  sys = Document (Doc.notebook `forTT` sys)

intro, body, review, procForAnls, summary, method, example, result, 
  reference :: [Contents] -> [Section] -> Section
  
intro       cs ss = section' (titleize Doc.introduction) cs ss "Intro"
review      cs ss = section' (titleize Doc.review)       cs ss "Review"
body        cs ss = section' (titleize Doc.body)         cs ss "Body"

summary     cs ss = section' (titleize Doc.summary)      cs ss "Summary"

method      cs ss = section' (titleize Doc.method_)      cs ss "Method"
procForAnls cs ss = section  (titleize Doc.procForAnls)  cs ss AnlsProcLabel
example     cs ss = section' (titleize Doc.example)      cs ss "Example"

result      cs ss = section' (titleize Doc.result)       cs ss "Result"

reference   cs ss = section  (titleize' Doc.reference)   cs ss referenceLabel

--function that sets the shortname of each section to be the reference address
section' :: Sentence -> [Contents] -> [Section] -> String -> Section
section' a b c d = section a b c (makeSecRef d (toString a))
  where
    toString :: Sentence -> String --FIXME: same as sentenceDoc, import instead? 
    toString (S x) = x
    toString ((:+:) s1 s2) = toString s1 ++ toString s2
    toString _ = error "Term is not a string"

--Labels--
AnlsProcLabel, referenceLabel :: Reference
AnlsProcLabel      = makeSecRef "AnlsProcedure" "Procedure for Analysis"
referenceLabel     = makeSecRef "References"    "References"