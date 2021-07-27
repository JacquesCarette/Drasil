module Drasil.Website.Documentation (docsSec, docRefs) where

import Language.Drasil

-------------------------------------------
--Haddock Documentation for Drasil Section
-------------------------------------------

-- | Creates the Haddock documentation for all of Drasil.
docsSec :: FilePath -> Section
docsSec path = section haddockDocsTitle [mkParagraph $ haddockDocsDesc path] [] docsSecRef

-- | Haddock Documentation Section title.
haddockDocsTitle :: Sentence
haddockDocsTitle = S "Haddock Documentation"

-- | Body paragraph that directs users to the Haddock documentation and a variant with fully exposed modules.
haddockDocsDesc :: FilePath -> Sentence
haddockDocsDesc path = S "Drasil's framework is primariliy written in Haskell, \
    \so we use Haddock to document our written code. The following link will take you \
    \to the current" +:+ namedRef (docsRef path) (S "Haddock documentation") 
    +:+ S "for the Drasil framework. A variant with"
    +:+ namedRef (fullDocsRef path) (S "fully exposed modules") +:+ S "is also available."

-- | Creates references to the haddock documentation (both normal and full variations).
docsRef, fullDocsRef :: FilePath -> Reference
docsRef path = Reference "haddockDocs" (URI $ path ++ "index.html") $ shortname' $ S "HaddockDocs"
fullDocsRef path = Reference "fullHaddockDocs" (URI $ path ++ "full/index.html") $ shortname' $ S "fullHaddockDocs"

-- | Section Reference.
docsSecRef :: Reference
docsSecRef = makeSecRef "Documentation" $ S "Documentation"

-- | Creates all references used in this file.
docRefs :: FilePath -> [Reference]
docRefs path = [docsRef path, fullDocsRef path, ref $ docsSec path, docsSecRef]
