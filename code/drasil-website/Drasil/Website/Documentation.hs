module Drasil.Website.Documentation (docsSec, docRefs) where

import Language.Drasil hiding (C)

----------------------------------------------------
--docs section

docsSec :: FilePath -> Section
docsSec path = section haddockDocsTitle [mkParagraph $ haddockDocsDesc path] [] docsSecRef

haddockDocsTitle :: Sentence
haddockDocsDesc :: FilePath -> Sentence
docsPath, fullDocsPath :: FilePath
docsRef, fullDocsRef :: FilePath -> Reference

haddockDocsTitle = S "Haddock Documentation"
haddockDocsDesc path = S "The current" +:+ namedRef (docsRef path) (S "Haddock documentation") +:+ S "for the Drasil framework. A variant with" +:+ namedRef (fullDocsRef path) (S "fully exposed modules") +:+ S "is also available."
docsPath = "index.html"
fullDocsPath = "full/index.html"
docsRef path = Reference "haddockDocs" (URI (path ++ docsPath)) (shortname' $ S "HaddockDocs") None
fullDocsRef path = Reference "fullHaddockDocs" (URI (path ++ fullDocsPath)) (shortname' $ S "fullHaddockDocs") None

docsSecRef :: Reference
docsSecRef = makeSecRef "Documentation" $ S "Documentation"

docRefs :: FilePath -> [Reference]
docRefs path = [docsRef path, fullDocsRef path, ref $ docsSec path, docsSecRef]
