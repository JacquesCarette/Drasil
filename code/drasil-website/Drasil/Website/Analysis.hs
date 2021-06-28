module Drasil.Website.Analysis where

import Language.Drasil

--------------------
-- Analysis Section
--------------------

analysisSec :: FilePath -> Section
analysisSec path = section drasilDataTableTitle [mkParagraph $ dataTableDesc path] [] analysisSecRef

drasilDataTableTitle :: Sentence
dataTableDesc :: FilePath -> Sentence
dataTableHTMLPath, dataTableCSVPath :: FilePath
dataTableHTMLRef, dataTableCSVRef :: FilePath -> Reference

drasilDataTableTitle = S "Drasil Data Table"
dataTableDesc path = S "Here is the updated" +:+ namedRef (dataTableHTMLRef path) (S "Data Table") +:+ S "for the Drasil framework. There is also a" +:+ namedRef (dataTableCSVRef path) (S "downloadable version") +:+ S "(csv format)."
dataTableHTMLPath = "DataTable/DataTable.html"
dataTableCSVPath = "DataTable/DataTable.csv"
dataTableHTMLRef path = Reference "dataTableHTML" (URI (path ++ dataTableHTMLPath)) (shortname' $ S "dataTableHTML") None
dataTableCSVRef path = Reference "dataTableCSV" (URI (path ++ dataTableCSVPath)) (shortname' $ S "dataTableCSV") None

analysisSecRef :: Reference
analysisSecRef = makeSecRef "Analysis" $ S "Analysis"

analysisRefs :: FilePath -> [Reference]
analysisRefs path = [analysisSecRef, dataTableHTMLRef path, dataTableCSVRef path, ref $ analysisSec path]

