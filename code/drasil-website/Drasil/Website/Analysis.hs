module Drasil.Website.Analysis where

import Language.Drasil

--------------------
-- Analysis Section
--------------------

-- | Creates the Analysis of Drasil Section.
analysisSec :: FilePath -> [String] -> Section
analysisSec path pkgs = section drasilAnalysisTitle [mkParagraph $ analysisIntro path] [] analysisSecRef

-- | Analysis section title.
drasilAnalysisTitle :: Sentence
drasilAnalysisTitle = S "Analysis of Drasil"

-- | Analysis section introduction. Also introduces the Data Table.
analysisIntro :: FilePath -> Sentence
analysisIntro path = S "This section contains an graphs and tables that may be used to analyze the \
  \structure of the Drasil framework. Here, we will explore the relationship between data types, \
  \classes, and instances of those classes within Drasil. This generated" +:+ namedRef (dataTableHTMLRef path) (S "Data Table") +:+
  S "keeps track of all the different types, classes, and where they intersect. \
  \The rows are organized in order of Drasil packages, then Drasil modules, and lastly by data type. \
  \The data types are further separated by their structure; those labelled \
  \Data Type are completely new types created and used in Drasil, while Newtype Types are \
  \type synonyms or wrappers of other types. All of the classes in Drasil are defined as \
  \column headers, starting from Haskell-native classes like Eq and going through every \
  \unique Drasil-defined class. A box marked with \
  \'YYYY' symbolizes the file location of where that particular data type is an instance of a particular class. \
  \There is also a" +:+ namedRef (dataTableCSVRef path) (S "downloadable version") +:+ S "of the Data Table available as a .csv file."

-- | Data table references.
dataTableHTMLRef, dataTableCSVRef :: FilePath -> Reference
dataTableHTMLRef path = Reference "dataTableHTML" (URI $ path ++ "DataTable/DataTable.html") (shortname' $ S "dataTableHTML")
dataTableCSVRef path = Reference "dataTableCSV" (URI $ path ++ "DataTable/DataTable.csv") (shortname' $ S "dataTableCSV")

typeGraphTable :: FilePath -> [String] -> [[Sentence]]
typeGraphTable path pkgs = [S "Drasil Package", S "Data Type Structure", S "Data Type Structure", S "Class and Data Type Structure", S "Class and Data Type Structure"]
    : map (typeGraphTable' path) pkgs

typeGraphTable' :: FilePath -> FilePath -> String -> [Sentence]
typeGraphTable' typePath clsInstPath pkg = [S pkg, namedRef (getDataTypeRef "" typePath pkg) (S "Dot Graph"), namedRef (getDataTypeRef "circo_" typePath pkg) (S "Circo Graph"), namedRef (getDataTypeRef "" clsInstPath pkg) (S "Dot Graph"), namedRef (getDataTypeRef "circo_" clsInstPath pkg) (S "Circo Graph")]

getDataTypeRef :: String -> FilePath -> String -> Reference
getDataTypeRef prfx path pkg = Reference (pkg ++ sufx ++ "graph") (URI $ path ++ prfx ++ pkg ++ ".svg") $ shortname' $ S $ pkg ++ sufx ++ "graph"

-- | Analysis section reference
analysisSecRef :: Reference
analysisSecRef = makeSecRef "Analysis" $ S "Analysis"

-- | Gathers all references used in this file.
analysisRefs :: FilePath -> [Reference]
analysisRefs path = [analysisSecRef, dataTableHTMLRef path, dataTableCSVRef path, ref $ analysisSec path]

