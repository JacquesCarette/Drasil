module Drasil.Website.Graphs (graphSec, graphRefs) where

import Language.Drasil



----------------------------------------------------------
--graphs section

graphSec :: FilePath -> Section
graphSec path = section packDepGraphTitle [UlC $ ulcc $ folderList path] [] graphSecRef

packDepGraphTitle :: Sentence
drasilFolders :: [String]
drasilDepGraphPaths :: FilePath -> [String]
drasilDepGraphRefs :: FilePath -> [Reference]

packDepGraphTitle = S "Package Dependency Graphs"
drasilFolders = ["drasil-build", "drasil-code", "drasil-data", "drasil-database", "drasil-docLang", "drasil-example", "drasil-gen", "drasil-gool", "drasil-lang", "drasil-printers", "drasil-theory", "drasil-utils"]
drasilDepGraphPaths path = map (\x -> path ++ x ++ ".pdf") drasilFolders
drasilDepGraphRefs path = zipWith (\x y -> Reference x (URI y) (shortname' $ S x) None) drasilFolders $ drasilDepGraphPaths path

folderList :: FilePath -> RawContent
folderList path= Enumeration $ Bullet $ zip (folderList' path) $ repeat Nothing

folderList' :: FilePath -> [ItemType]
folderList' path = map Flat (zipWith (\x y -> namedRef y (S x)) drasilFolders $ drasilDepGraphRefs path)

graphSecRef :: Reference
graphSecRef = makeSecRef "Dependency Graphs" $ S "Dependency Graphs"

graphRefs :: FilePath -> [Reference]
graphRefs path = [graphSecRef, ref $ graphSec path] ++ drasilDepGraphRefs path

