{-# LANGUAGE PostfixOperators #-}
-- | About Drasil
module Drasil.Website.About where

import Language.Drasil
import Drasil.Document.Contents (enumBulletU)

-- * About Section

-- | Reusable wiki page sentence.
wikiSentence :: Sentence
wikiSentence = S "wiki page"

-- | Creates the about section.
aboutSec :: Reference -> Reference -> Reference -> Reference -> Reference -> Reference -> Reference -> Reference ->
  Reference -> Reference -> Reference -> Reference -> Reference -> Section
aboutSec csRef docRef analysisSecRef repoRef wikiRef infoEncodingWiki chunksWiki recipesWiki paperGOOL papersWiki
  icsePositionPaper danPoster wellUnderstoodPaper =
  section (S "About") -- Title
  (map mkParagraph [aboutParagraph1 repoRef wikiRef, aboutParagraph2 csRef docRef analysisSecRef, aboutParagraph3]
  ++ [currentlyGeneratedArtifacts] ++ [mkParagraph aboutParagraph4] ++ [futureGeneratedArtifacts] ++ map mkParagraph
  [aboutParagraph5 infoEncodingWiki, aboutParagraph6 chunksWiki, aboutParagraph7 recipesWiki, aboutParagraph8 paperGOOL,
  aboutParagraph9 papersWiki icsePositionPaper danPoster wellUnderstoodPaper]) -- Contents
  [] $ makeSecRef "About" $ S "About" -- Section reference

-- | Paragraph to about Drasil and its goals.
aboutParagraph1 :: Reference -> Reference -> Sentence
aboutParagraph1 repoRef wikiRef = S "Drasil is a framework for generating all of the software artifacts \
  \from a stable knowledge base, focusing currently on scientific software. The main goals are to reduce knowledge \
  \duplication and improve traceability. The artifacts are generated from a common knowledge-base using recipes \
  \written in a Domain-Specific Language (DSL). These recipes allow us to specify which pieces of \
  \knowledge should be used in which artifacts, how to transform them, and more. For more information on the design, documentation, \
  \usage, and specifics of Drasil, please visit the" +:+ namedRef repoRef (S "GitHub repository") +:+ S "or the" +:+
  (namedRef wikiRef (S "GitHub Wiki") !.)

-- | Paragraph to describe the layout of the rest of the Drasil website.
aboutParagraph2 :: Reference -> Reference -> Reference -> Sentence
aboutParagraph2 caseStudySecRef docsRef analysisSecRef = S "This webpage is designed to contain the most up-to-date" +:+
  foldlList Comma List (zipWith (\x y -> namedRef x (S y)) [caseStudySecRef, docsRef, analysisSecRef] ["case study artifacts",
  "Haddock documentation", "Drasil analysis"]) +:+ S "from the Drasil repository. \
  \The case study artifacts include the Software Requirements Specification (SRS) for each case study, \
  \which specifies what the program sets out to achieve. \
  \The Haddock Documentation section contains the current documentation for the Drasil framework. \
  \The package dependency graphs show the hierarchy of modules within each package."
  -- \The footer of this page contains the continuous integration build of the project, \
  -- \as well as the commit number that the build and artifacts are based off of.

-- | Lists artifacts that Drasil generates
aboutParagraph3 :: Sentence
aboutParagraph3 = S "The following is a list of artifacts that Drasil currently generates:"

currentlyGeneratedArtifacts :: Contents
currentlyGeneratedArtifacts = enumBulletU $ map foldlSent_
  [[S "SRS"],
  [S "code"],
  [S "README"],
  [S "Makefile"]]

-- | Lists artifacts that we hope to generate with Drasil
aboutParagraph4 :: Sentence
aboutParagraph4 = S "We hope to generate the following artifacts in the future:"

futureGeneratedArtifacts :: Contents
futureGeneratedArtifacts = enumBulletU $ map foldlSent_
  [[S "License"],
  [S "Installation Instructions"],
  [S "Dependency List"],
  [S "Authors"],
  [S "Getting Started / User Manual"],
  [S "Release Info"],
  [S "Design Documentation"],
  [S "Build Scripts"],
  [S "Test Cases"]]

-- | Paragraph describing information encoding
aboutParagraph5 :: Reference -> Sentence
aboutParagraph5 infoEncodingWiki = S "As described in the" +:+ namedRef infoEncodingWiki (S "Information Encoding") +:+
  sC wikiSentence (S "Drasil uses specific terminology") +:+ S "to address types of \
  \knowledge for the purpose of encoding information, since we know that we want to eventually generate words, sentences, paragraphs, \
  \whole documents with headings, references, formulas, tables, graphs, and code. This is done by trying to understand the basic 'units' \
  \of all artifacts, and methods for composing larger structures from these units. The removal of duplicate units is an important feature \
  \of this methodology. The basic building blocks of the methodology include different expressions for units with a specific meaning. \
  \These are built into ontologies of domains that address broader knowledge. Chunks form a fundamental part of such ontologies."

-- | Paragraph describing chunks
aboutParagraph6 :: Reference -> Sentence
aboutParagraph6 chunksWiki = S "As described in the" +:+ namedRef chunksWiki (S "Chunks") +:+
  sC wikiSentence (S "a chunk is a data type specialized") +:+ S "in holding a specific type of information for \
  \a specific purpose so that knowledge may be used in generated models, definitions, and theories. Chunks are usually made up of several \
  \lower-level types that hold lower-lever information; when contained together, these pieces of lower-level information hold a new specific \
  \purpose. The structure of a chunk can be thought of as a wrapper of information, and this is all implemented using Haskell's record-type \
  \syntax. Recipes transform the acquired knowledge into a usable format."

-- | Paragraph describing recipes
aboutParagraph7 :: Reference -> Sentence
aboutParagraph7 recipesWiki = S "As described in the" +:+ namedRef recipesWiki (S "Recipes") +:+
  sC wikiSentence (S "recipes are instructions that") +:+
  S "unpackage necessary information from chunks and send \
  \that information to Drasil generators/printers to build complete artifacts. When an artifact needs to be changed, the recipe is modified to \
  \unpackage the additional necessary information from a chunk, or alternatively to omit unpackaging information that is no longer required."

-- | Paragraph describing GOOL
aboutParagraph8 :: Reference -> Sentence
aboutParagraph8 paperGOOL = S "As described in the" +:+ namedRef paperGOOL (S "GOOL") +:+ S "paper, this is a Generic Object-Oriented Language \
  \that provides intermediary assistance in code \
  \generation, allowing Drasil to more efficiently generate code in several languages, including Python, Java, C-Sharp, and C++."

-- | Paragraph providing a link to Drasil papers and documents
aboutParagraph9 :: Reference -> Reference -> Reference -> Reference -> Sentence
aboutParagraph9 papersWiki icsePositionPaper danPoster wellUnderstoodPaper =
  S "A list of papers and documents written about Drasil can be found on the" +:+
  namedRef papersWiki (S "Drasil Papers and Documents") +:+. wikiSentence +:+
  S "In particular" `sC` S "there is" +:+. foldlList Comma List [
    S "an" +:+ namedRef icsePositionPaper (S "Old Position paper") +:+
      S "outlining our original ideas",
    S "a" +:+ namedRef danPoster (S "Drasil poster"),
    S "a" +:+ namedRef wellUnderstoodPaper (S "Well-Understood paper") +:+
      S "discussing key concepts"
  ]
