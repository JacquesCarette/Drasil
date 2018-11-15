{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.Citation
  ( -- Types
    Citation, BibRef, EntryID
    -- Accessors
  , citeID, citeKind
    -- Citation smart constructors
  , cArticle, cBookA, cBookE, cBooklet
  , cInBookACP, cInBookECP, cInBookAC, cInBookEC, cInBookAP, cInBookEP
  , cInCollection, cInProceedings, cManual, cMThesis, cMisc, cPhDThesis
  , cProceedings, cTechReport, cUnpublished
  ) where

import Data.Drasil.IdeaDicts (softEng)
import Language.Drasil.People (People)
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.UID (UID)

import Language.Drasil.Classes.Core (HasUID(uid), HasShortName(shortname))
import Language.Drasil.Classes (HasLabel(getLabel), HasFields(getFields),
  ConceptDomain(cdom), CommonIdea(abrv), NamedIdea(term))
import Language.Drasil.Chunk.CommonIdea (CI, commonIdeaWithDict)
import Language.Drasil.Data.Citation (author, chapter, pages, editor, bookTitle, title, 
  year, school, journal, institution, note, publisher, CitationKind(..), CiteField)
import Language.Drasil.NounPhrase (cn')
import Language.Drasil.Misc (noSpaces)
import Language.Drasil.Label.Core (Label)

import Control.Lens (makeLenses, (^.), view)

type BibRef = [Citation]
type EntryID = String -- Should contain no spaces

-- | All citations require a unique identifier (String) used by the Drasil chunk.
-- We will re-use that as an EntryID (String) used for creating reference links.
-- Finally we will have the reference information (type and fields).
data Citation = Cite
  { _cid :: UID
  , _citeKind :: CitationKind
  , _fields :: [CiteField]
  , _lb :: Label
  , _ci :: CI
  }
makeLenses ''Citation

instance HasUID       Citation where uid       = cid
instance HasLabel     Citation where getLabel  = lb
instance HasShortName Citation where shortname = lb . shortname
instance HasFields    Citation where getFields = fields
instance NamedIdea    Citation where term = ci . term
instance CommonIdea   Citation where abrv = abrv . view ci


citation :: CI
citation  = commonIdeaWithDict "citation"  (cn' "citation")   "Citation"         [softEng]

-- | Smart constructor which implicitly uses EntryID as chunk i.
cite :: EntryID -> CitationKind -> [CiteField] -> Label -> Citation
cite i = (\x y z -> (Cite (noSpaces i) x y z citation))

-- | We don't let anyone know that the EntryID is in fact the UID
citeID :: Citation -> EntryID
citeID c = c ^. cid

-- | Article citation requires author(s), title, journal, year.
-- Optional fields can be: volume, number, pages, month, and note.
-- Implicitly uses the EntryID as the chunk i.
cArticle :: String -> People -> Sentence -> Sentence -> Int -> [CiteField] -> Label -> Citation
cArticle i aut t journ yr opt lbe = cite i Article 
  ((author aut) : (title t) : (journal journ) : (year yr) : opt) lbe

-- | Book citation requires author or editor, title, publisher, year.
-- Optional fields can be volume or number, series, address, edition, month, and note.
-- Implicitly uses the EntryID as the chunk i.
cBookA, cBookE :: String -> People -> Sentence -> Sentence -> Int ->
  [CiteField] -> Label -> Citation
-- | Book citation by author
cBookA i aut t pub yr opt lbe = cite i Book (author aut : stdFields t pub yr opt) lbe
-- | Book citation by editor
cBookE i ed t pub yr opt lbe = cite i Book (editor ed : stdFields t pub yr opt) lbe

-- | Booklet citation requires title.
-- Optional fields can be author, how published, address, month, year, note.
-- Implicitly uses the EntryID as the chunk i.
cBooklet :: String -> Sentence -> [CiteField] -> Label -> Citation
cBooklet i t opt lbe = cite i Booklet (title t : opt) lbe

-- | InBook citation requires author or editor, title, chapter and/or pages,
-- publisher, year. Optional fields can be volume or number, series, type,
-- address, edition, month, and note.
-- Implicitly uses the EntryID as the chunk i.
-- This smart constructor includes both chapter and page numbers.
cInBookACP, cInBookECP :: String -> People -> Sentence -> Int -> [Int] ->
  Sentence -> Int -> [CiteField] -> Label -> Citation
-- | InBook citation by author.
cInBookACP i auth t chap pgs pub yr opt lbe = cite i InBook 
  (author auth : chapter chap : pages pgs : stdFields t pub yr opt) lbe
-- | InBook citation by editor.
cInBookECP i ed t chap pgs pub yr opt lbe = cite i InBook 
  (editor ed : chapter chap : pages pgs : stdFields t pub yr opt) lbe

-- | InBook citation excluding page numbers.
cInBookAC, cInBookEC :: String -> People -> Sentence -> Int ->
  Sentence -> Int -> [CiteField] -> Label -> Citation

-- | Otherwise ientical to 'cInBookACP'
cInBookAC i auth t chap pub yr opt lbe = cite i InBook 
  (author auth : chapter chap : stdFields t pub yr opt) lbe
-- | Otherwise ientical to 'cInBookECP'
cInBookEC i ed t chap pub yr opt lbe = cite i InBook 
  (editor ed : chapter chap : stdFields t pub yr opt) lbe

-- | InBook citation excluding chapter.
cInBookAP, cInBookEP :: String -> People -> Sentence -> [Int] ->
  Sentence -> Int -> [CiteField] -> Label -> Citation

-- | Otherwise ientical to 'cInBookACP'
cInBookAP i auth t pgs pub yr opt lbe = cite i InBook 
  (author auth : pages pgs : stdFields t pub yr opt) lbe
-- | Otherwise ientical to 'cInBookECP'
cInBookEP i ed t pgs pub yr opt lbe = cite i InBook 
  (editor ed : pages pgs : stdFields t pub yr opt) lbe

-- | InCollection citation requires author, title, bookTitle, publisher, year.
-- Optional fields can be editor, volume or number, series, type, chapter,
-- pages, address, edition, month, and note.
-- Implicitly uses the EntryID as the chunk i.
cInCollection :: String -> People -> Sentence -> Sentence -> Sentence -> Int ->
  [CiteField] -> Label -> Citation
cInCollection i auth t bt pub yr opt lbe = cite i InCollection 
  (author auth : bookTitle bt : stdFields t pub yr opt) lbe

-- | InProceedings citation requires author, title, bookTitle, year.
-- Optional fields can be editor, volume or number, series, pages,
-- address, month, organization, publisher, and note.
-- Implicitly uses the EntryID as the chunk i.
cInProceedings :: String -> People -> Sentence -> Sentence -> Int ->
  [CiteField] -> Label -> Citation
cInProceedings i auth t bt yr opt lbe = cite i InProceedings 
  (author auth : title t : bookTitle bt : year yr : opt) lbe

-- | Manual (technical documentation) citation requires title.
-- Optional fields can be author, organization, address, edition, month, year, and note.
-- Implicitly uses the EntryID as the chunk i.
cManual :: String -> Sentence -> [CiteField] -> Label -> Citation
cManual i t opt lbe = cite i Manual (title t : opt) lbe

-- | Master's Thesis citation requires author, title, school, and year.
-- Optional fields can be type, address, month, and note.
-- Implicitly uses the EntryID as the chunk i.
cMThesis :: String -> People -> Sentence -> Sentence -> Int -> [CiteField] -> Label -> Citation
cMThesis i auth t sch yr opt lbe = cite i MThesis (thesis auth t sch yr opt) lbe

-- | Misc citation requires nothing.
-- Optional fields can be author, title, howpublished, month, year, and note.
-- Implicitly uses the EntryID as the chunk i.
cMisc :: String -> [CiteField] -> Label -> Citation
cMisc i opt lbe = cite i Misc opt lbe

-- | PhD Thesis citation requires author, title, school, and year.
-- Optional fields can be type, address, month, and note.
-- Implicitly uses the EntryID as the chunk i.
cPhDThesis :: String -> People -> Sentence -> Sentence -> Int -> [CiteField] -> Label -> Citation
cPhDThesis i auth t sch yr opt lbe = cite i PhDThesis (thesis auth t sch yr opt) lbe

-- | Proceedings citation requires title and year.
-- Optional fields can be editor, volume or number, series, address,
-- publisher, note, month, and organization.
-- Implicitly uses the EntryID as the chunk i.
cProceedings :: String -> Sentence -> Int -> [CiteField] -> Label -> Citation
cProceedings i t yr opt lbe = cite i Proceedings (title t : year yr : opt) lbe

-- | Technical Report citation requires author, title, institution, and year.
-- Optional fields can be type, number, address, month, and note.
-- Implicitly uses the EntryID as the chunk i.
cTechReport :: String -> People -> Sentence -> Sentence -> Int -> [CiteField] -> Label -> Citation
cTechReport i auth t inst yr opt lbe = cite i TechReport 
  (author auth : title t : institution inst : year yr : opt) lbe

-- | Unpublished citation requires author, title, and note.
-- Optional fields can be month and year.
-- Implicitly uses the EntryID as the chunk i.
cUnpublished :: String -> People -> Sentence -> Sentence -> [CiteField] -> Label -> Citation
cUnpublished i auth t n opt lbe = cite i Unpublished 
  (author auth : title t : note n : opt) lbe

-- Helper function (do not export) for creating book reference.
stdFields :: Sentence -> Sentence -> Int -> [CiteField] -> [CiteField]
stdFields t pub yr opt = title t : publisher pub : year yr : opt

-- Helper function (do not export) for creating thesis reference.
thesis :: People -> Sentence -> Sentence -> Int -> [CiteField] -> [CiteField]
thesis auth t sch yr opt = author auth : title t : school sch : year yr : opt
