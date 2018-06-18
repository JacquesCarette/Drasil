module Language.Drasil.SentenceExtract (getDoc)where

import Language.Drasil.Document
import Language.Drasil.Spec
import Language.Drasil.Chunk.AssumpChunk (AssumpChunk)
import Language.Drasil.Chunk.Change (Change)
import Language.Drasil.Chunk.Citation (BibRef)
import Language.Drasil.Chunk.Eq (QDefinition)
import Language.Drasil.Chunk.Relation (RelationConcept)
import Language.Drasil.Chunk.ReqChunk (ReqChunk)

getDoc :: Document -> [Sentence]
getDoc (Document t a s) = [t] ++ [a] ++ (concatMap getSec s)

getSec :: Section -> [Sentence]
getSec (Section t sc _ _) = [t] ++ (concatMap getSecCon sc)

getSecCon :: SecCons -> [Sentence]
getSecCon (Sub s) = getSec s
getSecCon (Con c) = getCon c

getCon :: Contents -> [Sentence]
getCon (Table s1 [s2] t _ _) = s1 ++ s2 ++ [t]
getCon (Paragraph s) = [s]
getCon (EqnBlock _ _) = []
getCon (Definition d) = getDtype d
getCon (Enumeration lst) = getLT lst
getCon (Figure l _ _ _) = [l]
getCon (Requirement reqc) = getReq reqc 
getCon (Assumption assc) = getAss assc
getCon (Change chg) = getChg chg
getCon (Bib bref) = getBib bref
getCon (Graph [(s1, s2)] _ _ l _) = [s1] ++ [s2] ++ [l]
getCon (Defnt dt [(_, con)] _) = (getDtype dt) ++ (concatMap getCon con)
getCon _ = []

----- not done ------
getDtype :: DType -> [Sentence]
getDtype (Data q) = getQDef q ----
getDtype (Theory t) = getRelaConc t ----
getDtype _ = []

---- not done --------
getQDef :: QDefinition -> [Sentence]
getQDef (_) = []

---- not done -------
getRelaConc :: RelationConcept -> [Sentence]
getRelaConc (_) = []

----- not done ------
getReq :: ReqChunk -> [Sentence]
getReq (_) = []

----- not done ------
getAss :: AssumpChunk -> [Sentence]
getAss (_) = []

----- not done ------
getChg :: Change -> [Sentence]
getChg (_) = []

getLT :: ListType -> [Sentence]
getLT (Bullet it) = concatMap getIL it
getLT (Numeric it) = concatMap getIL it
getLT (Simple lp) = concatMap getLP lp
getLT (Desc lp) = concatMap getLP lp
getLT (Definitions lp) = concatMap getLP lp


getIL :: ItemType -> [Sentence]
getIL (Flat s) = [s]
getIL (Nested h lt) = [h] ++ (getLT lt)

getLP :: ListPair -> [Sentence]
getLP (t, it) = [t] ++ (getIL it)

getBib :: BibRef -> [Sentence]
getBib (_) = []

