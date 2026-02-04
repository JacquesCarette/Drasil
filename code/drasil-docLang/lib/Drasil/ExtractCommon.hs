module Drasil.ExtractCommon (
  sentToExp, getCon', getCon, getCont, getContList
) where

import Control.Lens((^.))

import Language.Drasil hiding (getCitations, Manual, Verb)

-- | Converts a 'Sentence' into a list of expressions. If the 'Sentence' cant be
-- translated, returns an empty list.
sentToExp :: Sentence -> [ModelExpr]
sentToExp ((:+:) s1 s2) = sentToExp s1 ++ sentToExp s2
sentToExp (E e) = [e]
sentToExp _ = []

-- | Extracts 'Sentence's from something that has contents.
getCon' :: HasContents a => a -> [Sentence]
getCon' = getCon . (^. accessContents)

-- | Extracts reference 'UID's from 'Content's.
getCont :: Contents -> [Sentence]
getCont (UlC (UnlblC rc)) = getCon rc
getCont (LlC lc) = getCon (lc ^. accessContents)

-- | Extracts 'Sentence's from a list of 'Contents'.
getContList :: [Contents] -> [Sentence]
getContList = concatMap getCon'

-- | Extracts 'Sentence's from raw content.
getCon :: RawContent -> [Sentence]
getCon (Table s1 s2 t _)   = t : s1 ++ concat s2
getCon (Paragraph s)       = [s]
getCon EqnBlock{}          = []
getCon CodeBlock{}         = []
getCon (DerivBlock h d)    = h : concatMap getCon d
getCon (Enumeration lst)   = getLT lst
getCon (Figure l _ _ _)    = [l]
getCon (Bib bref)          = getBib bref
getCon (Graph sss _ _ l)   = let (ls, rs) = unzip sss
                             in l : ls ++ rs
getCon (Defini _ ics)      = concatMap (concatMap getCon' . snd) ics

-- | Get the bibliography from something that has a field.
getBib :: (HasFields c) => [c] -> [Sentence]
getBib a = map getField $ concatMap (^. getFields) a

-- | Unwraps a 'CiteField' into a 'Sentence'.
getField :: CiteField -> Sentence
getField (Address s) = S s
getField Author{} = EmptyS
getField (BookTitle s) = S s
getField Chapter{} = EmptyS
getField Edition{} = EmptyS
getField Editor{} = EmptyS
getField HowPublished{} = EmptyS
getField (Institution s) = S s
getField (Journal s) = S s
getField Month{} = EmptyS
getField (Note s) = S s
getField Number{} = EmptyS
getField (Organization s) = S s
getField Pages{} = EmptyS
getField (Publisher s) = S s
getField (School s) = S s
getField (Series s) = S s
getField (Title s) = S s
getField (Type s) = S s
getField Volume{} = EmptyS
getField Year{} = EmptyS

-- | Translates different types of lists into a 'Sentence' form.
getLT :: ListType -> [Sentence]
getLT (Bullet it)      = concatMap (getIL . fst) it
getLT (Numeric it)     = concatMap (getIL . fst) it
getLT (Simple lp)      = concatMap getLP lp
getLT (Desc lp)        = concatMap getLP lp
getLT (Definitions lp) = concatMap getLP lp

-- | Translates a 'ListTuple' into 'Sentence's.
getLP :: ListTuple -> [Sentence]
getLP (t, it, _) = t : getIL it

-- | Flattens out an ItemType into 'Sentence's. Headers for 'Nested' items are
-- prepended to its contents.
getIL :: ItemType -> [Sentence]
getIL (Flat s) = [s]
getIL (Nested h lt) = h : getLT lt
