module Language.Drasil.Code.Imperative.Comments (
  paramComment, returnComment
) where

getTermDoc :: (NamedIdea c) => UID -> Map UID c -> Reader State Doc
getTermDoc cname m = do
  g <- ask
  let db = sysinfodb $ csi $ codeSpec g
  return $ (maybe (text "No description given") (sentenceDoc db 
    Implementation Linear . phraseNP . view term) . Map.lookup cname) m

getUnitsDoc :: (MayHaveUnit c) => UID -> Map UID c -> Doc
getUnitsDoc cname m = maybe empty (parens . unitDoc Linear . usymb) 
  (Map.lookup cname m >>= getUnit)

getComment :: (NamedIdea c, MayHaveUnit c) => UID -> Map UID c -> 
  Reader State String
getComment l m = do
  t <- getTermDoc l m
  let u = getUnitsDoc l m
  return $ render $ t <+> u

paramComment :: UID -> Reader State String
paramComment l = do
  g <- ask
  let m = vMap $ codeSpec g
  getComment l m

returnComment :: UID -> Reader State String
returnComment l = do
  g <- ask
  let m = fMap $ codeSpec g
  getComment l m