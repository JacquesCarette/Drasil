-- | Defines helpers for printing 'Sentence's.
module Language.Drasil.Printing.Import.Sentence (spec) where

import Control.Lens ((^.))
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)

import Drasil.Database (UIDRef, raw, findOrErr, IsChunk)
import Drasil.Database.SearchTools (termResolve, termResolve', termResolveNP,
  TermAbbr(..))
import Language.Drasil (Sentence(..), NP, NounPhrase(..), (+:+), sParen,
  atStartNP, atStartNP', titleizeNP, titleizeNP', foldNums, checkValidStr,
  SentenceStyle(..), TermCapitalization(..), RefInfo(..), NamedIdea, Idea)
import Language.Drasil.Development (toSent)
import Language.Drasil.Document (IRefProg(..), LblType(..), ShortName,
  getSentSN, HasRefAddress(..), HasShortName(..))
import qualified Drasil.Document.Layout.Internal as P
import Language.Drasil.Printing.PrintingInformation (PrintingInformation, sysdb)
import Language.Drasil.Printing.Import.ExprCommon (lookupSymb)
import Language.Drasil.Printing.Import.ModelExpr (modelExpr)
import Language.Drasil.Printing.Import.Symbol (symbol, pUnit)

-- * Main Function

-- | Translates 'Sentence' to the printable representation of a 'Sentence' ('Spec').
spec :: PrintingInformation -> Sentence -> P.Spec
  -- make sure these optimizations are clear
spec sm (EmptyS :+: b)          = spec sm b
spec sm (a :+: EmptyS)          = spec sm a
spec sm (a :+: b)               = spec sm a P.:+: spec sm b
spec _  (S s)                   = either error P.S $ checkValidStr s invalidChars
  where invalidChars = ['<', '>', '\"', '&', '$', '%', '&', '~', '^', '\\', '{', '}']
spec _  (Sy s)                  = P.E $ pUnit s
spec sm (NP np)                 = spec sm (toSent $ phraseNP np)
spec _  Percent                 = P.E $ P.MO P.Perc
spec _  (P s)                   = P.E $ symbol s
spec sm (SyCh s)                = P.E $ symbol $ lookupSymb sm s

-- First term is the tooltip, second term is the rendered short form
spec sm (Ch ShortStyle caps s)  =
  let (tooltip, shortSent) = lookupS sm s caps
  in P.Tooltip (spec sm tooltip) (spec sm shortSent)

spec sm (Ch TermStyle caps s)   = spec sm $ lookupT sm s caps
spec sm (Ch PluralTerm caps s) = spec sm $ lookupP sm s caps
spec sm (Ref u dName notes)     = specRef sm u dName notes
spec sm (Quote q)          = P.Quote $ spec sm q
spec _  EmptyS             = P.EmptyS
spec sm (E e)              = P.E $ modelExpr e sm

-- * Helpers

-- | Render a chunk reference. Displays the given name, or a default one if it
-- is empty.
specRef :: forall t. (IsChunk t, HasRefAddress t, HasShortName t) =>
  PrintingInformation -> UIDRef t -> Sentence -> RefInfo -> P.Spec
specRef sm u dName notes = case getRefAdd c of
    RP rp ra    -> P.Ref P.Internal ra $ spec sm $ named $ renderShortName sm rp sn
    Citation ra -> P.Ref (P.Cite2 $ spec sm $ renderCitInfo notes) ra $ spec sm $ named $ S ra
    URI ra      -> P.Ref P.External ra $ spec sm $ named $ getSentSN sn
  where
    c = findOrErr (raw u) (sm ^. sysdb) :: t
    sn = shortname c
    named d = case dName of
      EmptyS -> d
      _      -> dName

-- | Renders the shortname of a reference/domain.
renderShortName :: PrintingInformation -> IRefProg -> ShortName -> Sentence
renderShortName pinfo (Deferred u) _ = S $ fromMaybe (error "Domain has no abbreviation.") $ shortForm $ termResolve TermAbbr (pinfo ^. sysdb) u
renderShortName pinfo (RConcat a b) sn = renderShortName pinfo a sn :+: renderShortName pinfo b sn
renderShortName _ (RS s) _ = S s
renderShortName _ Name sn = getSentSN sn

-- | Renders citation information.
renderCitInfo :: RefInfo -> Sentence
renderCitInfo  None          = EmptyS
renderCitInfo (RefNote   rn) = sParen (S rn)
renderCitInfo (Equation [x]) = sParen (S "Eq." +:+ S (show x))
renderCitInfo (Equation  i ) = sParen (S "Eqs." +:+ foldNums "-" i)
renderCitInfo (Page     [x]) = sParen (S "pg." +:+ S (show x))
renderCitInfo (Page      i ) = sParen (S "pp." +:+ foldNums "-" i)

-- | Look up the tooltip and acronym/abbreviation of a term (falling back to the
-- singular form if there is no abbreviation). Takes a chunk database and a
-- 'UIDRef' associated with the term.
lookupS :: Idea t => PrintingInformation -> UIDRef t -> TermCapitalization -> (Sentence, Sentence)
lookupS sm c sCap = (tooltip, maybe tooltip S $ shortForm l >>= capHelper sCap)
  where
    l = termResolve' (sm ^. sysdb) c
    tooltip = resolveCapT sCap $ longForm l

    -- | Get the capital case of an abbreviation based on
    -- 'TermCapitalization'. For sentence and title cases.
    capHelper :: TermCapitalization -> String -> Maybe String
    capHelper NoCap s      = pure s
    capHelper _     []     = Nothing
    capHelper _     (x:xs) = Just (toUpper x : xs)

-- | Look up a term given a chunk database and a 'UIDRef' associated with the term.
-- Also specifies capitalization
lookupT :: NamedIdea t => PrintingInformation -> UIDRef t -> TermCapitalization -> Sentence
lookupT sm c tCap = resolveCapT tCap $ termResolveNP (sm ^. sysdb) c

-- | Get the right function for capitalizing a 'NP' based on its
-- 'TermCapitalization'. Singular case.
resolveCapT :: TermCapitalization -> (NP -> Sentence)
resolveCapT NoCap = toSent . phraseNP
resolveCapT CapF = toSent . atStartNP
resolveCapT CapW = toSent . titleizeNP

-- | Look up the plural form of a term given a chunk database and a 'UIDRef'
-- associated with the term.
lookupP :: NamedIdea t => PrintingInformation -> UIDRef t -> TermCapitalization -> Sentence
lookupP sm c pCap = resolveCapP pCap $ termResolveNP (sm ^. sysdb) c
  where
    -- | Get the right function for capitalizing a 'NP' based on its
    -- 'TermCapitalization'. Plural case.
    resolveCapP :: TermCapitalization -> (NP -> Sentence)
    resolveCapP NoCap = toSent . pluralNP
    resolveCapP CapF = toSent . atStartNP'
    resolveCapP CapW = toSent . titleizeNP'
