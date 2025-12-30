-- | Defines helpers for printing 'Sentence's.
module Language.Drasil.Printing.Import.Sentence where

import Control.Lens ((^.))
import Data.Maybe (fromMaybe)

import Drasil.Database (unRef)
import Language.Drasil hiding (neg, sec, symbol, isIn)
import Language.Drasil.Development (toSent)
import Drasil.Database.SearchTools (termResolve', TermAbbr(..))
import Drasil.System (systemdb)

import qualified Language.Drasil.Printing.AST as P
import Language.Drasil.Printing.PrintingInformation
  (PrintingInformation, refFind, syst)
import Language.Drasil.Printing.Import.ModelExpr (modelExpr)
import Language.Drasil.Printing.Import.Helpers (lookupT, lookupS, lookupP, lookupC')
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
spec sm (SyCh s)                = P.E $ symbol $ lookupC' sm (unRef s)

-- First term is the tooltip, second term is the rendered short form
spec sm (Ch ShortStyle caps s)  = P.Tooltip (spec sm $ lookupT
  sm (unRef s) caps) (spec sm $ lookupS sm (unRef s) caps)

spec sm (Ch TermStyle caps s)   = spec sm $ lookupT sm (unRef s) caps
spec sm (Ch PluralTerm caps s) = spec sm $ lookupP sm (unRef s) caps
spec sm (Ref u EmptyS notes)    =
  let reff = refFind (unRef u) sm in
  case reff of
    (Reference _ (RP rp ra) sn) ->
      P.Ref P.Internal ra (spec sm $ renderShortName sm rp sn)
    (Reference _ (Citation ra) _) ->
      P.Ref (P.Cite2 (spec sm (renderCitInfo notes)))    ra (spec sm $ S ra)
    (Reference _ (URI ra) sn) ->
      P.Ref P.External    ra (spec sm $ renderURI sm sn)
spec sm (Ref u dName notes) =
  let reff = refFind (unRef u) sm in
  case reff of
    (Reference _ (RP _ ra) _) ->
      P.Ref P.Internal ra (spec sm dName)
    (Reference _ (Citation ra) _) ->
      P.Ref (P.Cite2 (spec sm (renderCitInfo notes)))   ra (spec sm dName)
    (Reference _ (URI ra) _) ->
      P.Ref P.External    ra (spec sm dName)
spec sm (Quote q)          = P.Quote $ spec sm q
spec _  EmptyS             = P.EmptyS
spec sm (E e)              = P.E $ modelExpr e sm

-- * Helpers

-- | Renders the shortname of a reference/domain.
renderShortName :: PrintingInformation -> IRefProg -> ShortName -> Sentence
renderShortName pinfo (Deferred u) _ = S $ fromMaybe (error "Domain has no abbreviation.") $ shortForm $ termResolve' (pinfo ^. syst . systemdb) u
renderShortName pinfo (RConcat a b) sn = renderShortName pinfo a sn :+: renderShortName pinfo b sn
renderShortName _ (RS s) _ = S s
renderShortName _ Name sn = getSentSN sn

-- | Render a uniform resource locator as a 'Sentence'.
renderURI :: ctx -> ShortName -> Sentence
renderURI _ = getSentSN

-- | Renders citation information.
renderCitInfo :: RefInfo -> Sentence
renderCitInfo  None          = EmptyS
renderCitInfo (RefNote   rn) = sParen (S rn)
renderCitInfo (Equation [x]) = sParen (S "Eq." +:+ S (show x))
renderCitInfo (Equation  i ) = sParen (S "Eqs." +:+ foldNums "-" i)
renderCitInfo (Page     [x]) = sParen (S "pg." +:+ S (show x))
renderCitInfo (Page      i ) = sParen (S "pp." +:+ foldNums "-" i)
