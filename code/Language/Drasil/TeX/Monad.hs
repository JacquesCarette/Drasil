{-# Language FlexibleInstances #-}
module Language.Drasil.TeX.Monad where

import Prelude hiding (print)
import Text.PrettyPrint (($$))
import qualified Text.PrettyPrint as TP

import Control.Applicative hiding (empty)
import Data.Monoid hiding ((<>))

import qualified Language.Drasil.Printing.Helpers as H
import Language.Drasil.Unicode

-----------------------------------------------------------------------------
-- Printing monad
--

-- first, start with a specific data type
-- note that this is just the Reader Monad for now, but we might need
-- to extend, so start there.

-- there are just two contexts, test and math.  There are multiple ways
-- of getting there: for Text, either being at the top-level or inside \text.
-- for Math, either surrounded by $ or in \begin{equation} .. \end{equation}.
data MathContext = Text | Math deriving Eq

data PrintLaTeX a = PL { runPrint :: MathContext -> a }

instance Functor PrintLaTeX where
  fmap f (PL ca) = PL $ \ctx -> f (ca ctx)

instance Applicative PrintLaTeX where
  pure x = PL $ \_ -> x
  PL f <*> PL v = PL $ \ctx -> (f ctx) (v ctx)

instance Monad PrintLaTeX where
  return = pure
  m >>= k = PL $ \ctx -> 
    let a = runPrint m ctx in
    runPrint (k a) ctx

-- MonadReader calls this 'local'.
-- Switch contexts (including no-switch cases).  Adjust printing as necessary.
switch :: (MathContext -> MathContext) -> PrintLaTeX TP.Doc -> PrintLaTeX TP.Doc
switch f (PL g) = PL $ \c -> adjust c (f c) g
  where
    dollar = H.dlr
    bstext = TP.text "\\text"
    br     = \doc -> TP.text "{" TP.<> doc TP.<> TP.text "}"
    adjust :: MathContext -> MathContext -> (MathContext -> TP.Doc) -> TP.Doc
    adjust Math Math gen = gen Math
    adjust Text Text gen = gen Text
    -- we are producing Math, but want some Text embedded
    adjust Math Text gen = bstext TP.<> br (gen Text)
    -- we are producing Text, but want some Math embedded
    adjust Text Math gen = dollar TP.<> (gen Math) TP.<> dollar

toMath, toText :: D -> D
toMath = switch (const Math)
toText = switch (const Text)

-- MonadReader calls this 'ask'
get_ctx :: PrintLaTeX MathContext
get_ctx = PL id

-- convenient abbreviation
type D = PrintLaTeX TP.Doc

-- very convenient lifting of $$
instance Monoid (PrintLaTeX TP.Doc) where
  mempty = pure TP.empty
  (PL s1) `mappend` (PL s2) = PL $ \ctx -> (s1 ctx) $$ (s2 ctx)

-- since Text.PrettyPrint steals <>, use %% instead
-- may revisit later
(%%) :: D -> D -> D
(%%) = mappend

($+$),(<>) :: D -> D -> D
($+$) = liftA2 (TP.$+$)
(<>) = liftA2 (TP.<>)

vcat :: [D] -> D
vcat l = PL $ \ctx -> TP.vcat $ map (\x -> runPrint x ctx) l

-- hcat . punctuate
hpunctuate :: TP.Doc -> [D] -> D
hpunctuate x l = PL $ \ctx -> 
  TP.hcat $ TP.punctuate x $ map (\z -> runPrint z ctx) l
 
--------
-- MathContext operations
lub :: MathContext -> MathContext -> MathContext
lub Math Math = Math
lub Text Text = Text
lub _    _    = Text -- Text is top-most

-----------------
-- Hacked up version, will get deleted
data Latex = L { unPL :: String }

instance RenderGreek Latex where
  greek Alpha_L   = L "\\alpha{}"
  greek Alpha     = L "\\Alpha{}"
  greek Beta_L   = L "\\beta{}"
  greek Beta     = L "\\Beta{}"
  greek Delta_L   = L "\\delta{}"
  greek Delta     = L "\\Delta{}"
  greek Ell    = L "\\ell{}"
  greek Eta_L   = L "\\eta{}"
  greek Eta     = L "\\Eta{}"
  greek Gamma_L   = L "\\gamma{}"
  greek Gamma     = L "\\Gamma{}"
  greek Lambda_L   = L "\\lambda{}"
  greek Lambda     = L "\\Lambda{}"
  greek Nabla   = L "\\nabla{}"
  greek Nu_L   = L "\\nu{}"
  greek Nu     = L "\\Nu{}"
  greek Omega_L   = L "\\omega{}"
  greek Omega     = L "\\Omega{}"
  greek Phi_L   = L "\\phi{}"
  greek Phi_V   = L "\\varphi{}"
  greek Phi     = L "\\Phi{}"
  greek Rho_L   = L "\\rho{}"
  greek Rho     = L "\\Rho{}"
  greek Tau_L   = L "\\tau{}"
  greek Tau     = L "\\Tau{}"
  greek Upsilon_L   = L "\\upsilon{}"
  greek Upsilon     = L "\\Upsilon{}"

instance RenderSpecial Latex where
  special Circle   = L "{}^{\\circ}"
  special LEQ        = L "\\leq{}"
  special Partial   = L "\\partial{}"
  special UScore   = L "\\_"
