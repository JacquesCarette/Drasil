{-# Language FlexibleInstances #-}
-- | Printing Monad. Starts with a specific data type (reader monad) and extends from there.
module Language.Drasil.TeX.Monad where

import Prelude hiding (print)
import qualified Text.PrettyPrint as TP

import Language.Drasil

import qualified Language.Drasil.Printing.Helpers as H

-----------------------------------------------------------------------------
-- * Printing Monad

-- first, start with a specific data type
-- note that this is just the Reader Monad for now, but we might need
-- to extend, so start there.

-- | There are two proper contexts, Text and Math; Curr is the current context.
-- There are multiple ways of getting there: for Text, either being at the top-level 
-- or inside \text. For Math, either surrounded by $ or 
-- in \begin{equation} .. \end{equation}.
-- Curr is when the current context is fine.
data MathContext = Text | Math | Curr deriving Eq

-- | A monad for printing in LaTeX.
newtype PrintLaTeX a = PL { runPrint :: MathContext -> a }

-- | Defines the printing monad as a functor.
instance Functor PrintLaTeX where
  fmap f (PL ca) = PL $ \ctx -> f (ca ctx)

-- | This printing monad is also applicative.
instance Applicative PrintLaTeX where
  pure = PL . const
  PL f <*> PL v = PL $ \ctx -> f ctx (v ctx)

-- | Define the printing monad.
instance Monad PrintLaTeX where
  return = pure
  m >>= k = PL $ \ctx -> 
    let a = runPrint m ctx in
    runPrint (k a) ctx

-- | Convenient abbreviation.
type D = PrintLaTeX TP.Doc

-- | MonadReader calls this @local@.
-- Can switch contexts (including no-switch cases).  Adjust printing as necessary.
switch :: (MathContext -> MathContext) -> D -> D
switch f (PL g) = PL $ \c -> adjust c (f c) g
  where
    bstext = TP.text "\\text"
    br doc = TP.text "{" TP.<> doc TP.<> TP.text "}"
    adjust :: MathContext -> MathContext -> (MathContext -> TP.Doc) -> TP.Doc
    adjust Math Math gen = gen Math
    adjust Text Text gen = gen Text
    -- we are producing Math, but want some Text embedded
    adjust Math Text gen = bstext TP.<> br (gen Text)
    -- we are producing Text, but want some Math embedded
    adjust Text Math gen = H.dollarDoc $ gen Math
    adjust Curr Curr gen = gen Text -- default
    adjust Curr x gen = gen x
    adjust x Curr gen = gen x 

toMath, toText :: D -> D
-- | Change context to Math.
toMath = switch (const Math)
-- | Change context to Text.
toText = switch (const Text)

-- | MonadReader calls this @ask@.
getCtx :: PrintLaTeX MathContext
getCtx = PL id

-- | D is a member of Semigroup.
instance Semigroup (PrintLaTeX TP.Doc) where
  (PL s1) <> (PL s2) = PL $ \ctx -> s1 ctx TP.<> s2 ctx

-- | D is a monoid.
instance Monoid (PrintLaTeX TP.Doc) where
  mempty = pure TP.empty

-- may revisit later
-- | Since Text.PrettyPrint steals <>, use %% instead for $$.
infixl 5 %%
(%%) :: D -> D -> D
(%%) = liftA2 (TP.$$)

-- | Lifts Text.PrettyPrint's $+$. Above, with no overlapping. Associative.
infixr 6 $+$
($+$) :: D -> D -> D
($+$) = liftA2 (TP.$+$)

-- | Concatenates a list of 'D' using a function from ['TP.Doc'] -> 'TP.Doc'.
tpRunPrint :: ([TP.Doc] -> TP.Doc) -> [D] -> D
tpRunPrint f l = PL $ \ctx -> f $ map (`runPrint` ctx) l

-- | List version of 'TP.$$'. Above, except that if the last line of the first
-- argument stops at least one position before the first line of the second begins,
-- these two lines are overlapped.
vcat :: [D] -> D
vcat = tpRunPrint TP.vcat

-- Combine 'TP.vcat' and 'TP.punctuate'.
vpunctuate :: TP.Doc -> [D] -> D
vpunctuate x = tpRunPrint (TP.vcat . TP.punctuate x)

-- Combine 'TP.hcat' and 'TP.punctuate'.
hpunctuate :: TP.Doc -> [D] -> D
hpunctuate x = tpRunPrint (TP.hcat . TP.punctuate x)

-- | Nest a 'D' by a specified indentation level.
nest :: Int -> D -> D
nest i (PL f) = PL $ \ctx -> TP.nest i (f ctx)
--------
-- | MathContext operations.
lub :: MathContext -> MathContext -> MathContext
lub Math Math = Math
lub Text Text = Text
lub Curr Curr = Curr
lub Curr x    = x
lub x    Curr = x
lub _    _    = Text -- Text is top-most

-----------------
-- Hacked up version, will get deleted
-- | Latex type. Holds 'String's.
newtype Latex = L { unPL :: String }

-- | Renders special characters.
instance RenderSpecial Latex where
  special Circle       = L "{}^{\\circ}"
  -- special Partial      = L "\\partial{}"
