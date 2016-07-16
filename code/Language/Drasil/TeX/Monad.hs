{-# Language FlexibleInstances #-}
module Language.Drasil.TeX.Monad where

import Prelude hiding (print)
import Text.PrettyPrint (($$))
import qualified Text.PrettyPrint as TP

import Control.Applicative hiding (empty)
import Data.Monoid hiding ((<>))

-----------------------------------------------------------------------------
-- Printing monad
--

-- first, start with a specific data type
-- note that this is just the Reader Monad for now, but we might need
-- to extend, so start there.
data MathContext = Text | Math -- should not be exported

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
