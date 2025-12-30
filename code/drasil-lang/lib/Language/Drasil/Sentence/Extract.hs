-- | Extract various kinds of UIDs from a Sentence. Used in conjunction with the
-- chunk database in order to render terms, symbols, and references properly.
-- The actual traversals now live in 'Language.Drasil.Sentence', but this
-- module keeps the old import path working.
module Language.Drasil.Sentence.Extract (sdep, shortdep, lnames, lnames') where

import Language.Drasil.Sentence (sdep, shortdep, lnames, lnames')
