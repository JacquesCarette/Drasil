module Utils.Drasil (
  -- Sentence
  andIts, andThe, isExpctdToHv, isThe, ofGiv, ofGiv', ofThe, ofThe', sOf, sOr,
  sVersus, sAnd, sAre, sIn, sIs, toThe,
  -- Fold
  foldle, foldle1, foldConstraints -- FIXME: Shouldn't be exported when drasil-utils is finished
) where

import Utils.Drasil.Fold (foldle, foldle1, foldConstraints)
import Utils.Drasil.Sentence (andIts, andThe, isExpctdToHv, isThe, ofGiv,
  ofGiv', ofThe, ofThe', sOf, sOr, sVersus, sAnd, sAre, sIn, sIs, toThe)
