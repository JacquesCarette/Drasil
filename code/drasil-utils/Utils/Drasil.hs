module Utils.Drasil (
  -- Sentence
  andIts, andThe, isExpctdToHv, isThe, ofGiv, ofGiv', ofThe, ofThe', sOf, sOr,
  sVersus, sAnd, sAre, sIn, sIs, toThe,
  -- Fold
  EnumType(..), WrapType(..), SepType(..), FoldType(..), foldConstraints, -- FIXME: foldConstraints shouldn't be exported when drasil-utils is finished
  foldlEnumList, foldlList, foldlSP, foldlSP_, foldlSPCol, foldlSent,
  foldlSent_,foldlSentCol, foldlsC
) where

import Utils.Drasil.Fold (EnumType(..), WrapType(..), SepType(..),
  FoldType(..), foldConstraints, foldlEnumList, foldlList, foldlSP,
  foldlSP_, foldlSPCol, foldlSent, foldlSent_, foldlSentCol, foldlsC)
import Utils.Drasil.Sentence (andIts, andThe, isExpctdToHv, isThe, ofGiv,
  ofGiv', ofThe, ofThe', sOf, sOr, sVersus, sAnd, sAre, sIn, sIs, toThe)
