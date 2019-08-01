module Utils.Drasil.Contents (enumBullet, enumBulletU, enumSimple,
  enumSimpleU, eqUnR, eqUnR', mkEnumSimpleD) where

import Language.Drasil
import Utils.Drasil.Misc (bulletFlat, mkEnumAbbrevList)

import Control.Lens ((^.))

eqUnR :: Expr -> Reference -> LabelledContent
eqUnR e lbl = llcc lbl $ EqnBlock e

eqUnR' :: Expr -> Contents
eqUnR' e = UlC $ ulcc $ EqnBlock e

-- | enumBullet apply Enumeration, Bullet and Flat to a list
enumBullet :: Reference -> [Sentence] -> LabelledContent --FIXME: should Enumeration be labelled?
enumBullet lb s = llcc lb $ Enumeration $ bulletFlat s

enumBulletU :: [Sentence] -> Contents --FIXME: should Enumeration be labelled?
enumBulletU s =  UlC $ ulcc $ Enumeration $ bulletFlat s

-- | enumSimple enumerates a list and applies simple and enumeration to it
-- s - start index for the enumeration
-- t - title of the list
-- l - list to be enumerated
enumSimple :: Reference -> Integer -> Sentence -> [Sentence] -> LabelledContent --FIXME: should Enumeration be labelled?
enumSimple lb s t l = llcc lb $ Enumeration $ Simple $ noRefsLT $ mkEnumAbbrevList s t l

enumSimpleU :: Integer -> Sentence -> [Sentence] -> Contents --FIXME: should Enumeration be labelled?
enumSimpleU s t l = UlC $ ulcc $ Enumeration $ Simple $ noRefsLT $ mkEnumAbbrevList s t l

-- | noRefsLT converts lists of tuples containing a title and ItemType into
-- a ListTuple which can be used with Contents but not directly referable.
noRefsLT :: [(Sentence, ItemType)] -> [ListTuple]
noRefsLT a = uncurry zip3 (unzip a) $ repeat Nothing

-- | mkEnumSimpleD is a convenience function for transforming types which are
-- instances of the constraints Referable, HasShortName, and Definition, into
-- Simple-type Enumerations.
mkEnumSimpleD :: (Referable c, HasShortName c, Definition c) => [c] -> [Contents]
mkEnumSimpleD = mkEnumSimple $ mkListTuple (\x -> Flat $ x ^. defn)

-- | mkEnumSimple is a convenience function for converting lists into
-- Simple-type Enumerations.
mkEnumSimple :: (a -> ListTuple) -> [a] -> [Contents]
mkEnumSimple f = replicate 1 . UlC . ulcc . Enumeration . Simple . map f

-- | Creates a list tuple filling in the title with a ShortName and filling
-- reference information.
mkListTuple :: (Referable c, HasShortName c) => (c -> ItemType) -> c -> ListTuple
mkListTuple f x = (S . getStringSN $ shortname x, f x, Just $ refAdd x)
