module Utils.Drasil.Contents (enumBullet, enumBulletU, enumConInst, enumSimple,
  enumSimpleU, eqUnR, eqUnR') where

import Language.Drasil
import Utils.Drasil.Misc (bulletFlat, conA, mkEnumAbbrevList)

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

-- | Creates an enumerated list of ConceptInstances
enumConInst :: [ConceptInstance] -> [Contents]
enumConInst x = [UlC . ulcc . Enumeration . Simple $ map lt $ zip x ([1..] :: [Int])] 
  where
    lt (c, n) = (start c n, Flat $ c ^. defn, Just $ refAdd c)
    start c n = S (getStringSN $ shortname c) +:+ sParen (S $ conA c ++ show n)
