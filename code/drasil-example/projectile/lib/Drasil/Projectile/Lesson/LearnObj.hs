module Drasil.Projectile.Lesson.LearnObj where

-- Drasil
import Data.Drasil.Concepts.Physics (motion)
import Data.Drasil.Concepts.Math (cartesian, equation)
import Drasil.Sentence.Combinators (bulletNested, bulletFlat)
import Language.Drasil

-- local
import Drasil.Projectile.Concepts (projMotion)

learnObjContext :: Contents
learnObjContext = UlC $ ulcc $ Enumeration $ bulletNested learnObjList $
  map bulletFlat [[], assumpResp, solveResp]

learnObjList :: [Sentence]
learnObjList = [S "Derive kinematic" +:+ plural equation +:+ S "for 2D" +:+ phrase projMotion
  +:+ S "from kinematic" +:+ plural equation +:+ S "from 1D rectilinear" +:+ phrase motion,
  S "Identify the assumptions required for the" +:+ phrase projMotion +:+
  plural equation +:+ S "to hold:",
  S "Solve any given (well-defined) free-flight" +:+ phrase projMotion +:+
  S "problems by:"]

assumpResp :: [Sentence]
assumpResp = [S "Air resistance is neglected",
  S "Gravitational acceleration acts downward and is constant, regardless of altitude"]

solveResp :: [Sentence]
solveResp = [S "Able to select an appropriate" +:+ titleize cartesian
  +:+ S "to simplify the problem as much as possible",
  S "Able to identify the known variables",
  S "Able to identify the unknown variables",
  S "Able to write" +:+ phrase projMotion +:+ plural equation +:+ S "for the given problem",
  S "Able to solve the" +:+ phrase projMotion +:+ plural equation +:+ S "for the unknown quantities"]
