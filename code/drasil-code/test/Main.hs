{-# LANGUAGE PatternSynonyms, QuasiQuotes, RankNTypes #-}

-- | Main module to gather all the GOOL tests and generate them.
module Main (main) where

import Control.Monad.State (evalState, runState)
import Control.Lens ((^.))
import System.OsPath (osp)
import Prelude hiding (return,print,log,exp,sin,cos,tan)

import Drasil.FileHandling (FileLayout, directory, ps, ps, (</>))
import Drasil.GOOL (OOProg, unJC, unPC, unCSC, unCPPC, unSC,
  initialState, ProgData(..), headers, sources, mainMod,
  GOOLState)
import qualified Drasil.GOOL as OO (unCI, ProgramSym(..), GSProgram)
import Drasil.GProc (ProcProg, unJLC, unMLC)
import qualified Drasil.GProc as Proc (unCI, ProgramSym(..), GSProgram)
import Drasil.TestingKit.Golden (goldenTestingGroup, goldenTest)
import Language.Drasil.Code (ImplementationType(..), makeSds, toFileLayout)
import Language.Drasil.GOOL (SoftwareDossierSym(..), package,
  PackageData(..), pattern PackageData,
  unPP, unJP, unCSP, unCPPP, unSP, unJLP, unMLP)

import HelloWorld (helloWorldOO, helloWorldProc)
import GOOL.PatternTest (patternTest)
import FileTests (fileTestsOO, fileTestsProc)
import NameGenTest (nameGenTestOO, nameGenTestProc)
import VectorTest (vectorTestProc)
import Test.Tasty (TestTree, defaultMain, testGroup)

-- | Renders four GOOL tests (FileTests, HelloWorld, PatternTest, and NameGenTest)
-- in Java, Python, C#, C++, Swift, and Julia.
main :: IO ()
main = defaultMain codeGenTestGroup

codeGenTestGroup :: TestTree
codeGenTestGroup =
  testGroup
    "Codegen Test"
    [ testGroup
        "GOOL"
        [ goolTestGroup "HelloWorldOO" helloWorldOO,
          goolTestGroup "PatternTestOO" patternTest,
          goolTestGroup "FileTestsOO" fileTestsOO,
          goolTestGroup "NameGenTestOO" nameGenTestOO
        ],
      testGroup
        "GProc"
        [ gProcTestGroup "HelloWorldProc" helloWorldProc,
          gProcTestGroup "FileTestsProc" fileTestsProc,
          gProcTestGroup "NameGenTestProc" nameGenTestProc,
          gProcMatlabTestGroup "VectorTestProc" vectorTestProc
        ]
    ]

goolTestGroup :: String -> (forall r. (OOProg r) => OO.GSProgram r) -> TestTree
goolTestGroup n p =
  goldenTestingGroup
    ([osp|test/build|] </> [ps|{n}|])
    ([osp|test/golden|] </> [ps|{n}|])
    n
    [ goldenTest "java" $ directory [ps|java|] $ genCodeGOOL unJC unJP p,
      goldenTest "python" $ directory [ps|python|] $ genCodeGOOL unPC unPP p,
      goldenTest "csharp" $ directory [ps|csharp|] $ genCodeGOOL unCSC unCSP p,
      goldenTest "cpp" $ directory [ps|cpp|] $ genCodeGOOL unCPPC unCPPP p,
      goldenTest "swift" $ directory [ps|swift|] $ genCodeGOOL unSC unSP p
    ]

gProcTestGroup :: String -> (forall r. (ProcProg r) => Proc.GSProgram r) -> TestTree
gProcTestGroup n p =
  goldenTestingGroup
    ([osp|test/build|] </> [ps|{n}|])
    ([osp|test/golden|] </> [ps|{n}|])
    n
    [ goldenTest "julia" $ directory [ps|julia|] $ genCodeProc unJLC unJLP p
    ]

gProcMatlabTestGroup :: String -> (forall r. (ProcProg r) => Proc.GSProgram r) -> TestTree
gProcMatlabTestGroup n p =
  goldenTestingGroup
    ([osp|test/build|] </> [ps|{n}|])
    ([osp|test/golden|] </> [ps|{n}|])
    n
    [ goldenTest "matlab" $ directory [ps|matlab|] $ genCodeProcNoMake unMLC unMLP p
    ]

genCodeProcNoMake :: (ProcProg r, Monad r') =>
  (r (Proc.Program r) -> ProgData) -> (r' PackageData -> PackageData) ->
  (forall s. (ProcProg s) => Proc.GSProgram s) -> [FileLayout]
genCodeProcNoMake unRepr unRepr' p =
  let
    gs = Proc.unCI (evalState p initialState)
    (p', gs') = runState p gs
    (PackageData prog aux) = unRepr' $ package (unRepr p') []
  in seq gs' $ toFileLayout (progMods prog) ++ aux

genCodeGOOL :: (OOProg r, SoftwareDossierSym r', Monad r') => (r (OO.Program r) -> ProgData) ->
  (r' PackageData -> PackageData) -> (forall s. (OOProg s) => OO.GSProgram s) -> [FileLayout]
genCodeGOOL unRepr unRepr' p =
  let
    gs = OO.unCI (evalState p initialState)
    (p', gs') = runState p gs
  in genCode' (unRepr p') gs' unRepr'

genCodeProc :: (ProcProg r, SoftwareDossierSym r', Monad r') => (r (Proc.Program r) -> ProgData) ->
  (r' PackageData -> PackageData) -> (forall s. (ProcProg s) => Proc.GSProgram s) -> [FileLayout]
genCodeProc unRepr unRepr' p =
  let
    gs = Proc.unCI (evalState p initialState)
    (p', gs') = runState p gs
  in genCode' (unRepr p') gs' unRepr'

genCode' :: (SoftwareDossierSym r', Monad r') => ProgData -> GOOLState ->
  (r' PackageData -> PackageData) -> [FileLayout]
genCode' pd gs' unRepr' =
  let
    fileInfoState = makeSds (gs' ^. headers) (gs' ^. sources) (gs' ^. mainMod)
    (PackageData prog aux) = unRepr' $ package pd [makefile [] Program [] fileInfoState pd]
  in toFileLayout (progMods prog) ++ aux
