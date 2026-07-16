{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

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
import qualified Drasil.GOOL as OO (unCI, GSProgram)
import Drasil.GProc (ProcProg, NativeVector, unJLC, unMLC)
import qualified Drasil.GProc as Proc (GSProgram)
import Drasil.TestingKit.Golden (goldenTestingGroup, goldenTest)
import Language.Drasil.Code (ImplementationType(..), makeSds, toFileLayout)
import Language.Drasil.GOOL (SoftwareDossierSym(..), package,
  PackageData(..), pattern PackageData,
  unPP, unJP, unCSP, unCPPP, unSP, unJLP, unMLP)

import HelloWorld (helloWorldOO, helloWorldProc)
import GOOL.PatternTest (patternTest)
import FileTests (fileTestsOO, fileTestsProc)
import OOVector (ooVector)
import NameGenTest (nameGenTestOO, nameGenTestProc)
import VectorTest (vectorTestProc)
import Test.Tasty (TestTree, defaultMain, testGroup)

-- | Renders five GOOL tests (FileTests, HelloWorld, OOVector, PatternTest, and NameGenTest)
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
          goolTestGroup "NameGenTestOO" nameGenTestOO,
          goolTestGroup "OOVector" ooVector
        ],
      testGroup
        "GProc"
        [ gProcTestGroup "HelloWorldProc" helloWorldProc,
          gProcTestGroup "FileTestsProc" fileTestsProc,
          gProcTestGroup "NameGenTestProc" nameGenTestProc,
          gProcVectorTestGroup "VectorTestProc" vectorTestProc
        ]
    ]

goolTestGroup
  :: String
  -> (forall r vis smt md svr att prg. (OOProg r vis smt md svr att prg) => OO.GSProgram r prg)
  -> TestTree
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

gProcTestGroup
  :: String
  -> (forall r vis smt md prg. (ProcProg r vis smt md prg) => Proc.GSProgram r prg)
  -> TestTree
gProcTestGroup n p =
  goldenTestingGroup
    ([osp|test/build|] </> [ps|{n}|])
    ([osp|test/golden|] </> [ps|{n}|])
    n
    [ goldenTest "julia" $ directory [ps|julia|] $ genCodeProc unJLC unJLP p
    ]

gProcVectorTestGroup
  :: String
  -> (forall r vis smt md prg. (ProcProg r vis smt md prg, NativeVector r) => Proc.GSProgram r prg)
  -> TestTree
gProcVectorTestGroup n p =
  goldenTestingGroup
    ([osp|test/build|] </> [ps|{n}|])
    ([osp|test/golden|] </> [ps|{n}|])
    n
    [ goldenTest "julia" $ directory [ps|julia|] $ genCodeProcNoMake unJLC unJLP p,
      goldenTest "matlab" $ directory [ps|matlab|] $ genCodeProcNoMake unMLC unMLP p
    ]

genCodeProcNoMake
  :: (ProcProg r vis smt md ProgData, NativeVector r, Monad r')
  => (r ProgData -> ProgData)
  -> (r' PackageData -> PackageData)
  -> (forall s vis' smt' md' prg'. (ProcProg s vis' smt' md' prg', NativeVector s) => Proc.GSProgram s prg')
  -> [FileLayout]
genCodeProcNoMake unRepr unRepr' p =
  let
    (p', gs') = runState p initialState
    (PackageData prog aux) = unRepr' $ package (unRepr p') []
  in seq gs' $ toFileLayout (progMods prog) ++ aux

genCodeGOOL
  :: (OOProg r vis smt md svr att ProgData, SoftwareDossierSym r', Monad r')
  => (r ProgData -> ProgData)
  -> (r' PackageData -> PackageData)
  -> (forall s vis' smt' md' svr' att' prg'. (OOProg s vis' smt' md' svr' att' prg') => OO.GSProgram s prg')
  -> [FileLayout]
genCodeGOOL unRepr unRepr' p =
  let
    gs = OO.unCI (evalState p initialState)
    (p', gs') = runState p gs
  in genCode' (unRepr p') gs' unRepr'

genCodeProc
  :: (ProcProg r vis smt md ProgData, SoftwareDossierSym r', Monad r')
  => (r ProgData -> ProgData)
  -> (r' PackageData -> PackageData)
  -> (forall s vis' smt' md' prg'. (ProcProg s vis' smt' md' prg') => Proc.GSProgram s prg')
  -> [FileLayout]
genCodeProc unRepr unRepr' p =
  let
    (p', gs') = runState p initialState
  in genCode' (unRepr p') gs' unRepr'

genCode' :: (SoftwareDossierSym r', Monad r') => ProgData -> GOOLState ->
  (r' PackageData -> PackageData) -> [FileLayout]
genCode' pd gs' unRepr' =
  let
    fileInfoState = makeSds (gs' ^. headers) (gs' ^. sources) (gs' ^. mainMod)
    (PackageData prog aux) = unRepr' $ package pd [makefile [] Program [] fileInfoState pd]
  in toFileLayout (progMods prog) ++ aux
