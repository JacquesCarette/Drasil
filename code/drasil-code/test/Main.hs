{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Main module to gather all the GOOL tests and generate them.
module Main (main) where

import Control.Monad.State (evalState, runState)
import Control.Lens ((^.))
import System.OsPath (osp)
import Prelude hiding (return,print,log,exp,sin,cos,tan)

import Drasil.FileHandling (FileLayout, directory, ps, ps, (</>))
import Drasil.GOOL (OOProg, Literal, Comparison, GetSet, StrategyPattern,
  ObserverPattern, DeclStatement, ControlStatement, unJC, unPC, unCSC, unCPPC,
  unSC, initialState, ProgData(..), headers, sources, mainMod, GOOLState)
import qualified Drasil.GOOL as OO (unCI, GSProgram)
import Drasil.GProc (ProcProg, NativeVector, unJLC, unMLC)
import qualified Drasil.GProc as Proc (GSProgram)
import Drasil.TestingKit (testMain)
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
import Test.Tasty (TestTree, testGroup)

-- | Renders five GOOL tests (FileTests, HelloWorld, OOVector, PatternTest, and NameGenTest)
-- in Java, Python, C#, C++, Swift, and Julia.
main :: IO ()
main = testMain codeGenTestGroup

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
  -> ( forall r prg file mod cls stvr mthd attch vis param bod block stmt var scope val binder typ.
       ( OOProg r prg file mod cls stvr mthd attch vis param bod block stmt var scope val binder typ
       , GetSet r var val
       , StrategyPattern r bod block var val
       , ObserverPattern r stmt typ
       ) => OO.GSProgram r prg
     )
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
  ->
    ( forall r prg file mod mthd vis param bod block stmt var scope val binder typ.
      -- TODO [Brandon Bosman, 09/21/2026]: Add Literal, Comparision, DeclStatement, ControlStatement to ProcProg
      ( Literal r val typ
      , Comparison r val
      , DeclStatement r bod stmt var scope val
      , ControlStatement r bod stmt var val
      , ProcProg r prg file mod mthd vis param bod block stmt var scope val binder typ
      ) => Proc.GSProgram r prg)
  -> TestTree
gProcTestGroup n p =
  goldenTestingGroup
    ([osp|test/build|] </> [ps|{n}|])
    ([osp|test/golden|] </> [ps|{n}|])
    n
    [ goldenTest "julia" $ directory [ps|julia|] $ genCodeProc unJLC unJLP p,
      goldenTest "matlab" $ directory [ps|matlab|] $ genCodeProc unMLC unMLP p
    ]

gProcVectorTestGroup
  :: String
  ->
    ( forall r prg file mod mthd vis param bod block stmt var scope val binder typ.
      ( Comparison r val
      , NativeVector r val typ
      , DeclStatement r bod stmt var scope val
      , ControlStatement r bod stmt var val
      , ProcProg r prg file mod mthd vis param bod block stmt var scope val binder typ
      ) => Proc.GSProgram r prg
    )
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
  ::
    ( NativeVector r val typ
    , ProcProg r ProgData file mod mthd vis param bod block stmt var scope val binder typ
    , Monad r'
    )
  => (r ProgData -> ProgData)
  -> (r' PackageData -> PackageData)
  ->
    ( forall s prg' file' mod' mthd' vis' param' bod' block' stmt' var' scope' val' binder' typ'.
      ( Comparison s val'
      , NativeVector s val' typ'
      , DeclStatement s bod' stmt' var' scope' val'
      , ProcProg s prg' file' mod' mthd' vis' param' bod' block' stmt' var' scope' val' binder' typ'
      ) => Proc.GSProgram s prg'
    )
  -> [FileLayout]
genCodeProcNoMake unRepr unRepr' p =
  let
    (p', gs') = runState p initialState
    (PackageData prog aux) = unRepr' $ package (unRepr p') []
  in seq gs' $ toFileLayout (progMods prog) <> aux

genCodeGOOL
  ::
    ( OOProg r ProgData file mod cls stvr mthd attch vis param bod block stmt var scope val binder typ
    , GetSet r var val
    , StrategyPattern r bod block var val
    , ObserverPattern r stmt typ
    , SoftwareDossierSym r'
    , Monad r'
    )
  => (r ProgData -> ProgData)
  -> (r' PackageData -> PackageData)
  -> ( forall s prg' file' mod' cls' stvr' mthd' attch' vis' param' bod' block' stmt' var' scope' val' binder' typ'.
       ( OOProg s prg' file' mod' cls' stvr' mthd' attch' vis' param' bod' block' stmt' var' scope' val' binder' typ'
       , GetSet s var' val'
       , StrategyPattern s bod' block' var' val'
       , ObserverPattern s stmt' typ'
       ) => OO.GSProgram s prg'
     )
  -> [FileLayout]
genCodeGOOL unRepr unRepr' p =
  let
    gs = OO.unCI (evalState p initialState)
    (p', gs') = runState p gs
  in genCode' (unRepr p') gs' unRepr'

genCodeProc
  ::
    ( ProcProg r ProgData file mod mthd vis param bod block stmt var scope val binder typ
    , SoftwareDossierSym r'
    , Monad r'
    )
  => (r ProgData -> ProgData)
  -> (r' PackageData -> PackageData)
  -> ( forall s prg' file' mod' mthd' vis' param' bod' block' stmt' scope' typ' val' var' binder'.
       ( ProcProg s prg' file' mod' mthd' vis' param' bod' block' stmt' scope' typ' val' var' binder'
       ) => Proc.GSProgram s prg'
     )
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
  in toFileLayout (progMods prog) <> aux
