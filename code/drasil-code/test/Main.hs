{-# LANGUAGE PatternSynonyms, QuasiQuotes, TupleSections, RankNTypes #-}

-- | Main module to gather all the GOOL tests and generate them.
module Main (main) where

import Control.Monad.State (evalState, runState)
import Control.Lens ((^.))
import qualified Data.Map as M
import System.OsPath (osp)
import Prelude hiding (return,print,log,exp,sin,cos,tan)

import Drasil.FileHandling (FileLayout, file, directory, ps, goldenTestingGroup, goldenTest)
import Drasil.GOOL (OOProg, unJC, unPC, unCSC, unCPPC, unSC,
  initialState, ProgData(..), headers, sources, mainMod,
  FileData(..), modDoc)
import qualified Drasil.GOOL as OO (unCI, ProgramSym(..), GSProgram)
import Drasil.GProc (ProcProg, unJLC)
import qualified Drasil.GProc as Proc (unCI, ProgramSym(..), GSProgram)
import Language.Drasil.Code (ImplementationType(..), makeSds)
import Language.Drasil.GOOL (SoftwareDossierSym(..), package,
  PackageData(..), pattern PackageData,
  unPP, unJP, unCSP, unCPPP, unSP, unJLP)

import HelloWorld (helloWorldOO, helloWorldProc)
import GOOL.PatternTest (patternTest)
import FileTests (fileTestsOO, fileTestsProc)
import VectorTest (vectorTestOO, vectorTestProc)
import NameGenTest (nameGenTestOO, nameGenTestProc)
import Test.Tasty (TestTree, defaultMain, testGroup)

-- | Renders five GOOL tests (FileTests, HelloWorld, PatternTest, VectorTest, and NameGenTest)
-- in Java, Python, C#, C++, Swift, and Julia.
main :: IO()
main = main'

main' :: IO ()
main' = defaultMain codeGenTestGroup

codeGenTestGroup :: TestTree
codeGenTestGroup =
  testGroup
    "Codegen Test"
    [ testGroup
        "GOOL"
        [ goolTestGroup "HelloWorldOO" helloWorldOO,
          goolTestGroup "PatternTestOO" patternTest,
          goolTestGroup "FileTestsOO" fileTestsOO,
          goolTestGroup "VectorTestOO" vectorTestOO,
          goolTestGroup "NameGenTestOO" nameGenTestOO
        ],
      testGroup
        "GProc"
        [ gProcTestGroup "HelloWorldProc" helloWorldProc,
          gProcTestGroup "FileTestsProc" fileTestsProc,
          gProcTestGroup "VectorTestProc" vectorTestProc,
          gProcTestGroup "NameGenTestProc" nameGenTestProc
        ]
    ]

goolTestGroup :: String -> (forall r. (OOProg r) => OO.GSProgram r) -> TestTree
goolTestGroup n p =
  goldenTestingGroup
    [osp|test/build2/{n}|]
    [osp|test/golden2/{n}|]
    n
    [ goldenTest "java" $ directory [ps|java|] $ genCode [classes unJC unJP p],
      goldenTest "python" $ directory [ps|python|] $ genCode [classes unPC unPP p],
      goldenTest "csharp" $ directory [ps|csharp|] $ genCode [classes unCSC unCSP p],
      goldenTest "cpp" $ directory [ps|cpp|] $ genCode [classes unCPPC unCPPP p],
      goldenTest "swift" $ directory [ps|swift|] $ genCode [classes unSC unSP p]
    ]

gProcTestGroup :: String -> (forall r. (ProcProg r) => Proc.GSProgram r) -> TestTree
gProcTestGroup n p =
  goldenTestingGroup
    [osp|test/build2/{n}|]
    [osp|test/golden2/{n}|]
    n
    [ goldenTest "julia" $ directory [ps|julia|] $ genCode [jlClasses unJLC unJLP p]
    ]

-- | Gathers all information needed to generate code, sorts it, and calls the renderers.
genCode :: [PackageData] -> [FileLayout]
genCode = map genCode'
  where
    genCode' (PackageData prog aux) =
      let
        label = progName prog
        layout = toFileLayout (progMods prog) ++ aux
      in directory [ps|{label}|] layout

classes :: (OOProg r, SoftwareDossierSym r', Monad r') => (r (OO.Program r) -> ProgData) ->
  (r' PackageData -> PackageData) -> (forall s. (OOProg s) => OO.GSProgram s) -> PackageData
classes unRepr unRepr' p =
  let
    gs = OO.unCI (evalState p initialState)
    (p', gs') = runState p gs
    pd = unRepr p'
    fileInfoState = makeSds (gs' ^. headers) (gs' ^. sources) (gs' ^. mainMod)
  in unRepr' $ package pd [makefile [] Program [] fileInfoState pd]

-- Classes that Julia is currently able to render
jlClasses :: (ProcProg r, SoftwareDossierSym r', Monad r') => (r (Proc.Program r) -> ProgData) ->
  (r' PackageData -> PackageData) -> (forall s. (ProcProg s) => Proc.GSProgram s) -> PackageData
jlClasses unRepr unRepr' p =
  let
    gs = Proc.unCI (evalState p initialState)
    (p', gs') = runState p gs
    pd = unRepr p'
    fileInfoState = makeSds (gs' ^. headers) (gs' ^. sources) (gs' ^. mainMod)
  in unRepr' $ package pd [makefile [] Program [] fileInfoState pd]

-- | Internal: Converts a list of `FileData` to a `FileLayout`.
toFileLayout :: [FileData] -> [FileLayout]
toFileLayout fc =
  let
    root = foldl (\m f -> insertFile (filePath f, modDoc $ fileMod f) m) M.empty fc

    entryToLayout (n, File d) = file [ps|{n}|] d
    entryToLayout (n, Folder m) = directory [ps|{n}|] $ map entryToLayout $ M.assocs m
  in
    map entryToLayout (M.assocs root)

data Entry a = File a | Folder (M.Map String (Entry a))
  deriving (Show)

insertFile :: (String, a) -> M.Map String (Entry a) -> M.Map String (Entry a)
insertFile (p, d) m =
  if '/' `elem` p
    then
      let (fname, rest) = break (== '/') p
          folderM = case M.findWithDefault (Folder M.empty) fname m of
                      File _   -> dupError fname
                      Folder f -> f
      in M.insert fname (Folder $ insertFile (drop 1 rest, d) folderM) m
    else M.insertWith (\_ -> dupError p) p (File d) m
  where
    dupError fname = error $ "A file or folder with name '" ++ fname ++ "' already exists."
