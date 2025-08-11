{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Language.Drasil.TypeCheck where

import qualified Data.Map.Strict as M

import Data.Typeable (Proxy(Proxy), typeRep)

import Language.Drasil
import Database.Drasil
import Data.Either (isRight, rights)
import Control.Lens ((^.))
import Data.Bifunctor (second)
import Data.List (partition)
import Drasil.System (System, HasSystem (instModels, dataDefns, systemdb))

typeCheckSI :: System -> IO ()
typeCheckSI sys = do
    let ims = sys ^. instModels
        dds = sys ^. dataDefns
        chks = sys ^. systemdb
    -- build a variable context (a map of UIDs to "Space"s [types])
    let cxt = M.fromList $ map (\x -> (x ^. uid, x ^. typ)) $ (findAll (typeRep (Proxy @DefinedQuantityDict)) chks :: [DefinedQuantityDict])

    -- dump out the list of variables
    putStr "Symbol Table: "
    print $ M.toList cxt

    putStrLn "=====[ Start type checking ]====="
    let
      exprSpaceTups :: (HasUID t, RequiresChecking t Expr Space) => [t] -> [(UID, [(Expr, Space)])] 
      exprSpaceTups = map (\t -> (t ^. uid, requiredChecks t))

    -- grab all type-check-able expressions (w.r.t. Space) from DDs and IMs
    let toChk = exprSpaceTups ims ++ exprSpaceTups dds

    -- split up theories by "ones that contain things to type check" vs "not",
    -- but in reverse
    let (notChkd, chkd) = partition (\(_, exsps) -> null exsps) toChk

    -- note that some theories didn't expose anything to type-check
    mapM_ 
      (\(t, _) -> putStrLn $ "WARNING: `" ++ show t ++ "` does not expose any expressions to type check.")
      notChkd

    -- type check them
    let chkdd = map (second (map (uncurry (check cxt)))) chkd

    -- format 'ok' messages and 'type error' messages, as applicable
    let formattedChkd :: [Either [Char] ([Char], [Either Space TypeError])]
        formattedChkd = map 
                          (\(t, tcs) -> if any isRight tcs
                            then Right ("`" ++ show t ++ "` exposes ill-typed expressions!", filter isRight tcs)
                            else Left $ "`" ++ show t ++ "` OK!") 
                          chkdd

    let errConsumer s = do 
          putStr "  - ERROR: "
          putStrLn $ temporaryIndent "  " s

    mapM_ (either
            putStrLn
            (\(tMsg, tcs) -> do
              putStrLn tMsg
              mapM_ errConsumer (rights tcs)
            )
      ) formattedChkd
    putStrLn "=====[ Finished type checking ]====="

    -- TODO: When we want to have Drasil panic on type-errors, use the following code:
    -- add back import: Control.Monad (when)
    -- when (any isRight formattedChkd) $ error "Type errors occurred, please check your expressions and adjust accordingly"
