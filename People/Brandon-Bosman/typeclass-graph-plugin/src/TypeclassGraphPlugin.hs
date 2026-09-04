module TypeclassGraphPlugin (plugin) where

import Prelude hiding ((<>))

import GHC.Plugins
import GHC.Tc.Types (TcGblEnv, TcM, tcg_type_env)
import GHC.Types.TypeEnv (typeEnvClasses)
import GHC.Core.Class (Class, className, classSCTheta, classMethods)
import GHC.Core.Predicate (classMethodTy)
import GHC.Core.TyCo.Rep

plugin :: Plugin
plugin = defaultPlugin
  { typeCheckResultAction = inspectClasses
  }

inspectClasses
  :: [CommandLineOption]
  -> ModSummary
  -> TcGblEnv
  -> TcM TcGblEnv
inspectClasses _ _ gblEnv = do
  let classes = typeEnvClasses $ tcg_type_env gblEnv
      classGraph = map (\cls -> (cls, superClasses cls)) classes
      baseClasses = filter (classDependsOnTyConName "TypeData") classes
      output = (unlines ((map printNode classGraph) ++ (map printBaseNode baseClasses)))
  liftIO $ appendFile ("./facts.pl") output
  pure gblEnv

printNode :: (Class, [TyCon]) -> String
printNode (cls, sups) =
  let cName = nameToString (className cls)
  in unlines (map (\sup -> "parent(" ++ cName ++ ", " ++ nameToString (tyConName sup) ++ ").") sups)

printBaseNode :: Class -> String
printBaseNode cls = "base(" ++ nameToString (className cls) ++ ")."

-- Given a list of classes we're interested in and a particular class, find all classes in that list that it relies on
superClasses :: Class -> [TyCon]
superClasses cls =
  map tyConAppTyCon (classSCTheta cls)

-- Given a Type Constructor and a Type, determine whether that Type references the Type Constructor
containsTyConName :: String -> Type -> Bool
containsTyConName name = go
  where
    go :: Type -> Bool
    go (TyVarTy _) = False
    go (AppTy fun arg) = go fun || go arg
    go (TyConApp tc args) = occNameString (nameOccName (tyConName tc)) == name || any go args
    go (ForAllTy _ body) = go body
    go (FunTy _ _ arg result) = go arg || go result
    go (LitTy _) = False
    go (CastTy ty _) = go ty
    go (CoercionTy _) = False

classDependsOnTyConName :: String -> Class -> Bool
classDependsOnTyConName target cls =
  any (containsTyConName target . classMethodTy) (classMethods cls)

nameToString :: Name -> String
nameToString nm = stripBadChars ((srcSpanFileSafe $ nameSrcSpan nm) ++ "." ++ (occNameString $ occName nm))

stripBadChars :: String -> String
stripBadChars = map (\ch -> if elem ch ['.', '/'] then '_' else ch)

srcSpanFileSafe :: SrcSpan -> String
srcSpanFileSafe spn = case srcSpanToRealSrcSpan spn of
  (Just s) -> unpackFS $ srcSpanFile s
  Nothing -> "<unknown file>"
