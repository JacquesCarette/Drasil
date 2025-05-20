module Drasil.Shared.LanguageRenderer.DocHelpers (
  doxFunc,
  doxClass,
  doxMod,
  docMain,
  docInOutFunc,
  docInOutFunc',
  functionDoc,
  docField,
  modDoc',
  docMod'
) where

import Drasil.Shared.RendererClassesCommon (CommonRenderSym, SMethod )
import Drasil.GOOL.RendererClassesOO ( OORenderSym )
import Drasil.Shared.InterfaceCommon ( SMethod )
import Drasil.GOOL ( SClass, SFile )

-- | Add documentation to a function
doxFunc :: (CommonRenderSym r) => String -> [String] -> Maybe String -> SMethod r -> SMethod r
doxFunc = docFunc functionDox

-- | Add documentation to a class
doxClass :: (OORenderSym r) => String -> SClass r -> SClass r
doxClass = docClass classDox

-- | Add documentation to a module/file
doxMod :: (OORenderSym r) => String -> String -> [String] -> String -> SFile r -> SFile r
doxMod = docMod moduleDox

-- | Document the main function
docMain :: (CommonRenderSym r) => String -> SMethod r -> SMethod r
docMain = docFunc mainDox

-- | Document an input/output function
docInOutFunc :: (CommonRenderSym r) => String -> [String] -> [String] -> SMethod r -> SMethod r
docInOutFunc = docFunc inOutDox

-- | Document an input/output function (alternate)
docInOutFunc' :: (CommonRenderSym r) => String -> [String] -> [String] -> SMethod r -> SMethod r
docInOutFunc' = docFunc inOutDox'

-- | Document a function (generic)
functionDoc :: (CommonRenderSym r) => String -> SMethod r -> SMethod r
functionDoc = docFunc functionDox

-- | Document a field/variable
docField :: (CommonRenderSym r) => String -> SVariable r -> SVariable r
docField = docVar fieldDox

-- | Document a module (alternate)
modDoc' :: (OORenderSym r) => String -> SFile r -> SFile r
modDoc' = docMod moduleDox'

-- | Document a module (alternate)
docMod' :: (OORenderSym r) => String -> SFile r -> SFile r
docMod' = docMod moduleDox'