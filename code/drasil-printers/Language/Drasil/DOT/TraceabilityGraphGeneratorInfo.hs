module Language.Drasil.DOT.TraceabilityGraphGeneratorInfo (getInfo) where

import Data.List
import System.IO

-- node labels + styles for each chunk
iML = [(1,"eBalanceOnWtr"), (2,"eBalanceOnPCM"), (3,"heatEInWtr"), (4,"heatEInPCM")]
iMS = ("IM", " [color=black, fillcolor=khaki1, label=\"IM: ", "\", style=filled];")

fRL = [(1,"inputQuantities"), (2,"findMass"), (3,"checkWithPhysConsts"), (4,"outputInputDerivVals"),
       (5,"calcTempWtrOverTime"), (6,"calcTempPCMOverTime"), (7,"calcChgHeatEnergyWtrOverTime"),
       (8,"calcChgHeatEnergyPCMOverTime"), (9,"verifyEnergyOutput"), (10,"calcPCMMeltBegin"), (11,"calcPCMMeltEnd")]
fRS = ("FR", " [color=black, fillcolor=gainsboro, label=\"FR: ", "\", style=filled];")

tML = [(1,"consThermE"), (2,"senseHtE"), (3,"latentHtE"), (4,"nwtnCooling")]
tMS = ("TM", " [color=black, fillcolor=green3, label=\"TM: ", "\", style=filled];")

gDL = [(1,"rocTempSimp"), (2,"htFluxWaterFromCoil"), (3,"htFluxPCMFromWater")]
gDS = ("GD", " [color=black, fillcolor=lightskyblue, label=\"GD: ", "\", style=filled];")

dDL = [(1,"balanceDecayRate"), (2,"balanceDecayTime"), (3,"balanceSolidPCM"), 
       (4,"balanceLiquidPCM"), (5,"htFusion"), (6,"meltFrac"), (7,"aspectRatio")]
dDS = ("DD", " [color=black, fillcolor=paleturquoise1, label=\"DD: ", "\", style=filled];")

aL = [(1,"thermEOnly"), (2,"heatTransCoeffsConst"), (3,"constWtrTempAcrossTank"), (4,"tempPCMConstAcrossVol"),
      (5,"densityWtrPCMConstOverVol"), (6,"specHeatEConstOverVol"), (7,"nwtnLawConvCoolCoilWtr"),
      (8,"tempHeatCoilConstOverTime"), (9,"tempHeatCoilConstOverLength"), (10,"tempHeatCoilConstOverLength"),
      (11,"chrgingTankNoTempDischrg"), (12,"sameInitialTempWtrPCM"), (13,"PCMInitiallySolid"), (14,"wtrAlwaysLiquid"),
      (15,"perfectInsulationTank"), (16,"noInternalHeatGenByWtrPCM"), (17,"volChgMeltingPCMNegligible"), 
      (18,"noGaseousStatePCM"), (19,"atmosPressureTank"), (20,"volCoilNegligible")]
aS = ("AS", " [color=black, fillcolor=brown1, label=\"A: ", "\", style=filled];")

lCL = [(1,"uniformTempPCM"), (2,"tempCoilVarOverDay"), (3,"tempCoilVarOverLength"),
       (4,"dischrgingTank"), (5,"diffInitialTempsPCMWater"), (6,"tankLoseHeat")]
lCS = ("LC", " [color=black, fillcolor=burlywood3, label=\"LC: ", "\", style=filled];")

uCL = [(1,"wtrPCMFixedStates"), (2,"noInternalHeatGen"), (3,"noGaseousState")]
uCS = ("UC", " [color=black, fillcolor=aquamarine2, label=\"UC: ", "\", style=filled];")


type Chunk = String

-- creates chunk node with node style + label
createNode :: (String, String, String) -> (Int, String) -> String
createNode (head, body, tail) (num, label) = head ++ show num ++ body ++ label ++ tail

-- output the following: node definitions + relationships
getInfo :: [Chunk] -> [String]
getInfo chunk =
  if chunk == ["all"] then
    let iMods = map (createNode iMS) iML
        funcReqs = map (createNode fRS) fRL
        tMods = map (createNode tMS) tML
        genDefs = map (createNode gDS) gDL
        dataDefs = map (createNode dDS) dDL
        assumps = map (createNode aS) aL
        likeChgs = map (createNode lCS) lCL 
        unlikeChgs = map (createNode uCS) uCL
    in iMods ++ [""] ++ funcReqs  ++ [""] ++ tMods ++ [""] ++ genDefs ++ [""] ++ dataDefs ++ [""] ++ assumps ++ [""] ++ likeChgs ++ [""] ++ unlikeChgs
    -- in return (foldl (++) [] (intersperse [""] [iMods,funcReqs,tMods,genDefs,dataDefs,assumps,likeChgs,unlikeChgs]))
  else []

-- nodeDefns =
