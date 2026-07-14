module Drasil.SRS.DummyChunks (
  sections, tables
) where

import Language.Drasil
import Language.Drasil.Document

import Drasil.SRS.Concepts
import Drasil.SRS.Sections.AuxiliaryConstants (tableOfConstants)
import Drasil.SRS.Sections.SpecificSystemDescription (inDataConstTbl, outDataConstTbl)
import Drasil.SRS.Sections.TableOfAbbAndAcronyms (tableAbbAccGen)
import Drasil.SRS.Sections.TableOfSymbols (table)
import Drasil.SRS.Sections.TableOfUnits (tOfUnitNone)

sections :: [Section]
sections = map (($ []) . ($ []))
  [tOfCont, refMat, tOfUnit, tOfSymb, tOfAbbAcc, intro, prpsOfDoc, scpOfReq,
  charOfIR, orgOfDoc, stakeholder, theCustomer, theClient, genSysDes, sysCont,
  userChar, sysCon, specSysDes, probDesc, termAndDefn, physSyst, goalStmt,
  solCharSpec, assumpt, thModel, genDefn, dataDefn, inModel, datCon, propCorSol,
  require, nonfuncReq, funcReq, likeChg, unlikeChg, traceyMandG, valsOfAuxCons,
  reference, appendix, offShelfSol, scpOfTheProj, prodUCTable, indPRCase,
  termogy]

tables :: [LabelledContent]
tables = [tableAbbAccGen [], table Equational empT (const EmptyS),
  tOfUnitNone [], tableOfConstants [], inDataConstTbl empTCQs, outDataConstTbl empTCQs]
  where
    empT = [] :: [DefinedQuantityDict]
    empTCQs = [] :: [UncertQ]
