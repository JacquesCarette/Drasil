#Commented sections pending removal
import sys
sys.path.insert(0, 'Testing/')

import loadParamsTest
import invalidInputTest
import unrecommendedInputTest
#import eventTest
import energyTest
import verifyOutputTest
import compareFortranTest
import compareMatlabTest
import compareCTest
import unittest

suiteLoadParams = loadParamsTest.LoadParamsSuite().suite()
suiteInvalidInput = invalidInputTest.InvalidInputSuite().suite()
suiteUnrecommendedInput = unrecommendedInputTest.UnrecommendedInputSuite().suite()
#suiteEvent = eventTest.EventSuite().suite()
suiteEnergy = energyTest.EnergySuite('suite').suite()
suiteVerifyOutput = verifyOutputTest.VerifyOutputSuite().suite()
suiteCompareFortran = compareFortranTest.CompareFortranSuite().suite()
suiteCompareMatlab = compareMatlabTest.CompareMatlabSuite().suite()
suiteCompareC = compareCTest.CompareCSuite().suite()
suite = unittest.TestSuite([suiteLoadParams, suiteInvalidInput, suiteUnrecommendedInput, #suiteEvent, 
                            suiteEnergy,
                            suiteVerifyOutput, suiteCompareFortran, suiteCompareMatlab, suiteCompareC])
unittest.TextTestRunner().run(suite)
