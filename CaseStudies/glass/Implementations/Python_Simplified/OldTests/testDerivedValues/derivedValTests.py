import unittest
from . import testDerivedValues
from . import testDerivedValues2
from . import testDerivedValues3

class DerivedValTests(unittest.TestSuite):

    testSuite = unittest.TestSuite()
    testResult = unittest.TestResult()
    
    testSuite.addTests(map ((lambda x: unittest.TestLoader().loadTestsFromTestCase(x)),
                            [testDerivedValues.TestDerivedValues, testDerivedValues2.TestDerivedValues,
                             testDerivedValues3.TestDerivedValues]))

    testSuite.run(testResult)
    derivedVal_results = testResult.testsRun
    derivedVal_failures = len(testResult.failures)
    derivedVal_successes = derivedVal_results -derivedVal_failures

    print("Ran " + str(derivedVal_results) + " tests for derivedValues.py. " + str(derivedVal_successes) + \
          " passed and " + str(derivedVal_failures) + " failed.")
      
    if derivedVal_failures > 0:
        print(testResult.failures)


"""
    def setup(self):
        pass
    
    def test_derived_params(self):
        params = param.Param()
        inputFormat.get_input(os.path.join("Test/Inputfiles", inputFileName), params)
        derivedValues.derived_params(params)
        self.assertEqual(arExpected, params.asprat)
        self.assertEqual(sdExpected, params.sd)
        self.assertEqual(ldfExpected, params.ldf)
        self.assertEqual(wtntExpected, params.wtnt)
        self.assertEqual(hExpected, params.h)
        self.assertEqual(gtfExpected, params.gtf)

"""
"""
testFileName      , inputFileName     , arExpected        , sdExpected        , ldfExpected       , wtntExpected, hExpected, gtfExpected
testDerivedValues , "defaultInput.txt", 1.0666666666666667, 11.10180165558726 , 0.2696493494752911, 10.0        , 9.02     , 2
testDerivedValues2, "testInput1.txt"  , 1.2               , 10.198039027185569, 0.2696493494752911, 10.0        , 7.42     , 1
testDerivedValues3, "testInput2.txt"  , 1.25              , 9.093404203047394 , 0.2696493494752911, 15.0        , 9.02     , 4
"""