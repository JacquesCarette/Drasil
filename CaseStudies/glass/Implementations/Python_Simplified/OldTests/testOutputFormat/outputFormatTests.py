import unittest
from . import testOutputFormat
from . import testOutputFormat2
from . import testOutputFormat3

class OutputFormatTests(unittest.TestSuite):

    testSuite = unittest.TestSuite()
    testResult = unittest.TestResult()

    testSuite.addTests(map ((lambda x: unittest.TestLoader().loadTestsFromTestCase(x)),
                            [testOutputFormat.TestOutputFormat, testOutputFormat2.TestOutputFormat,
                             testOutputFormat3.TestOutputFormat]))

    testSuite.run(testResult)
    outputFormat_results = testResult.testsRun
    outputFormat_failures = len(testResult.failures)
    outputFormat_successes = outputFormat_results - outputFormat_failures
    
    print("Ran " + str(outputFormat_results) + " tests for outputFormat.py. " + str(outputFormat_successes) + \
          " passed and " + str(outputFormat_failures) + " failed.")

    if outputFormat_failures > 0:
        print(testResult.failures)

"""
class TestOutputFormat(unittest.TestCase):
    
    def setUp(self):
        self.params = param.Param()
        self.inputFormat = inputFormat.get_input(inputFileName, self.params)
        self.derivedValues = derivedValues.derived_params(self.params)
        self.output = outputFormat.display_output("testoutput.txt", qVal, jVal,
                                                qHatTolVal, pbVal, lrVal, 
                                                nflVal, is_safe1State, is_safe2State,
                                                self.params) 
         
    def test_display_output(self):
        f1 = open(os.path.join("Test/Inputfiles", outputFileName), "r")
        f2 = open("testoutput.txt", "r")
        text1 = f1.readlines()
        text2 = f2.readlines()
        self.assertEqual(text1,text2)
        f1.close()
        f2.close()
        
if __name__ == "__main__":
        unittest.main()
"""
"""
fileName.py      , inputFileName     , qVal                 , jVal                 , qHatTolVal           , pbVal                , lrVal                , nflVal               , is_safe1State, is_safe2State, outputFileName
testOutputFormat , "defaultInput.txt", 3.258285992018616e+00, 9.548951781296090e+00, 4.152349099707993e+01, 1.301524590203718e-04, 6.843002155788037e+00, 3.421501077894018e+00, 1.000000     , 1.000000     , "output.txt"
testOutputFormat2, "testInput3.txt"  , 3.258285992018616e+00, 9.548951781296090e+00, 1.895908087386919e+01, 1.301524590203718e-04, 3.124424950223241e+00, 1.562212475111620e+00, 0.000000     , 0.000000     , "output3.txt"
testOutputFormat3, "testInput4.txt"  , 7.377747177423622e+00, 1.406670550988096e+01, 4.152349099707993e+01, 1.185574651484522e-02, 6.843002155788037e+00, 3.421501077894018e+00, 0.000000     , 0.000000     , "output4.txt"
"""