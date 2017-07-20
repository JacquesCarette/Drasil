import unittest
from . import testMainfun
from . import testMainfun2
from . import testMainfun3
from . import testMainfun4
from . import testMainfun5
from . import testMainfun6
from . import testMainfun7

class MainTests(unittest.TestSuite):

    testSuite = unittest.TestSuite()
    testResult = unittest.TestResult()

    testSuite.addTests(map ((lambda x: unittest.TestLoader().loadTestsFromTestCase(x)),
                            [testMainfun.TestMainfun, testMainfun2.TestMainfun,
                             testMainfun3.TestMainfun, testMainfun4.TestMainfun,
                             testMainfun5.TestMainfun, testMainfun6.TestMainfun,
                             testMainfun7.TestMainfun]))

    testSuite.run(testResult)
    main_results = testResult.testsRun
    main_failures = len(testResult.failures)
    main_successes = main_results - main_failures

    print("Ran " + str(main_results) + " tests for mainfun.py. " + str(main_successes) + \
          " passed and " + str(main_failures) + " failed.")

    if main_failures > 0:
        print(testResult.failures)

"""
    def setUp(self):
        self.output = mainfun.main(os.path.join("Test/Inputfiles", inputFileName))
        
    def test_main(self):
        f1 = open("outputfile.txt", 'r')
        f2 = open(os.path.join("Test/Inputfiles", outputFileName), 'r')
        text1 = f1.readlines()
        text2 = f2.readlines()
        self.assertEqual(text1, text2)
        f1.close()
        f2.close()

if __name__ == '__main__':
    unittest.main()
"""
"""
fileName.py , inputFileName     , outputFileName
testMainFun , "defaultInput.txt", "output.txt"
testMainFun2, "testInput1.txt"  , "output1.txt"
testMainFun3, "testInput2.txt"  , "output2.txt"
testMainFun4, "testInput3.txt"  , "output3.txt"
testMainFun5, "testInput4.txt"  , "output4.txt"
testMainFun6, "testInput5.txt"  , "output5.txt"
testMainFun7, "testInput6.txt"  , "output6.txt"
"""