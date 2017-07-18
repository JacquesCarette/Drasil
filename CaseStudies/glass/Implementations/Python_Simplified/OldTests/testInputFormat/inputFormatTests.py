import unittest
from . import testInputFormat
from . import testInputFormat2
from . import testInputFormat3

class InputFormatTests(unittest.TestSuite):

    testSuite = unittest.TestSuite()
    testResult = unittest.TestResult()

    testSuite.addTests(map ((lambda x: unittest.TestLoader().loadTestsFromTestCase(x)),
                            [testInputFormat.TestInputFormat, testInputFormat2.TestInputFormat,
                             testInputFormat3.TestInputFormat]))

    testSuite.run(testResult)
    inputFormat_results = testResult.testsRun
    inputFormat_failures = len(testResult.failures)
    inputFormat_successes = inputFormat_results - inputFormat_failures

    print("Ran " + str(inputFormat_results) + " tests for inputFormat.py. " + str(inputFormat_successes) + \
          " passed and " + str(inputFormat_failures) + " failed.")

    if inputFormat_failures > 0:
        print(testResult.failures)

"""
class TestInputFormat(unittest.TestCase):
 
    def setUp(self):
        pass
 
    def test_get_input(self):
        params = param.Param()
        inputFormat.get_input(os.path.join("Test/Inputfiles", inputFileName), params)
        self.assertEqual(aExpctd, params.a)
        self.assertEqual(bExpctd, params.b)
        self.assertEqual(tExpctd, params.t)
        self.assertEqual(gtExpctd, params.gt)
        self.assertEqual(wExpctd, params.w)
        self.assertEqual(tntExpctd, params.tnt)
        self.assertEqual((sdxExpctd, sdyExpctd, sdzExpctd), params.sdvect)
        self.assertEqual(pbTolExpctd, params.pbtol)
        
if __name__ == '__main__':
    unittest.main()
"""
"""
fileName           , inputFileName     , aExpctd, bExpctd, tExpctd, gtExpctd, wExpctd, tntExpctd, sdxExpctd, sdyExpctd, sdzExpctd, pbTolExpctd
testInputFormat.py , "defaultInput.txt", 1600.0 , 1500.0 , 10.0   , "HS"    , 10     , 1.0      , 0.0      , 1.5      , 11.0     , 0.008
testInputFormat2.py, "testInput1.txt"  , 1200.0 , 1000.0 , 8.0    , "AN"    , 10     , 1.0      , 0.0      , 2.0      , 10.0     , 0.010
testInputFormat3.py, "testInput2.txt"  , 2000.0 , 1600.0 , 10.0   , "FT"    , 15     , 1.0      , 0.5      , 1.2      , 9.0      , 0.009
"""
