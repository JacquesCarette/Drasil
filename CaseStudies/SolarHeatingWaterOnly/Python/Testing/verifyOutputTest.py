#Commented sections pending removal
import sys
sys.path.insert(0, '.')

import unittest
import load_params
import warnings
import verify_output


class TestVerifyOutput(unittest.TestCase):

    def setUp(self):
        self.params = load_params.load_params('test.in')
        self.time = [0, 10, 20, 30]
        self.tempW = [40, 42, 44, 46]
        #self.tempP = [40, 41.9, 43.8, 45.7]

    def test_VO1(self):
        eW = [0, 1000, 2000, 25200]
        #eP = [0, 1000, 2000, 5400]
        with warnings.catch_warnings(record=True) as w:
            verify_output.verify_output(self.time, self.tempW, eW, self.params)#self.tempP, eW, eP, self.params)
            assert len(w) is 0

    def test_VO2(self):
        eW = [0, 1000, 2000, 25200]
        #eP = [0, 1000, 2000, 3000]
        with warnings.catch_warnings(record=True) as w:
            verify_output.verify_output(self.time, self.tempW, eW, self.params)#self.tempP, eW, eP, self.params)
            assert issubclass(w[0].category, UserWarning)
            assert ('There is > ' + str(self.params.ConsTol) + '% relative error between the energy in the PCM output' +
                    ' and the expected output based on the law of conservation of energy.\n') in str(w[0].message)

    def test_VO3(self):
        eW = [0, 1000, 2000, 3000]
        #eP = [0, 1000, 2000, 5400]
        with warnings.catch_warnings(record=True) as w:
            verify_output.verify_output(self.time, self.tempW, eW, self.params)#self.tempP, eW, eP, self.params)
            assert issubclass(w[0].category, UserWarning)
            assert ('There is > ' + str(self.params.ConsTol) + '% relative error between the energy in the water ' +
                    'output and the expected output based on the law of conservation of energy.\n') in str(w[0].message)

    def test_VO4(self):
        eW = [0, 1000, 2000, 3000]
        #eP = [0, 1000, 2000, 3000]
        with warnings.catch_warnings(record=True) as w:
            verify_output.verify_output(self.time, self.tempW, eW, self.params)#self.tempP, eW, eP, self.params)
            assert issubclass(w[0].category, UserWarning)
            assert issubclass(w[1].category, UserWarning)
            assert ('There is > ' + str(self.params.ConsTol) + '% relative error between the energy in the water ' +
                    'output and the expected output based on the law of conservation of energy.\n') in str(w[0].message)
            assert ('There is > ' + str(self.params.ConsTol) + '% relative error between the energy in the PCM output' +
                    ' and the expected output based on the law of conservation of energy.\n') in str(w[1].message)


class VerifyOutputSuite:

    def suite(self):
        suite = unittest.TestLoader().loadTestsFromTestCase(TestVerifyOutput)
        return suite
