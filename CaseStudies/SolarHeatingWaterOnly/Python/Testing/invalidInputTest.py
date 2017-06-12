import sys
sys.path.insert(0, '.')

import unittest
import load_params
import verify_params


class TestInvalidInput(unittest.TestCase):

    def test_FI01(self):
        params = load_params.load_params('Testing/invalidInput/FI01.txt')
        self.assertRaisesRegexp(ValueError, 'Tank length must be > 0\n', verify_params.verify_valid, params)

    def test_FI02(self):
        params = load_params.load_params('Testing/invalidInput/FI02.txt')
        self.assertRaisesRegexp(ValueError, 'Tank length must be > 0\n', verify_params.verify_valid, params)

    def test_FI03(self):
        params = load_params.load_params('Testing/invalidInput/FI03.txt')
        self.assertRaisesRegexp(ValueError, 'Tank diameter must be > 0\n', verify_params.verify_valid, params)

    def test_FI04(self):
        params = load_params.load_params('Testing/invalidInput/FI04.txt')
        self.assertRaisesRegexp(ValueError, 'Tank diameter must be > 0\n', verify_params.verify_valid, params)

    def test_FI05(self):
        self.assertRaises(ZeroDivisionError, load_params.load_params, 'Testing/invalidInput/FI05.txt')

    def test_FI06(self):
        params = load_params.load_params('Testing/invalidInput/FI06.txt')
        self.assertRaisesRegexp(ValueError, 'Ac must be > 0\n', verify_params.verify_valid, params)

    def test_FI07(self):
        self.assertRaises(ZeroDivisionError, load_params.load_params, 'Testing/invalidInput/FI07.txt')

    def test_FI08(self):
        params = load_params.load_params('Testing/invalidInput/FI08.txt')
        self.assertRaisesRegexp(ValueError, 'Tc must be > 0 and < 100\n', verify_params.verify_valid, params)

    def test_FI09(self):
        params = load_params.load_params('Testing/invalidInput/FI09.txt')
        self.assertRaisesRegexp(ValueError, 'Tc must be > 0 and < 100\n', verify_params.verify_valid, params)

    def test_FI10(self):
        params = load_params.load_params('Testing/invalidInput/FI10.txt')
        self.assertRaisesRegexp(ValueError, 'rho_w must be > 0\n', verify_params.verify_valid, params)

    def test_FI11(self):
        params = load_params.load_params('Testing/invalidInput/FI11.txt')
        self.assertRaisesRegexp(ValueError, 'rho_w must be > 0\n', verify_params.verify_valid, params)

    def test_FI12(self):
        params = load_params.load_params('Testing/invalidInput/FI12.txt')
        self.assertRaisesRegexp(ValueError, 'C_w must be > 0\n', verify_params.verify_valid, params)

    def test_FI13(self):
        params = load_params.load_params('Testing/invalidInput/FI13.txt')
        self.assertRaisesRegexp(ValueError, 'C_w must be > 0\n', verify_params.verify_valid, params)

    def test_FI14(self):
        params = load_params.load_params('Testing/invalidInput/FI14.txt')
        self.assertRaisesRegexp(ValueError, 'hc must be > 0\n', verify_params.verify_valid, params)

    def test_FI15(self):
        self.assertRaises(ZeroDivisionError, load_params.load_params, 'Testing/invalidInput/FI15.txt')

    def test_FI16(self):
        params = load_params.load_params('Testing/invalidInput/FI16.txt')
        self.assertRaisesRegexp(ValueError, 'Tinit must be > 0 and < 100\n', verify_params.verify_valid, params)

    def test_FI17(self):
        params = load_params.load_params('Testing/invalidInput/FI17.txt')
        self.assertRaisesRegexp(ValueError, 'Tc must be > Tinit\n', verify_params.verify_valid, params)

    def test_FI18(self):
        params = load_params.load_params('Testing/invalidInput/FI18.txt')
        self.assertRaisesRegexp(ValueError, 'Tc must be > Tinit\n', verify_params.verify_valid, params)

    def test_FI19(self):
        params = load_params.load_params('Testing/invalidInput/FI19.txt')
        self.assertRaisesRegexp(ValueError, 'tfinal must be > 0\n', verify_params.verify_valid, params)

    def test_FI20(self):
        params = load_params.load_params('Testing/invalidInput/FI20.txt')
        self.assertRaisesRegexp(ValueError, 'tfinal must be > 0\n', verify_params.verify_valid, params)


class InvalidInputSuite:

    def suite(self):
        suite = unittest.TestLoader().loadTestsFromTestCase(TestInvalidInput)
        return suite
