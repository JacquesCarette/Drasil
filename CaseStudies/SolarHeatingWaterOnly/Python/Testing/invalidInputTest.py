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

    def test_FI10(self):
        self.assertRaises(ZeroDivisionError, load_params.load_params, 'Testing/invalidInput/FI10.txt')

    def test_FI23(self):
        params = load_params.load_params('Testing/invalidInput/FI23.txt')
        self.assertRaisesRegexp(ValueError, 'Ac must be > 0\n', verify_params.verify_valid, params)

    def test_FI24(self):
        self.assertRaises(ZeroDivisionError, load_params.load_params, 'Testing/invalidInput/FI24.txt')

    def test_FI27(self):
        params = load_params.load_params('Testing/invalidInput/FI27.txt')
        self.assertRaisesRegexp(ValueError, 'Tc must be > 0 and < 100\n', verify_params.verify_valid, params)

    def test_FI28(self):
        params = load_params.load_params('Testing/invalidInput/FI28.txt')
        self.assertRaisesRegexp(ValueError, 'Tc must be > 0 and < 100\n', verify_params.verify_valid, params)

    def test_FI29(self):
        params = load_params.load_params('Testing/invalidInput/FI29.txt')
        self.assertRaisesRegexp(ValueError, 'rho_w must be > 0\n', verify_params.verify_valid, params)

    def test_FI30(self):
        params = load_params.load_params('Testing/invalidInput/FI30.txt')
        self.assertRaisesRegexp(ValueError, 'rho_w must be > 0\n', verify_params.verify_valid, params)

    def test_FI31(self):
        params = load_params.load_params('Testing/invalidInput/FI31.txt')
        self.assertRaisesRegexp(ValueError, 'C_w must be > 0\n', verify_params.verify_valid, params)

    def test_FI32(self):
        params = load_params.load_params('Testing/invalidInput/FI32.txt')
        self.assertRaisesRegexp(ValueError, 'C_w must be > 0\n', verify_params.verify_valid, params)

    def test_FI33(self):
        params = load_params.load_params('Testing/invalidInput/FI33.txt')
        self.assertRaisesRegexp(ValueError, 'hc must be > 0\n', verify_params.verify_valid, params)

    def test_FI34(self):
        self.assertRaises(ZeroDivisionError, load_params.load_params, 'Testing/invalidInput/FI34.txt')

    def test_FI36(self):
        self.assertRaises(ZeroDivisionError, load_params.load_params, 'Testing/invalidInput/FI36.txt')

    def test_FI37(self):
        params = load_params.load_params('Testing/invalidInput/FI37.txt')
        self.assertRaisesRegexp(ValueError, 'Tinit must be > 0 and < 100\n', verify_params.verify_valid, params)

    def test_FI38(self):
        params = load_params.load_params('Testing/invalidInput/FI38.txt')
        self.assertRaisesRegexp(ValueError, 'Tinit must be > 0 and < 100\n', verify_params.verify_valid, params)

    def test_FI39(self):
        params = load_params.load_params('Testing/invalidInput/FI39.txt')
        self.assertRaisesRegexp(ValueError, 'Tc must be > Tinit\n', verify_params.verify_valid, params)

    def test_FI40(self):
        params = load_params.load_params('Testing/invalidInput/FI40.txt')
        self.assertRaisesRegexp(ValueError, 'Tc must be > Tinit\n', verify_params.verify_valid, params)

    def test_FI43(self):
        params = load_params.load_params('Testing/invalidInput/FI43.txt')
        self.assertRaisesRegexp(ValueError, 'Tc must be > Tinit\n', verify_params.verify_valid, params)

    def test_FI44(self):
        params = load_params.load_params('Testing/invalidInput/FI44.txt')
        self.assertRaisesRegexp(ValueError, 'Tc must be > Tinit\n', verify_params.verify_valid, params)

    def test_FI45(self):
        params = load_params.load_params('Testing/invalidInput/FI45.txt')
        self.assertRaisesRegexp(ValueError, 'tfinal must be > 0\n', verify_params.verify_valid, params)

    def test_FI46(self):
        params = load_params.load_params('Testing/invalidInput/FI46.txt')
        self.assertRaisesRegexp(ValueError, 'tfinal must be > 0\n', verify_params.verify_valid, params)


class InvalidInputSuite:

    def suite(self):
        suite = unittest.TestLoader().loadTestsFromTestCase(TestInvalidInput)
        return suite
