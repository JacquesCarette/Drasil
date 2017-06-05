import sys
sys.path.insert(0, '.')

#Commented sections pending removal
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

    # def test_FI05(self):
        # params = load_params.load_params('Testing/invalidInput/FI05.txt')
        # self.assertRaisesRegexp(ValueError, 'PCM volume must be > 0\n', verify_params.verify_valid, params)

    # def test_FI06(self):
        # params = load_params.load_params('Testing/invalidInput/FI06.txt')
        # self.assertRaisesRegexp(ValueError, 'PCM volume must be > 0\n', verify_params.verify_valid, params)

    # def test_FI07(self):
        # params = load_params.load_params('Testing/invalidInput/FI07.txt')
        # self.assertRaisesRegexp(ValueError, 'PCM volume must be < tank volume\n', verify_params.verify_valid, params)

    # def test_FI08(self):
        # params = load_params.load_params('Testing/invalidInput/FI08.txt')
        # self.assertRaisesRegexp(ValueError, 'PCM volume must be < tank volume\n', verify_params.verify_valid, params)

    # def test_FI09(self):
        # params = load_params.load_params('Testing/invalidInput/FI09.txt')
        # self.assertRaisesRegexp(ValueError, 'PCM area must be > 0\n', verify_params.verify_valid, params)

    def test_FI10(self):
        self.assertRaises(ZeroDivisionError, load_params.load_params, 'Testing/invalidInput/FI10.txt')

    # def test_FI11(self):
        # params = load_params.load_params('Testing/invalidInput/FI11.txt')
        # self.assertRaisesRegexp(ValueError, 'rho_p must be > 0\n', verify_params.verify_valid, params)

    # def test_FI12(self):
        # params = load_params.load_params('Testing/invalidInput/FI12.txt')
        # self.assertRaisesRegexp(ValueError, 'rho_p must be > 0\n', verify_params.verify_valid, params)

    # def test_FI13(self):
        # params = load_params.load_params('Testing/invalidInput/FI13.txt')
        # self.assertRaisesRegexp(ValueError, 'Tmelt must be > 0 and < Tc\n', verify_params.verify_valid, params)

    # def test_FI14(self):
        # params = load_params.load_params('Testing/invalidInput/FI14.txt')
        # self.assertRaisesRegexp(ValueError, 'Tmelt must be > 0 and < Tc\n', verify_params.verify_valid, params)

    # def test_FI15(self):
        # params = load_params.load_params('Testing/invalidInput/FI15.txt')
        # self.assertRaisesRegexp(ValueError, 'Tmelt must be > 0 and < Tc\n', verify_params.verify_valid, params)

    # def test_FI16(self):
        # params = load_params.load_params('Testing/invalidInput/FI16.txt')
        # self.assertRaisesRegexp(ValueError, 'Tmelt must be > 0 and < Tc\n', verify_params.verify_valid, params)

    # def test_FI17(self):
        # params = load_params.load_params('Testing/invalidInput/FI17.txt')
        # self.assertRaisesRegexp(ValueError, 'C_ps must be > 0\n', verify_params.verify_valid, params)

    # def test_FI18(self):
        # params = load_params.load_params('Testing/invalidInput/FI18.txt')
        # self.assertRaisesRegexp(ValueError, 'C_ps must be > 0\n', verify_params.verify_valid, params)

    # def test_FI19(self):
        # params = load_params.load_params('Testing/invalidInput/FI19.txt')
        # self.assertRaisesRegexp(ValueError, 'C_pl must be > 0\n', verify_params.verify_valid, params)

    # def test_FI20(self):
        # params = load_params.load_params('Testing/invalidInput/FI20.txt')
        # self.assertRaisesRegexp(ValueError, 'C_pl must be > 0\n', verify_params.verify_valid, params)

    # def test_FI21(self):
        # params = load_params.load_params('Testing/invalidInput/FI21.txt')
        # self.assertRaisesRegexp(ValueError, 'Hf must be > 0\n', verify_params.verify_valid, params)

    # def test_FI22(self):
        # params = load_params.load_params('Testing/invalidInput/FI22.txt')
        # self.assertRaisesRegexp(ValueError, 'Hf must be > 0\n', verify_params.verify_valid, params)

    def test_FI23(self):
        params = load_params.load_params('Testing/invalidInput/FI23.txt')
        self.assertRaisesRegexp(ValueError, 'Ac must be > 0\n', verify_params.verify_valid, params)

    def test_FI24(self):
        self.assertRaises(ZeroDivisionError, load_params.load_params, 'Testing/invalidInput/FI24.txt')

    # def test_FI25(self):
        # params = load_params.load_params('Testing/invalidInput/FI25.txt')
        # self.assertRaisesRegexp(ValueError, 'Tmelt must be > 0 and < Tc\n', verify_params.verify_valid, params)

    # def test_FI26(self):
        # params = load_params.load_params('Testing/invalidInput/FI26.txt')
        # self.assertRaisesRegexp(ValueError, 'Tmelt must be > 0 and < Tc\n', verify_params.verify_valid, params)

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

    # def test_FI35(self):
        # params = load_params.load_params('Testing/invalidInput/FI35.txt')
        # self.assertRaisesRegexp(ValueError, 'hp must be > 0\n', verify_params.verify_valid, params)

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

    # def test_FI41(self):
        # params = load_params.load_params('Testing/invalidInput/FI41.txt')
        # self.assertRaisesRegexp(ValueError, 'Tinit must be < Tmelt\n', verify_params.verify_valid, params)

    # def test_FI42(self):
        # params = load_params.load_params('Testing/invalidInput/FI42.txt')
        # self.assertRaisesRegexp(ValueError, 'Tinit must be < Tmelt\n', verify_params.verify_valid, params)

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
