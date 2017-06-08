import sys
sys.path.insert(0, '.')

import unittest
import load_params
import verify_params
import warnings


class TestUnrecommendedInput(unittest.TestCase):

    def test_UI01(self):
        params = load_params.load_params('Testing/unrecommendedInput/UI01.txt')
        with warnings.catch_warnings(record=True) as w:
            verify_params.verify_recommended(params)
            assert issubclass(w[0].category, UserWarning)
            assert 'It is recommended that 0.1 <= L <= 50\n' in str(w[0].message)

    def test_UI02(self):
        params = load_params.load_params('Testing/unrecommendedInput/UI02.txt')
        with warnings.catch_warnings(record=True) as w:
            verify_params.verify_recommended(params)
            assert issubclass(w[0].category, UserWarning)
            assert 'It is recommended that 0.1 <= L <= 50\n' in str(w[0].message)

    def test_UI03(self):
        params = load_params.load_params('Testing/unrecommendedInput/UI03.txt')
        with warnings.catch_warnings(record=True) as w:
            verify_params.verify_recommended(params)
            assert issubclass(w[0].category, UserWarning)
            assert 'It is recommended that 0.002 <= D/L <= 200\n' in str(w[0].message)

    def test_UI04(self):
        params = load_params.load_params('Testing/unrecommendedInput/UI04.txt')
        with warnings.catch_warnings(record=True) as w:
            verify_params.verify_recommended(params)
            assert issubclass(w[0].category, UserWarning)
            assert 'It is recommended that 0.002 <= D/L <= 200\n' in str(w[0].message)

    def test_UI05(self):
        params = load_params.load_params('Testing/unrecommendedInput/UI05.txt')
        with warnings.catch_warnings(record=True) as w:
            verify_params.verify_recommended(params)
            assert issubclass(w[0].category, UserWarning)
            assert 'It is recommended that Ac <= pi * (D/2) ^ 2\n' in str(w[0].message)

    def test_UI06(self):
        params = load_params.load_params('Testing/unrecommendedInput/UI06.txt')
        with warnings.catch_warnings(record=True) as w:
            verify_params.verify_recommended(params)
            assert issubclass(w[0].category, UserWarning)
            assert 'It is recommended that 950 < rho_w <= 1000\n' in str(w[0].message)

    def test_UI07(self):
        params = load_params.load_params('Testing/unrecommendedInput/UI07.txt')
        with warnings.catch_warnings(record=True) as w:
            verify_params.verify_recommended(params)
            assert issubclass(w[0].category, UserWarning)
            assert 'It is recommended that 950 < rho_w <= 1000\n' in str(w[0].message)

    def test_UI08(self):
        params = load_params.load_params('Testing/unrecommendedInput/UI08.txt')
        with warnings.catch_warnings(record=True) as w:
            verify_params.verify_recommended(params)
            assert issubclass(w[0].category, UserWarning)
            assert 'It is recommended that 4170 < C_w < 4210\n' in str(w[0].message)

    def test_UI09(self):
        params = load_params.load_params('Testing/unrecommendedInput/UI09.txt')
        with warnings.catch_warnings(record=True) as w:
            verify_params.verify_recommended(params)
            assert issubclass(w[0].category, UserWarning)
            assert 'It is recommended that 4170 < C_w < 4210\n' in str(w[0].message)

    def test_UI10(self):
        params = load_params.load_params('Testing/unrecommendedInput/UI10.txt')
        with warnings.catch_warnings(record=True) as w:
            verify_params.verify_recommended(params)
            assert issubclass(w[0].category, UserWarning)
            assert 'It is recommended that 10 < hc < 10000\n' in str(w[0].message)

    def test_UI11(self):
        params = load_params.load_params('Testing/unrecommendedInput/UI11.txt')
        with warnings.catch_warnings(record=True) as w:
            verify_params.verify_recommended(params)
            assert issubclass(w[0].category, UserWarning)
            assert 'It is recommended that 10 < hc < 10000\n' in str(w[0].message)

    def test_UI12(self):
        params = load_params.load_params('Testing/unrecommendedInput/UI12.txt')
        with warnings.catch_warnings(record=True) as w:
            verify_params.verify_recommended(params)
            assert issubclass(w[0].category, UserWarning)
            assert 'It is recommended that 0 < tfinal < 86400\n' in str(w[0].message)


class UnrecommendedInputSuite:

    def suite(self):
        suite = unittest.TestLoader().loadTestsFromTestCase(TestUnrecommendedInput)
        return suite
