import sys
sys.path.insert(0, '.')

import unittest
import PCM_Error


class TestCompareFortran(unittest.TestCase):

    def setUp(self):
        self.delta = 0.002

    def test_CF1(self):
        errTw = PCM_Error.PCM_ErrorF('Testing/compareFortran/F01.out', 'P01.out', 'TWat')
        errTp = PCM_Error.PCM_ErrorF('Testing/compareFortran/F01.out', 'P01.out', 'TPCM')
        errEw = PCM_Error.PCM_ErrorF('Testing/compareFortran/F01.out', 'P01.out', 'EWat')
        errEp = PCM_Error.PCM_ErrorF('Testing/compareFortran/F01.out', 'P01.out', 'EPCM')
        self.assertAlmostEqual(errTw, 0, places=None, msg='Water temperature', delta=self.delta)
        self.assertAlmostEqual(errTp, 0, places=None, msg='PCM temperature', delta=self.delta)
        self.assertAlmostEqual(errEw, 0, places=None, msg='Water energy', delta=self.delta)
        self.assertAlmostEqual(errEp, 0, places=None, msg='PCM energy', delta=self.delta)

    def test_CF2(self):
        errTw = PCM_Error.PCM_ErrorF('Testing/compareFortran/F02.out', 'P02.out', 'TWat')
        errTp = PCM_Error.PCM_ErrorF('Testing/compareFortran/F02.out', 'P02.out', 'TPCM')
        errEw = PCM_Error.PCM_ErrorF('Testing/compareFortran/F02.out', 'P02.out', 'EWat')
        errEp = PCM_Error.PCM_ErrorF('Testing/compareFortran/F02.out', 'P02.out', 'EPCM')
        self.assertAlmostEqual(errTw, 0, places=None, msg='Water temperature', delta=self.delta)
        self.assertAlmostEqual(errTp, 0, places=None, msg='PCM temperature', delta=self.delta)
        self.assertAlmostEqual(errEw, 0, places=None, msg='Water energy', delta=self.delta)
        self.assertAlmostEqual(errEp, 0, places=None, msg='PCM energy', delta=self.delta)

    def test_CF3(self):
        errTw = PCM_Error.PCM_ErrorF('Testing/compareFortran/F03.out', 'P03.out', 'TWat')
        errTp = PCM_Error.PCM_ErrorF('Testing/compareFortran/F03.out', 'P03.out', 'TPCM')
        errEw = PCM_Error.PCM_ErrorF('Testing/compareFortran/F03.out', 'P03.out', 'EWat')
        errEp = PCM_Error.PCM_ErrorF('Testing/compareFortran/F03.out', 'P03.out', 'EPCM')
        self.assertAlmostEqual(errTw, 0, places=None, msg='Water temperature', delta=self.delta)
        self.assertAlmostEqual(errTp, 0, places=None, msg='PCM temperature', delta=self.delta)
        self.assertAlmostEqual(errEw, 0, places=None, msg='Water energy', delta=self.delta)
        self.assertAlmostEqual(errEp, 0, places=None, msg='PCM energy', delta=self.delta)

    def test_CF4(self):
        errTw = PCM_Error.PCM_ErrorF('Testing/compareFortran/F04.out', 'P04.out', 'TWat')
        errTp = PCM_Error.PCM_ErrorF('Testing/compareFortran/F04.out', 'P04.out', 'TPCM')
        errEw = PCM_Error.PCM_ErrorF('Testing/compareFortran/F04.out', 'P04.out', 'EWat')
        errEp = PCM_Error.PCM_ErrorF('Testing/compareFortran/F04.out', 'P04.out', 'EPCM')
        self.assertAlmostEqual(errTw, 0, places=None, msg='Water temperature', delta=self.delta)
        self.assertAlmostEqual(errTp, 0, places=None, msg='PCM temperature', delta=self.delta)
        self.assertAlmostEqual(errEw, 0, places=None, msg='Water energy', delta=self.delta)
        self.assertAlmostEqual(errEp, 0, places=None, msg='PCM energy', delta=self.delta)

    def test_CF5(self):
        errTw = PCM_Error.PCM_ErrorF('Testing/compareFortran/F01.out', 'P05.out', 'TWatNoP')
        errEw = PCM_Error.PCM_ErrorF('Testing/compareFortran/F01.out', 'P05.out', 'EWatNoP')
        self.assertAlmostEqual(errTw, 0, places=None, msg='Water temperature', delta=self.delta)
        self.assertAlmostEqual(errEw, 0, places=None, msg='Water energy', delta=self.delta)


class CompareFortranSuite:

    def suite(self):
        suite = unittest.TestLoader().loadTestsFromTestCase(TestCompareFortran)
        return suite