import sys
sys.path.insert(0, '.')

import unittest
import PCM_Error


class TestCompareMatlab(unittest.TestCase):

    def setUp(self):
        self.delta = 0.000005

    def test_CM1(self):
        errTw = PCM_Error.PCM_ErrorM('Testing/compareMatlab/M01.out', 'P01.out', 'TWat')
        errTp = PCM_Error.PCM_ErrorM('Testing/compareMatlab/M01.out', 'P01.out', 'TPCM')
        errEw = PCM_Error.PCM_ErrorM('Testing/compareMatlab/M01.out', 'P01.out', 'EWat')
        errEp = PCM_Error.PCM_ErrorM('Testing/compareMatlab/M01.out', 'P01.out', 'EPCM')
        self.assertAlmostEqual(errTw, 0, places=None, msg='Water temperature', delta=self.delta)
        self.assertAlmostEqual(errTp, 0, places=None, msg='PCM temperature', delta=self.delta)
        self.assertAlmostEqual(errEw, 0, places=None, msg='Water energy', delta=self.delta)
        self.assertAlmostEqual(errEp, 0, places=None, msg='PCM energy', delta=self.delta)

    def test_CM2(self):
        errTw = PCM_Error.PCM_ErrorM('Testing/compareMatlab/M02.out', 'P02.out', 'TWat')
        errTp = PCM_Error.PCM_ErrorM('Testing/compareMatlab/M02.out', 'P02.out', 'TPCM')
        errEw = PCM_Error.PCM_ErrorM('Testing/compareMatlab/M02.out', 'P02.out', 'EWat')
        errEp = PCM_Error.PCM_ErrorM('Testing/compareMatlab/M02.out', 'P02.out', 'EPCM')
        self.assertAlmostEqual(errTw, 0, places=None, msg='Water temperature', delta=self.delta)
        self.assertAlmostEqual(errTp, 0, places=None, msg='PCM temperature', delta=self.delta)
        self.assertAlmostEqual(errEw, 0, places=None, msg='Water energy', delta=self.delta)
        self.assertAlmostEqual(errEp, 0, places=None, msg='PCM energy', delta=self.delta)

    def test_CM3(self):
        errTw = PCM_Error.PCM_ErrorM('Testing/compareMatlab/M03.out', 'P03.out', 'TWat')
        errTp = PCM_Error.PCM_ErrorM('Testing/compareMatlab/M03.out', 'P03.out', 'TPCM')
        errEw = PCM_Error.PCM_ErrorM('Testing/compareMatlab/M03.out', 'P03.out', 'EWat')
        errEp = PCM_Error.PCM_ErrorM('Testing/compareMatlab/M03.out', 'P03.out', 'EPCM')
        self.assertAlmostEqual(errTw, 0, places=None, msg='Water temperature', delta=self.delta)
        self.assertAlmostEqual(errTp, 0, places=None, msg='PCM temperature', delta=self.delta)
        self.assertAlmostEqual(errEw, 0, places=None, msg='Water energy', delta=self.delta)
        self.assertAlmostEqual(errEp, 0, places=None, msg='PCM energy', delta=self.delta)

    def test_CM4(self):
        errTw = PCM_Error.PCM_ErrorM('Testing/compareMatlab/M04.out', 'P04.out', 'TWat')
        errTp = PCM_Error.PCM_ErrorM('Testing/compareMatlab/M04.out', 'P04.out', 'TPCM')
        errEw = PCM_Error.PCM_ErrorM('Testing/compareMatlab/M04.out', 'P04.out', 'EWat')
        errEp = PCM_Error.PCM_ErrorM('Testing/compareMatlab/M04.out', 'P04.out', 'EPCM')
        self.assertAlmostEqual(errTw, 0, places=None, msg='Water temperature', delta=self.delta)
        self.assertAlmostEqual(errTp, 0, places=None, msg='PCM temperature', delta=self.delta)
        self.assertAlmostEqual(errEw, 0, places=None, msg='Water energy', delta=self.delta)
        self.assertAlmostEqual(errEp, 0, places=None, msg='PCM energy', delta=self.delta)


class CompareMatlabSuite:

    def suite(self):
        suite = unittest.TestLoader().loadTestsFromTestCase(TestCompareMatlab)
        return suite