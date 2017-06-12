#Commented sections pending removal
#No valid comparison tests available for NoPCM hence the lack of action. If/when useable data becomes available,
#apply the same setup as compareFortranTest.py as seen in test_CM5
import sys
sys.path.insert(0, '.')

import unittest
import PCM_Error


class TestCompareC(unittest.TestCase):

    def setUp(self):
        self.delta = 0.000001

    # def test_CC1(self):
        # errTw = PCM_Error.PCM_ErrorC('Testing/compareC/C01.out', 'P01.out', 'TWat')
        # errTp = PCM_Error.PCM_ErrorC('Testing/compareC/C01.out', 'P01.out', 'TPCM')
        # errEw = PCM_Error.PCM_ErrorC('Testing/compareC/C01.out', 'P01.out', 'EWat')
        # errEp = PCM_Error.PCM_ErrorC('Testing/compareC/C01.out', 'P01.out', 'EPCM')
        # self.assertAlmostEqual(errTw, 0, places=None, msg='Water temperature', delta=self.delta)
        # self.assertAlmostEqual(errTp, 0, places=None, msg='PCM temperature', delta=self.delta)
        # self.assertAlmostEqual(errEw, 0, places=None, msg='Water energy', delta=self.delta)
        # self.assertAlmostEqual(errEp, 0, places=None, msg='PCM energy', delta=self.delta)

    # def test_CC2(self):
        # errTw = PCM_Error.PCM_ErrorC('Testing/compareC/C02.out', 'P02.out', 'TWat')
        # errTp = PCM_Error.PCM_ErrorC('Testing/compareC/C02.out', 'P02.out', 'TPCM')
        # errEw = PCM_Error.PCM_ErrorC('Testing/compareC/C02.out', 'P02.out', 'EWat')
        # errEp = PCM_Error.PCM_ErrorC('Testing/compareC/C02.out', 'P02.out', 'EPCM')
        # self.assertAlmostEqual(errTw, 0, places=None, msg='Water temperature', delta=self.delta)
        # self.assertAlmostEqual(errTp, 0, places=None, msg='PCM temperature', delta=self.delta)
        # self.assertAlmostEqual(errEw, 0, places=None, msg='Water energy', delta=self.delta)
        # self.assertAlmostEqual(errEp, 0, places=None, msg='PCM energy', delta=self.delta)

    # def test_CC3(self):
        # errTw = PCM_Error.PCM_ErrorC('Testing/compareC/C03.out', 'P03.out', 'TWat')
        # errTp = PCM_Error.PCM_ErrorC('Testing/compareC/C03.out', 'P03.out', 'TPCM')
        # errEw = PCM_Error.PCM_ErrorC('Testing/compareC/C03.out', 'P03.out', 'EWat')
        # errEp = PCM_Error.PCM_ErrorC('Testing/compareC/C03.out', 'P03.out', 'EPCM')
        # self.assertAlmostEqual(errTw, 0, places=None, msg='Water temperature', delta=self.delta)
        # self.assertAlmostEqual(errTp, 0, places=None, msg='PCM temperature', delta=self.delta)
        # self.assertAlmostEqual(errEw, 0, places=None, msg='Water energy', delta=self.delta)
        # self.assertAlmostEqual(errEp, 0, places=None, msg='PCM energy', delta=self.delta)

    # def test_CC4(self):
        # errTw = PCM_Error.PCM_ErrorC('Testing/compareC/C04.out', 'P04.out', 'TWat')
        # errTp = PCM_Error.PCM_ErrorC('Testing/compareC/C04.out', 'P04.out', 'TPCM')
        # errEw = PCM_Error.PCM_ErrorC('Testing/compareC/C04.out', 'P04.out', 'EWat')
        # errEp = PCM_Error.PCM_ErrorC('Testing/compareC/C04.out', 'P04.out', 'EPCM')
        # self.assertAlmostEqual(errTw, 0, places=None, msg='Water temperature', delta=self.delta)
        # self.assertAlmostEqual(errTp, 0, places=None, msg='PCM temperature', delta=self.delta)
        # self.assertAlmostEqual(errEw, 0, places=None, msg='Water energy', delta=self.delta)
        # self.assertAlmostEqual(errEp, 0, places=None, msg='PCM energy', delta=self.delta)
        
    ##def test_CF5(self):##
        ##errTw = PCM_Error.PCM_ErrorC('Testing/compareC/C01.out', 'P05.out', 'TWatNoP')##
        ##errEw = PCM_Error.PCM_ErrorC('Testing/compareC/C01.out', 'P05.out', 'EWatNoP')##
        ##self.assertAlmostEqual(errTw, 0, places=None, msg='Water temperature', delta=self.delta)##
        ##self.assertAlmostEqual(errEw, 0, places=None, msg='Water energy', delta=self.delta)##

class CompareCSuite:

    def suite(self):
        suite = unittest.TestLoader().loadTestsFromTestCase(TestCompareC)
        return suite