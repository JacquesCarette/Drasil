import sys
sys.path.insert(0, '.')

import unittest
import load_params
import energy


class TestEnergy(unittest.TestCase):

    def setUp(self):
        self.params = load_params.load_params('test.in')

    def test_E1W(self):
        temp = [40, 41, 42, 43, 44]
        expE = [0, 627795.09369793726918140921263406, 1255590.1873958745383628184252681,
                1883385.2810938118075442276379022, 2511180.3747917490767256368505363]
        eWat = energy.energy1Wat(temp, self.params)
        self.assertAlmostEqual(eWat[0], expE[0], places=None, msg='energy1Wat: eWat[0]', delta=1e-9)
        self.assertAlmostEqual(eWat[1], expE[1], places=None, msg='energy1Wat: eWat[1]', delta=1e-9)
        self.assertAlmostEqual(eWat[2], expE[2], places=None, msg='energy1Wat: eWat[2]', delta=1e-9)
        self.assertAlmostEqual(eWat[3], expE[3], places=None, msg='energy1Wat: eWat[3]', delta=1e-9)
        self.assertAlmostEqual(eWat[4], expE[4], places=None, msg='energy1Wat: eWat[4]', delta=1e-9)

    # def test_E1P(self):
        # temp = [40, 41, 42, 43, 44]
        # expE = [0, 88616.0, 177232.0, 265848.0, 354464.0]
        # ePCM = energy.energy1PCM(temp, self.params)
        # self.assertAlmostEqual(ePCM[0], expE[0], places=None, msg='energy1PCM: ePCM[0]', delta=1e-9)
        # self.assertAlmostEqual(ePCM[1], expE[1], places=None, msg='energy1PCM: ePCM[1]', delta=1e-9)
        # self.assertAlmostEqual(ePCM[2], expE[2], places=None, msg='energy1PCM: ePCM[2]', delta=1e-9)
        # self.assertAlmostEqual(ePCM[3], expE[3], places=None, msg='energy1PCM: ePCM[3]', delta=1e-9)
        # self.assertAlmostEqual(ePCM[4], expE[4], places=None, msg='energy1PCM: ePCM[4]', delta=1e-9)

    def test_E2W(self):
        temp = [44.2, 44.3, 44.4, 44.5, 44.6]
        expE = [2636739.3935313365305619186930631, 2699518.9029011302574800596143265, 2762298.4122709239843982005355899,
                2825077.9216407177113163414568533, 2887857.4310105114382344823781167]
        eWat = energy.energy2Wat(temp, self.params)
        self.assertAlmostEqual(eWat[0], expE[0], places=None, msg='energy2Wat: eWat[0]', delta=1e-8)
        self.assertAlmostEqual(eWat[1], expE[1], places=None, msg='energy2Wat: eWat[1]', delta=1e-8)
        self.assertAlmostEqual(eWat[2], expE[2], places=None, msg='energy2Wat: eWat[2]', delta=1e-8)
        self.assertAlmostEqual(eWat[3], expE[3], places=None, msg='energy2Wat: eWat[3]', delta=1e-9)
        self.assertAlmostEqual(eWat[4], expE[4], places=None, msg='energy2Wat: eWat[4]', delta=1e-9)

    # def test_E2P(self):
        # latentHeat= [372000, 423000, 474000, 525000, 576000]
        # expE = [744187.2, 795187.2, 846187.2, 897187.2, 948187.2]
        # ePCM = energy.energy2PCM(latentHeat, self.params)
        # self.assertAlmostEqual(ePCM[0], expE[0], places=None, msg='energy2PCM: ePCM[0]', delta=1e-9)
        # self.assertAlmostEqual(ePCM[1], expE[1], places=None, msg='energy2PCM: ePCM[1]', delta=1e-9)
        # self.assertAlmostEqual(ePCM[2], expE[2], places=None, msg='energy2PCM: ePCM[2]', delta=1e-9)
        # self.assertAlmostEqual(ePCM[3], expE[3], places=None, msg='energy2PCM: ePCM[3]', delta=1e-9)
        # self.assertAlmostEqual(ePCM[4], expE[4], places=None, msg='energy2PCM: ePCM[4]', delta=1e-9)

    def test_E3W(self):
        temp = [45, 46, 47, 48, 49]
        expE = [3138975.4684896863459070460631703, 3766770.5621876236150884552758044, 4394565.6558855608842698644884384,
                5022360.7495834981534512737010725, 5650155.8432814354226326829137066]
        eWat = energy.energy3Wat(temp, self.params)
        self.assertAlmostEqual(eWat[0], expE[0], places=None, msg='energy3Wat: eWat[0]', delta=1e-9)
        self.assertAlmostEqual(eWat[1], expE[1], places=None, msg='energy3Wat: eWat[1]', delta=1e-9)
        self.assertAlmostEqual(eWat[2], expE[2], places=None, msg='energy3Wat: eWat[2]', delta=1e-9)
        self.assertAlmostEqual(eWat[3], expE[3], places=None, msg='energy3Wat: eWat[3]', delta=1e-9)
        self.assertAlmostEqual(eWat[4], expE[4], places=None, msg='energy3Wat: eWat[4]', delta=1e-9)

    # def test_E3P(self):
        # temp = [45, 46, 47, 48, 49]
        # expE = [11117682.8, 11231977.3, 11346271.8, 11460566.3, 11574860.8]
        # ePCM = energy.energy3PCM(temp, self.params)
        # self.assertAlmostEqual(ePCM[0], expE[0], places=None, msg='energy3PCM: ePCM[0]', delta=1e-9)
        # self.assertAlmostEqual(ePCM[1], expE[1], places=None, msg='energy3PCM: ePCM[1]', delta=1e-9)
        # self.assertAlmostEqual(ePCM[2], expE[2], places=None, msg='energy3PCM: ePCM[2]', delta=1e-9)
        # self.assertAlmostEqual(ePCM[3], expE[3], places=None, msg='energy3PCM: ePCM[3]', delta=1e-9)
        # self.assertAlmostEqual(ePCM[4], expE[4], places=None, msg='energy3PCM: ePCM[4]', delta=1e-9)


class EnergySuite(unittest.TestCase):

    def suite(self):
        suite = unittest.TestLoader().loadTestsFromTestCase(TestEnergy)
        return suite