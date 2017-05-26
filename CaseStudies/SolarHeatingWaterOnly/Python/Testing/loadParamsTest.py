import sys
sys.path.insert(0, '.')

import unittest
import load_params


class TestLoadParams(unittest.TestCase):

    def setUp(self):
        self.params = load_params.load_params('test.in')

    def test_params_L(self):
        self.assertEqual(self.params.L, 1.5, 'L')

    def test_params_diam(self):
        self.assertEqual(self.params.diam, 0.412, 'diam')

    def test_params_Vp(self):
        self.assertEqual(self.params.Vp, 0.05, 'Vp')

    def test_params_Ap(self):
        self.assertEqual(self.params.Ap, 1.2, 'Ap')

    def test_params_rho_p(self):
        self.assertEqual(self.params.rho_p, 1007, 'rho_p')

    def test_params_Tmelt(self):
        self.assertEqual(self.params.Tmelt, 44.2, 'Tmelt')

    def test_params_C_ps(self):
        self.assertEqual(self.params.C_ps, 1760, 'C_ps')

    def test_params_C_pl(self):
        self.assertEqual(self.params.C_pl, 2270, 'C_pl')

    def test_params_Hf(self):
        self.assertEqual(self.params.Hf, 211600, 'Hf')

    def test_params_Ac(self):
        self.assertEqual(self.params.Ac, 0.12, 'Ac')

    def test_params_Tc(self):
        self.assertEqual(self.params.Tc, 50.0, 'Tc')

    def test_params_rho_w(self):
        self.assertEqual(self.params.rho_w, 1000.0, 'rho_w')

    def test_params_C_w(self):
        self.assertEqual(self.params.C_w, 4186, 'C_w')

    def test_params_hc(self):
        self.assertEqual(self.params.hc, 1000.0, 'hc')

    def test_params_hp(self):
        self.assertEqual(self.params.hp, 1000.0, 'hp')

    def test_params_Tinit(self):
        self.assertEqual(self.params.Tinit, 40.0, 'Tinit')

    def test_params_tstep(self):
        self.assertEqual(self.params.tstep, 10.0, 'tstep')

    def test_params_tfinal(self):
        self.assertEqual(self.params.tfinal, 50000, 'tfinal')

    def test_params_AbsTol(self):
        self.assertEqual(self.params.AbsTol, 1e-10, 'AbsTol')

    def test_params_RelTol(self):
        self.assertEqual(self.params.RelTol, 1e-10, 'RelTol')

    def test_params_ConsTol(self):
        self.assertEqual(self.params.ConsTol, 1e-3, 'ConsTol')

    def test_params_Vt(self):
        self.assertAlmostEqual(self.params.Vt, 0.1999749387716046988011011019127, places=None, msg='Vt', delta=1e-10)

    def test_params_Mw(self):
        self.assertAlmostEqual(self.params.Mw, 149.97493877160469880110110191927, places=None, msg='Mw', delta=1e-10)

    def test_params_tau_w(self):
        self.assertAlmostEqual(self.params.tau_w, 5231.6257808161439098450767719507, places=None, msg='tau_w',
                               delta=1e-10)

    def test_params_eta(self):
        self.assertAlmostEqual(self.params.eta, 10.0, places=None, msg='eta', delta=1e-10)

    def test_params_Mp(self):
        self.assertAlmostEqual(self.params.Mp, 50.35, places=None, msg='Mp', delta=1e-10)

    def test_params_tau_ps(self):
        self.assertAlmostEqual(self.params.tau_ps, 73.846666666666666666666666666667, places=None, msg='tau_ps',
                               delta=1e-10)

    def test_params_tau_pl(self):
        self.assertAlmostEqual(self.params.tau_pl, 95.245416666666666666666666666667, places=None, msg='tau_pl',
                               delta=1e-10)

    def test_params_Epmelt_init(self):
        self.assertAlmostEqual(self.params.Epmelt_init, 372187.2, places=None, msg='Epmelt_init', delta=1e-9)

    def test_params_Ep_melt3(self):
        self.assertAlmostEqual(self.params.Ep_melt3, 10654060.0, places=None, msg='Ep_melt3', delta=1e-10)

    def test_params_Mw_noPCM(self):
        self.assertAlmostEqual(self.params.Mw_noPCM, 199.97493877160469880110110191927, places=None, msg='Mw_noPCM',
                               delta=1e-10)

    def test_params_tau_w_noPCM(self):
        self.assertAlmostEqual(self.params.tau_w_noPCM, 6975.7924474828105765117434386173, places=None,
                               msg='tau_w_noPCM', delta=1e-10)


class LoadParamsSuite:

    def suite(self):
        suite = unittest.TestLoader().loadTestsFromTestCase(TestLoadParams)
        return suite
