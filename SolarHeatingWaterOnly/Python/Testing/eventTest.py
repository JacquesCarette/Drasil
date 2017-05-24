import sys
sys.path.insert(0, '.')

import unittest
import PyDSTool
from PyDSTool import args
import event
import load_params


class TestEvent(unittest.TestCase):

    def setUp(self):
        self.params = load_params.load_params('test.in')

    def test_event1a(self):
        pardict = {'Tmelt': self.params.Tmelt}
        icdict = {'Tp': 44.1}
        ode = {'Tp': '0'}

        DSargs = args(name='Event1aTest')
        DSargs.events = [event.event1(self.params)]
        DSargs.pars = pardict
        DSargs.tdata =[0, self.params.tfinal]
        DSargs.algparams = {'init_step': self.params.tstep, 'rtol':self.params.RelTol, 'atol': self.params.AbsTol}
        DSargs.varspecs = ode
        DSargs.ics = icdict
        event1aTest = PyDSTool.Generator.Vode_ODEsystem(DSargs)
        traj = event1aTest.compute('event1aTest')
        evs = traj.getEvents('event_melt_begin')

        assert evs is None

    def test_event1b(self):
        pardict = {'Tmelt': self.params.Tmelt}
        icdict = {'Tp': 44.1}
        ode = {'Tp': '0.1'}

        DSargs = args(name='Event1bTest')
        DSargs.events = [event.event1(self.params)]
        DSargs.pars = pardict
        DSargs.tdata = [0, self.params.tfinal]
        DSargs.algparams = {'init_step': 0.5, 'rtol': self.params.RelTol, 'atol': self.params.AbsTol}
        DSargs.varspecs = ode
        DSargs.ics = icdict
        event1bTest = PyDSTool.Generator.Vode_ODEsystem(DSargs)
        traj = event1bTest.compute('event1bTest')
        evs = traj.getEvents('event_melt_begin')

        self.assertAlmostEqual(evs['t'][-1], 1, places=None, msg='event1b', delta=1e-10)

    def test_event2a(self):
        pardict = {'Hf': self.params.Hf,
                   'Mp': self.params.Mp}
        icdict = {'Qp': 0}
        ode = {'Qp': '0'}

        DSargs = args(name='Event2aTest')
        DSargs.events = [event.event2(self.params)]
        DSargs.pars = pardict
        DSargs.tdata = [0, self.params.tfinal]
        DSargs.algparams = {'init_step': self.params.tstep, 'rtol': self.params.RelTol, 'atol': self.params.AbsTol}
        DSargs.varspecs = ode
        DSargs.ics = icdict
        event2aTest = PyDSTool.Generator.Vode_ODEsystem(DSargs)
        traj = event2aTest.compute('event2aTest')
        evs = traj.getEvents('event_melt_end')

        assert evs is None

    def test_event2b(self):
        pardict = {'Hf': self.params.Hf,
                   'Mp': self.params.Mp}
        icdict = {'Qp': 600000}
        ode = {'Qp': '0'}

        DSargs = args(name='Event2bTest')
        DSargs.events = [event.event2(self.params)]
        DSargs.pars = pardict
        DSargs.tdata = [0, self.params.tfinal]
        DSargs.algparams = {'init_step': self.params.tstep, 'rtol': self.params.RelTol, 'atol': self.params.AbsTol}
        DSargs.varspecs = ode
        DSargs.ics = icdict
        event2bTest = PyDSTool.Generator.Vode_ODEsystem(DSargs)
        traj = event2bTest.compute('event2bTest')
        evs = traj.getEvents('event_melt_end')

        assert evs is None

    def test_event2c(self):
        pardict = {'Hf': self.params.Hf,
                   'Mp': self.params.Mp}
        icdict = {'Qp': 10654059.9}
        ode = {'Qp': '0.1'}

        DSargs = args(name='Event2cTest')
        DSargs.events = [event.event2(self.params)]
        DSargs.pars = pardict
        DSargs.tdata = [0, self.params.tfinal]
        DSargs.algparams = {'init_step': 0.5, 'rtol': self.params.RelTol, 'atol': self.params.AbsTol}
        DSargs.varspecs = ode
        DSargs.ics = icdict
        event2cTest = PyDSTool.Generator.Vode_ODEsystem(DSargs)
        traj = event2cTest.compute('event2cTest')
        evs = traj.getEvents('event_melt_end')

        self.assertAlmostEqual(evs['t'][-1], 1, places=None, msg='event2c', delta=1e-4)


class EventSuite:

    def suite(self):
        suite = unittest.TestLoader().loadTestsFromTestCase(TestEvent)
        return suite