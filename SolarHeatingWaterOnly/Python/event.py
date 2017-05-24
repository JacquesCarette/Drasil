import PyDSTool


def event1(params):

    event_args = {'name': 'event_melt_begin',
                  'eventtol': params.RelTol,
                  'active': True,
                  'term': True}

    event_melt_begin = PyDSTool.makeZeroCrossEvent('Tmelt - Tp', 0, event_args, varnames=['Tp'], parnames=['Tmelt'])

    return event_melt_begin


def event2(params):

    event_args = {'name': 'event_melt_end',
                  'eventtol': params.RelTol,
                  'active': True,
                  'term': True}

    event_melt_end = PyDSTool.makeZeroCrossEvent('Qp / (Hf * Mp) - 1', 0, event_args, varnames=['Qp'], parnames=['Hf', 'Mp'])

    return event_melt_end
