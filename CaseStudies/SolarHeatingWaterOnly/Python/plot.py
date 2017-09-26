from matplotlib import pyplot

def plot(time, tempW, eW, filename):
    plotFilename = filename + 'png'
    pyplot.subplot(121)
    pyplot.plot(time, tempW, label='Water')
    pyplot.xlabel('Time (seconds)')
    pyplot.ylabel('Temperature (Celsius)')
    pyplot.title('Temperature Profiles')
    pyplot.legend(bbox_to_anchor=(0.95, 0.25))
    pyplot.subplot(122)
    pyplot.plot(time, eW, label='Water')
    pyplot.xlabel('Time (seconds)')
    pyplot.ylabel('Energy (Joules)')
    pyplot.title('Energy Profiles')
    pyplot.legend(bbox_to_anchor=(0.95, 0.25))
    pyplot.tight_layout()
    pyplot.savefig(plotFilename)
