#Commented lines pending removal
from matplotlib import pyplot

def plot(time, tempW, eW, filename):#tempP, eW, eP, filename):
    plotFilename = filename + 'png'
    pyplot.subplot(121)
    pyplot.plot(time, tempW, label='Water')
#    pyplot.plot(time, tempP, label='PCM')
    pyplot.xlabel('Time (seconds)')
    pyplot.ylabel('Temperature (Celsius)')
    pyplot.title('Temperature Profiles')
    pyplot.legend(bbox_to_anchor=(0.95, 0.25))
    pyplot.subplot(122)
    pyplot.plot(time, eW, label='Water')
#    pyplot.plot(time, eP, label='PCM')
    pyplot.xlabel('Time (seconds)')
    pyplot.ylabel('Energy (Joules)')
    pyplot.title('Energy Profiles')
    pyplot.legend(bbox_to_anchor=(0.95, 0.25))
    pyplot.tight_layout()
    pyplot.savefig(plotFilename)
