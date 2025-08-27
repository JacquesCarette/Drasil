## \brief This is an arbitrary class acting as an Observer
class Observer:
    def __init__(self):
        self.x = 5
    
    def printNum(self):
        print(self.x)
    
    def getX(self):
        return self.x
    
    def setX(self, x):
        self.x = x
