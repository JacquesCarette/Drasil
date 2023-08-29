import Observer

myFSM = "Off"
myFSM = "On"
if (myFSM == "Off"):
    print("Off")
elif (myFSM == "On"):
    print("On")
else:
    print("Neither")

print("myStrat")
n = 3

obs1 = Observer.Observer()
obs2 = Observer.Observer()

observerList = [obs1]
observerList.insert(len(observerList), obs2)
for observerIndex in range(0, len(observerList), 1):
    observerList[observerIndex].printNum()

obs1.setX(10)
print(obs1.getX(), end="")
