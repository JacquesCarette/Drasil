var n: Int
var myFSM: String = "Off"
myFSM = "On"
switch myFSM {
    case "Off":
        print("Off")
    case "On":
        print("On")
    default:
        print("Neither")
};

print("myStrat")
n = 3

var obs1: Observer = Observer()
var obs2: Observer = Observer()

var observerList: [Observer] = [obs1]
observerList.insert(obs2, at: observerList.count)
for observerIndex in [Int](stride(from: 0, to: observerList.count, by: 1)) {
    observerList[observerIndex].printNum()
}

obs1.setX(10)
print(obs1.getX(), terminator: "")
