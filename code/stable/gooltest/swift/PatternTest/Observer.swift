/** This is an arbitrary class acting as an Observer
*/
class Observer {
    var x: Int = 0
    
    init() {
        self.x = 5
    }
    
    func printNum() -> Void {
        print(self.x)
    }
    
    func getX() -> Int {
        return self.x
    }
    
    func setX(_ x: Int) -> Void {
        self.x = x
    }
}
