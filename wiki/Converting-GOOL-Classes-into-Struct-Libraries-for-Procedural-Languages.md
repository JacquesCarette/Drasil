## Explanation
Essentially, converting from GOOL classes into procedural structs means putting the class's data into a struct, and converting the class's methods into functions that take an instance of the struct as their first parameter.  For example, a class that looks like this in Java:
```java
public class Observer {
    public int x;
    
    public Observer() {
        this.x = 5;
    }
    
    public void printNum() {
        System.out.println(this.x);
    }
    
    public int getX() {
        return this.x;
    }
    
    public void setX(int x) {
        this.x = x;
    }
}

Observer obs = new Observer()
```
Might translate to something like this in Julia:
```julia
mutable struct Observer
    x::Integer
end

function Observer()
    return Observer(5) # Observer(x) is auto-generated based on the definition of Observer
end

function setX(self::Observer, x::Integer)
    self.x = x
end

obs = Observer()
```

## Open Questions:
- Is there a way to translate static attributes?