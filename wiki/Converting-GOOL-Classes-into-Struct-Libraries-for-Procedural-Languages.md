## Overview
Essentially, converting from GOOL classes into procedural structs means putting the class's data into a struct, and converting the class's methods into functions that take an instance of the struct as their first parameter.  For example, a class that looks like this in Java:
```java
public class Observer {
    public int x;
    
    public Observer() {
        this.x = 5;
    }
    
    public int getX() {
        return this.x;
    }
}

Observer obs = new Observer()
```
Might translate to something like this in Julia:
```julia
mutable struct Observer
    x::Integer

	function Observer()
	    return new(5)
	end
end

function getX(self::Observer, x::Integer)
    self.x = x
end

obs = Observer()
```

## Details
#### Class members
Class members (aka variables) can cleanly be translated into struct members, without any complications.
#### Methods
Because we're using structs instead of classes, we don't have methods.  Instead, we use functions that take an instance of the struct as their first parameter.  This is in line with many existing libraries.  For example, in Python to append an item to a list you use:
```python
myList.append(3)
```
but in Julia you use:
```julia
append!(myList, 3)
```
#### Constructors
Constructors are going to be a bit weird in procedural languages.  
- The reason for this is that in a class constructor, we right away have the object being created, and we can call methods on it.  In procedural languages the constructor is a function, and we don't have the instance of the struct until we create it.
- This means that in the most basic implementation, we cannot call methods inside constructors in procedural languages.
- We can likely modify GOOL to allow some method calls:
	- We have decided that in all targets, we're going to avoid default and undefined values when dealing with constructors.  This means that we cannot create the instance of the struct until we have all of the values to give it, which in turn means that we cannot use method calls to set a struct's values.
	- But we can set it up so that we can specify method calls that happen after all of a struct's values have been defined, and then we can call those after creating the instance of the struct.

So there's three levels of functionality:
###### Everything done through function calls (not method calls):
Java:
```java
public class Input {
	public ArrayList<Integer> data;

	public Input(String filepath) {
		this.data = parse(filepath) // function call
		validate(this.data) // function call
	}
}
```
Easily translates to Julia:
```julia
struct Input
	data::Array{Integer}

	function Input(filepath::String)
		data = parse(filepath)
		validate(data)
		return new(data)
	end
end
```
###### Method calls only allowed after struct creation:
Java:
```java
public class Input {
	public ArrayList<Integer> data;

	public Input(String filepath) {
		this.data = parse(filepath) // function call
		this.validate() // method call
	}
}
```
Could feasibly translate to Julia:
```julia
struct Input
	data::Array{Integer}

	function Input(filepath::String)
		data = parse(filepath)
		s = new(data)
		validate(s)
		return s
	end
end
```
This would require (likely small) changes to GOOL, but I think it will be worth doing.
###### Unrestricted method calls:
Java:
```java
public class Input {
	public ArrayList<Integer> data;

	public Input(String filepath) {
		this.parse(filepath) // method call that sets this.data
		this.validate() // method call
	}
}
```
In theory, this *could* translate to the following Julia:
```julia
struct Input
	data::Array{Integer}

	function Input(filepath::String)
		data = parse(filepath)
		s = new(data)
		validate(s)
		return s
	end
end
```
But that would require telling GOOL a *lot* of extra information about `parse`, and I don't think that it's a worthwhile change.
#### Static members/methods
I haven't put much thought into these as of yet.  I don't think I'll prioritize implementing them for procedural languages until we've gotten further.  There may be a way of translating them into module-level variables and functions, at least in some targets.