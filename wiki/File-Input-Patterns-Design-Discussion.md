## Purpose
It has become clear that there are issues with our current patterns for reading input parameters from the file.  This page seeks to analyze the patterns we currently have, and to propose improvements.

Note that I have implemented case studies for the designs listed below.  They can be found [here](https://github.com/smiths/caseStudies/tree/FileInputTests/CaseStudies/FileInputTests).

## What We Have
Currently, the `Choices` construct has two options for `inputStructure`, which defines how input is stored: `Bundled` and `Unbundled`.

### Unbundled Analysis
#### Current:
- There is no input object holding all the input parameters; instead each input parameter has its own variable within the main function.
- Instead of having a constructor that returns an object, `InputParameters` has a static method that reads in the data and returns it sequentially.
	- In Java, it returns an array of objects, which then has to be destructured.
	- In Python and swift, it returns a tuple, which can be destructured using syntactic sugar.
	- In C++ and C#, it mutates arguments that are passed to the method.
- I don't really like this, as it requires users of the API to know the arbitrary ordering of the returned values.  It's somewhat OK in Python and swift as Tuples are designed for this sort of thing and their syntactic sugar makes it clearer, but the other targets feel a bit hacky.
- Examples: Double Pendulum, PD Controller, some versions of Projectile, and Solar Water Heater
#### Potential Changes:
###### Design 1: Leave it
- It is functional enough as is, and in some respects simple might be the best.
###### Design 2: Use hashmaps
- Instead of using a linear data structure to return the values, use a key/value pair data structure.
- Pros:
	- Structure of returned object is more clear.
- Cons:
	- Less type safety than Tuples.
	- No enforced structure of what keys will be returned.
###### Design 3: Use structs
- Similar to Design 2, but clearly state the structure and type of the returned data.
- Pros:
	- Type safety and structural safety.
- Cons:
	- Requires that a new struct be defined.
	- A struct needs to be created, then used exactly once.
###### Design 4: Get rid of Unbundled
- I'm not sure how many cases there are where it would actually be useful to use unbundled.
- I guess if there's only a few variables, it would be nice to have an unqualified name, but that's hardly the end of the world.
- Also, the code's being generated, so a qualified name won't take longer to write, and arguably makes it easier to understand.

I didn't see this till after, but this [page](https://www.baeldung.com/java-method-return-multiple-values) summarizes the first three options.

### Bundled Analysis
#### Current:
- There's a special input object to hold the input parameters.
- Its constructor doesn't set its members directly, but calls two of its methods:
	- `get_input`, which sets its members by reading and parsing a file.
	- `input_constraints`, which validates its members based on given constraints.
- Honestly, I don't hate the design.  It's modular and keeps the constructor simple.
- The issue is, Swift wants all members to be defined in the constructor, and requires defaults even if the constructor calls methods to set these values.  Procedural languages are likely to have similar issues.
- Examples: some versions of Projectile.
###### Special Case: GlassBR
- The GlassBR is bundled, but it has some weird behaviour:
	- Its constructor is empty; its members are undefined at initialization.
	- The main program passes a reference of it to another class's method, which defines the object's members.
- This isn't great, as we want to avoid having undefined members outside the constructor, even for just a second.
- We shouldn't be allowing behaviour like this.  No matter what, we want the class's members to be defined by the time the constructor is finished.
#### Potential Changes
###### Forward:
From my research and thinking, there are 3 main questions we need to ask:
1. Do we allow `this` method calls in the constructor before all class members are defined?
	- E.g. calling a file parser method which defines the class's members.
	- Probably not: Swift requires defaults if we do that.
2. Do we allow `this` method calls in the constructor after all class members have been defined?
	- E.g. input verifiers.
	- From my research, this is acceptable enough.  The AI chatbots I was talking with noted that while there are tradeoffs, keeping the data in the same class as the constraints on the data makes sense.
3. Do we want the constructor to do the 'rough work' of finding values for its members, or should we have a function that finds the values, then calls the constructor on them?
	- E.g. do we want the initializer to parse the input and then set its variables, or do we want a function that parses the input to get the variables, then passes them to the constructor?
	- If the initializer does all the work, then we need to either call a method or do all the work inside the function.  Calling methods to set members is not ideal, as discussed; and putting all the code inside the constructor can lead to large constructors.
	- Putting all the hard work in another function increases the complexity of the program, but it simplifies the individual pieces.  It also decouples the class from the input method, which would make alternate input methods easier to implement.
###### Design 1: Leave Bundled as-is
See discussion above.
###### Design 2: Bundled with external parsing and internal validation
- Only allow calls to `this` methods *after* all class members have been defined.
- Values for class members can be calculated by an external function that would then call the constructor (as in Design 3).  We could also calculate the values within the constructor, but I like the modularity of this way.
- Pros:
	- Using an external function for parsing decouples the class from the input method.
	- Keeping validation internal to the class makes that code reusable (e.g. if we had multiple functions that call the constructor).
	- The constructor is simple.
- Cons:
	- Requires an external function to parse the input and call the constructor.
###### Design 3: Bundled with external parsing and validation
- Do not allow any kind of `this` method calls within constructors.  This will encourage constructors to be as simple as possible.
- Instead of the constructor reading and parsing the file, have another function that parses the file to get the values for the class members, then calls the constructor on those values.  Validation would need to occur either within the external function, or be a method of the class that is called after the constructor is finished.
- Pros:
	- Keeps the constructor simple, since the code for 'deriving' class member definitions is in another function.  This is also the only solution where the constructor's sole responsibility is to create the object.
	- Decouples the class from its input format.
- Cons:
	- Less flexible constructors - e.g. validation has to be performed externally or after the constructor is finished.
	- Requires an extra function for parsing and calling the initializer.
###### Design 4: No Modularity
- Just put the code for `get_input` and `input_constraints` inside the constructor.
- That is, `this` method calls are not allowed, and we choose not have external functions that return new instances of the class.
- Pros:
	- Makes sure that all of the class's members are defined in its constructor.
	- Should be easy to translate to procedural languages.
- Cons:
	- Loses out on modularity.  It would lead to bloated constructors and a tight coupling between the class and the class's input method.
#### Discussion of Secrets/Information Hiding
[@smiths](https://github.com/smiths) asked that I add a discussion of David Parnas' Secrets to this document.  I can think of the following secrets for this problem:
- Input method (file, console, etc.)
- File format (custom txt, JSON, XML, etc.)
- Constraints on parameters
- Internal representation of the parameters.
- The parameters themselves could be thought of as a secret, but they are unlikely to change, and most likely need to be shared between modules.

In Design 4, all of the secrets are kept by the constructor.  In Designs 1-3, the secrets are split up the following way:
- The input method and file format are kept secret by the parsing function/method.
- The constraints on parameters are kept secret by the validation function/method.
	- Although, code further on is likely dependent on the constraints, so I suppose they aren't fully kept secret.
- The internal representation of the parameters and the parameters themselves are stored in the class and its constructor.
	- Because the parameters are to be accessed directly rather than through getters, their representation is not secret within the module.

Based on these, I can see two minor improvements we could choose to make:
- Right now, input method and file structure are both secrets held by the parsing method.  This is probably fine, but we could split them up by using a factory pattern or something similar.
- Because the parameters are accessed by directly accessing the class's members, their internal representation is not kept secret.  We could improve this by using getters instead.
- These are more minor nitpicks in my opinion; they would make the code slightly easier to extend and modify, but it seems like small gains at the cost of a fair bit of complexity.

#### Summary
I like Design 2 the best.
- While input parsing could be part of the class, it definitely doesn't need to be, and decoupling it from the class opens the door to using the same class with different input methods (e.g. file vs command-line).
- I also think that constraint validation should be a part of the class, since the constraints are not coupled to the input method.
	- There might be some validation that needs to be different (e.g. sanitizing strings based on how trustworthy their source is), but the constraints on the variables themselves (e.g. nonnegativity) will be the same.

## Language-Specific Idioms
Most of these notes are gained from conversations I had with Meta's AI and with Microsoft Copilot.  Note that while they're both pretty good at writing and explaining code, they're not very consistent with what they say, so take this section with a grain of salt.
#### Java, C\#
These languages have strong foundations in Design Patterns, and this definitely shows up here.  Particularly, the two terms that I heard a lot were Factory Pattern and Single Responsibility Principle.
- With the Factory Pattern, you would have an external parser implementing the Factory interface.  Validation can go wherever suits you - either in the constructor, in the parser, or in its own function.
- Following the Single Responsibility Principle, parsing, validation, and object creation must all be in their own functions.  Some of these functions could potentially call each other, but they at least have to be separate pieces of code.
#### Swift
Swift has 'failable initializers', which are allowed to return `nil` if something goes wrong.  Because of this, it is far more common to have parsing and validation inside the constructor of a Swift class/struct than in other programming languages.  Despite this, it was about 50/50 if the chatbot recommended putting the parsing logic within the initializer.
#### Julia
With Julia, the chatbot usually ended up agreeing that parsing should not be done inside the constructor (since if parsing fails, you have no choice but to throw an error), and that validation should be done inside the constructor (since Julia's inner constructors are designed for this very purpose).

## Note on Implementation
One of the purposes of more clearly defining the structure of and constraints on constructors was to aid in designing translation from GOOL into procedural languages.  Here I'm commenting on any potential issues we might run into while implementing a procedural translator for each of the designs.
#### Unbundled
- Any of the designs proposed above could easily be translated from GOOL to a procedural language.
#### Bundled
###### Design 1
- Design 1 would be very difficult to implement for procedural languages in GOOL.
- The issue is, if we want a method to define a class's members, then we need to call it before we have an instance to call it on.
- We would likely need to create a new type of method that is the only kind that can be called by the constructor, and can only be called by the constructor.
- It would need to clearly state which class members it uses, and which class members it defines.
- Then, instead of just reading and setting them directly on the instance as it would in an OO language, it could take in the members it requires, and return a tuple or dictionary of the members it sets.
- There are other problems with Design 1, but the complexity involved with implementing it is on its own enough to make me strongly advise against this option.
###### Design 2
- We would need to add a field to the `constructor` function in GOOL denoting code that occurs *after* all class members have been defined, so that GOOL knows it can safely instantiate the struct before that code in procedural languages.
- This shouldn't be too much work, though.
###### Design 3
- Because of the tighter restrictions, I don't see any major issues making this work for procedural languages.
###### Design 4
- Because of the tighter restrictions, I don't see any major issues making this work for procedural languages.