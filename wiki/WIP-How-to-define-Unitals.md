# Purpose of this Wiki
This is a wiki to note a new way to think about symbol `Concepts` to be used in these SRS documents. This is to help with a number of current issues with capturing and rendering the knowledge. See [#91](https://github.com/JacquesCarette/literate-scientific-software/issues/91) or [#265](https://github.com/JacquesCarette/literate-scientific-software/issues/265) for example.

# Capturing Knowledge
We create three groups to capture concepts.
## Three Groups
### Physical Properties
Examples:
- Force
- Weight
- Length
- Number
- Probability
- Factor
- Time
### Physical Object or Events
Examples:
- Water
- Ball
- Tank
- Breakage
- Throw
### Auxiliaries
This is for concepts that cannot fit in the other two categories. Examples:
- Coordinates such as x, y, (x,y,z), (i,j,k)
- (Arbitrary) Constants, functions, variables, or parameters such as y=**m**x + **b**, find **x(t)** where mx''=-kx
- Misc ? such as gradient operator &nabla;
## Putting them together
### Idea property pair
Take a property and combine it with an object or event to create a `UnitalChunk`. Examples;
- Water weight
- Number of slices
- Probability of failure
## Auxiliary Entities
Auxiliaries can become a `UnitalChunk` on their own.
## Modifiers
These can be added to a `UnitalChunk` to create a new `UnitalChunk`. Examples;
- initial / final
- normal / shear
- angular
- difference (think change in temperature or &Delta;x)
- ratio
- vector 
- unit (as in unit vector)
# Examples of use and ideal implementation
We can take **Temperature** and combine it with **Water** and modify it with **Final**. Give temperature the symbol T. Then add the subscript W for water to get T<sub>W</sub>. Then add a second subscript or superscript for final to get T<sup>f</sup><sub>W</sub>.