# Generate softifacts
In the Drasil, we call `genCode` to generate [softifacts](https://gitlab.cas.mcmaster.ca/SEforSC/se4sc/-/wikis/Advice-and-Checklists-for-Repos-(including-a-list-of-recommended-artifacts)), including executable code, documentation, logging file, and other auxiliary files. The `genCode` takes two parameters `Choices` and `CodeSpec`, and output a Haskell IO type.

start with genCode
```haskell
genCode :: Choices -> CodeSpec -> IO ()
```
## What is Choices? 
In short, `Choices` is a data type to indicate how the generated program should be written down. They are choices a user needs to make to tell Drasil what features and style the program will be. The word "features and style" may be too broad, because there is numerous way to write a program. At least, this gives us some ideas about the role `Choices` serves. 

Here is  the feature diagram describing the **hierarchical structure** of `Choices`
<img width="1200" src="https://user-images.githubusercontent.com/16947266/158629471-cc98127c-19cf-4a98-8d16-ecd26aff1230.png">

Here is the full detail of the `Choices` data type
1. lang: a decision on what language will be generated.
    - Cpp
    - CSharp
    - Java
    - Python
    - Swift
    
2. architecture: the architecture of a program
    - modularity: a decision on how to modularize the generated code.
      - Modular: a decision on making generated code in different modules(controller, input, calculations, and output). Currently, we only have choices for the input modular.
        - Combined: all input functions will be combined in one module/file.
        - Separated: all input functions will have their own module/file. eg, InputConstraints.py and InputFormat.py are in their own file.
      - Unmodular - decision on making generated code in one module/file.
        - default the input modular to Combined.
    - impType: a decision on whether to generate a controller file. (Usually, we call it the main file)
      - Library: does not generate a controller component.
      - Program: generate a controller component.

3. dataInfo: how information should be encoded.
    - inputStructure: a decision on input the structure of variables  
      - Unbundled: each input variable was directly passed to the function as its own parameter. 
      - Bundled: all input variables stored in a separated object.
    - constStructure: a decision on the structure of constants
      - Inline: macro expansion on variables. All constants that are represented by variables will be replaced by their value.
      - WithInputs: constants are stored in variables within the input object 
      - Store Unbundled: constants are stored in variables that were directly passed to the function as its own parameter. 
      - Store Bundled: constants are stored in variables within a separated object
    - constRepr: the representation of constants
      - Var: all constants represented as regular variables.
      - Const: use selected language's mechanism for defining constants. For example, C# has a build-in constant called `const`, but not all languages have build-in support on constants.

4. maps: matching database for Concepts and Space 
    - conceptMatch: specifies matches between chunks and `CodeConcept`s, meaning the target language's pre-existing definition of the concept should be used instead of defining a new variable for the concept in the generated code. (directly from code)
    - spaceMatch: specify which mathematical spaces should be matched to which type in the code.

5. optFeats: they are softifacts that can be added to the program or left it out
    - docConfig: configuration on Doxygen documentation.
      - comments: Doxygen comments setting
        - CommentFunc: comments in function/method-level
        - CommentClass: comments in class level
        - CommentMod: comments in file/module level
      - doxVerbosity: choose a type of documentation
        - Verbose: full Doxygen documentation
        - Quiet: brief Doxygen documentation (default in GitHub Drasil host)
      - dates:
        - Show: show date field in module-level Doxygen comments.
        - Hide: hide date field in module-level Doxygen comments.
    - logConfig: configuration on logging setting.
      - logging: logging message setting. (most of them are printing messages on the console)
        - LogFunc: log messages for function calls.
        - LogVar: log messages for variable assignments.
      - logFile: FilePath - the name/path of logging file
    - auxFiles: auxiliary files
      - SampleInput FilePath: a sample input file compatible with the generated program. SampleInput is parameterized by a FilePath, it is a path of a file that contains the sample data which will be written to the sample input file.
      - ReadME: geneate a ReadME.md file

6. srsConstraints: SRS constraints
    - onSfwrConstraint: constraints about the idealized model, once the parameter falls out of constraint, there is no guarantee the result is valid.
      - Warning
      - Exception
    - onPhysConstraint: constraint in the physics world. eg. the mass cannot be negative
      - Warning
      - Exception

7. extLibs: External Libraries
    - Math ODE: All Information needed to solve an ODE (ODE eventually need be removed from `Choices`)
      - odeInfo: a description of an ODE that needs to be solved.
        - indepVar: CodeVarChunk
        - depVar: CodeVarChunk
        - otherVars: [CodeVarChunk]
        - tInit: CodeExpr
        - tFinal: CodeExpr
        - initVal: [CodeExpr]
        - odeSyst: [CodeExpr],
        - ODEOptions: ODEOptions
      - odeLib: choices of to connect external ODE library
        - libName: Name
        - libVers: Version
        - libSpec: ExternalLibrary
        - libCall: ODEInfo -> ExternalLibraryCall
        - libPath: Maybe FilePath
        - compatibleLangs: [Lang]

## What is CodeSpec
the `codeSpec` is “code specification” stores the information needed by the code generator about the scientific problem being solved, in a format that is useful to the generator. 

```haskell
codeSpec :: SystemInformation -> Choices -> [Mod] -> CodeSpec
```

The codeSpec takes SystemInformation, Choices, [Mod] as parameters and returns the CodeSpec type.

##### SystemInformation
the giant system information collects a multitude of pieces of information, whenever we need it, we extract the information from the system information.

##### Mod
It holds module information. if their program requires modules that Drasil can not yet generate automatically, the user can specify those modules themselves. For example,
- ReadTable Module: Reads glass ASTM data from a file with the given filename
- Interpolation Module: provides functions for linear interpolation on three-dimensional data
