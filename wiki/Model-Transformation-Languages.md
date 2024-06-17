A Model Transformation Language (MTL) provides a language that operates on models to extract information (query), identify errors and inconsistencies (verification), and produce textual artifacts (generation), including code.
With this definition, Drasil can be viewed as an MTL.  We should, therefore, have a better idea of what other MTLs exist and what they are capable of doing.

A list of MTLs is given at:
https://en.wikipedia.org/wiki/Model_transformation_language.
We should look at this list and determine what lessons there are to be learned from others.
The specific question is whether the knowledge in Drasil could be captured in an existing MTL.

***

## What lessons can be learned from others?

## Can the knowledge in Drasil be captured in an existing MTL?

***

# Definition of MTL
* model transformation can be broken down into M2M transformation or M2Text/Code (a.k.a. code generation) (StephanAndStevenson2009)

# Advantages of Using MTLs
* syntax is available to make referencing model elements with more ease (https://en.wikipedia.org/wiki/Model_transformation_language)
* "models are the only elements that change (as the implementing software code is generated and updated automatically)" (StephanAndStevenson2009)
* **SEE** [SendallAndKozaczynski2003](https://gitlab.cas.mcmaster.ca/SEforSC/se4sc/blob/f207fda52604d92928a5535c466f24f0388c84da/SciCompAndSoftEngPapers/SendallAndKozaczynski2003.pdf)

# Review of Existing MTLs
We need some way to compare and contrast the existing tools.
The key question is whether they can capture Drasil style knowledge.
Could we enter an expression in one of these languages and generate documentation and code?

# Research (WIP)

## [StephanAndStevenson2009](https://gitlab.cas.mcmaster.ca/SEforSC/se4sc/blob/2f70a21d01b17288ab6b18f2ae1025d200d58d79/SciCompAndSoftEngPapers/StephanAndStevenson2009.pdf)
Talks about the following 3 popular M2M model transformation languages and compares their work with Ecore models (which are metamodels "at the heart of" the Eclipse Modelling Framework) as a foundation for comparison qualitatively (in terms of expressiveness, ease of use, and modularization):

### **_EMF_**
- a graph-transformation based approach
- work with Ecore models (which is widely used in both industry and research)
- "based on the Query/View/Transformation (QVT) standard set by the Object Management Group"
- "provides an entirely graphical interface to describe in-place transformation rules" (colors play an important role in relay of info)
- Figure 2 is a figure that shows how model elements are structured before and after transformation
- build upon the graph transformation tool AGG
- more intuitive and comprehensive
- "best suited to when the source and target metamodels are the same" though "is possible to write transformation
rules from one metamodel to another"
- "lack of automatic build tools makes running the transformations problematic"
### **_KerMeta_**
- a direct-manipulation approach
- work with Ecore models (which is widely used in both industry and research)
- "based on the Query/View/Transformation (QVT) standard set by the Object Management Group"
- intended for metamodeling
- "allows for navigation and manipulation of a model in an object-oriented way"
- "M2M model transformation is accomplished in KerMeta by using actual instances located within the target model in conjunction with those from the source model"
- powerful, less intuitive, "not designed with transformations in mind"
### _**ATL**_
- hybrid approach
- work with Ecore models (which is widely used in both industry and research)
- "ease of use and expressive power and still allows for modularity" (which "is achieved using helper functions and transformation staging")
- composed of 3 types of rules: lazy (execute only when called by another), matched (automatically called), and called ("do not include a source pattern and may contain optional local variables/parameters")
- 2 types of execution modes: (normal and refined ("used to focus on specific transformations...to transform one source model element into one target model element and focus while leaving the rest implicitly copied" (**RELATED TO SWHS AND NOPCM**?)))
- avoids imperative language "constructs in favor of splitting the complex transformation into two or more simpler transformations that run in sequence"

### _**Other points**_
- it's quite the common occurrence for a "software project wants to change the underlying platform-
independent implementation", which is difficult to do manually; which is how a transformation language or approach to accomplish this is now used
- transformation languages should maximize ease of allowing writer "to have a mental map of both the source and target meta models simultaneously and visualize the transition from one to the other"
- "A tool that supports the choice to execute transformations recursively would be more expressive than a language that forces one option in all cases."
- Concluding point: "Seeing as ATL is being integrated into official Eclipse's M2M project stream, we believe that it will eventually establish itself as a prevalent approach to perform M2M transformations"

## [Beanbag](http://sei.pku.edu.cn/~xiongyf04/papers/SES09.pdf) **ADD TO SE4SC REPO?**
"A Language for Automatic Model Inconsistency Fixing"
- "A program in Beanbag mainly defines a consistency relation, but also has a fixing semantics defining how to propagate updates to fix an inconsistency"
- Similar to Drasil in terms of consistency goals when updating models, as the paper recognizes that "manually implementing such fixing behavior is time-consuming and error-prone"**EXPAND**
- **LOOK INTO** heterogeneous data synchronization **AS A RELATED TOPIC**

## [Graph Rewriting and Transformation language (GReAT) - Agrawal2003](https://pdfs.semanticscholar.org/8acf/8cb63c4bb5a6d08475cdba8406ac57c620cb.pdf)
- use of model-driven architecture for a particular domain "helps developers to represent their systems using familiar domain concepts"; "high productivity... users are familiar with the use of modeling"
- transformers are needed to convert Domain Specific Platform Independent Model (DSPIM) to Domain Specific Platform Specific Model (DSPDM)
- Model Integrated Computing (MIC) benefits = high productivity
- Model Integrated Computing (MIC) drawbacks = high development cost, lack of standardization, lack of vendor review
- GReAT "can lead to standardization that will allow vendors to support various domain-specific modeling environments within the framework"
- model transformer designs are non-trivial as they "can work with arbitrarily different domains and can perform fairly complex computations"
1. As a pattern specification language (pattern matching, graph grammars)
2. As a graph transformation language 
    - "In model-interpreters, structural integrity is a bigger concern because model-to-model transformations usually transform models from one domain to models that conform to another domain."
    - Causes 2 main problems: specifying/maintaining 2 different models conforming to 2 different meta-models and maintaining references between 2 models
    - "It is important to maintain some sort of reference, link, and other intermediate values. These are required to correlate graph objects across the two domains." **(DRASIL'S FOCUS ON TRACEABILITY AIM; SWHS to NOPCM EXAMPLE?)**
    - solution = "to use the source and destination meta-models to explicitly specify the temporary vertices and edges" --> a unified meta-model that can be treated as a single graph
3. As a control flow language ("control the application of the productions and allow the user to manage the complexity of the transformation"; features include sequencing, non-determinism, hierarchy, recursion, test/case)

## [UML-RSDS](https://nms.kcl.ac.uk/kevin.lano/uml2web/) 
- "solves the long-standing problem of how to combine declarative high-level specification of model transformations and general software systems, with efficient execution"
- users write their specifications in OCL and class diagrams --> automatically generates efficient Java code from these specifications
- "The tool can be used to quickly sketch designs in UML and immediately generate working code - even for incomplete models"
- "It can also be used to quickly produce prototypes or test scripts."
