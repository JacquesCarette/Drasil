# Chapter "Take Homes" and Outlines

1. Introduction:

    Take home: A single universal mathematical expression language is
    insufficient to describe theories and all of their meta-level information.

    Outline: Terse introduction to Drasil and Drasils ideology, followed by a
    discussion of the immediate issues associated with this research (encoding
    theories with expressions, need for typing rules, and need for term
    restriction to particular domains [i.e., CodeExpr/Expr/ModelExpr]).
	
2. Ideology:

    Take home: Software artifacts are the result of the communication of product
    owners. The quality of their communication will be a direct reflection of
    the quality of the created software artifacts.

    Outline: Discussion of communication and logistical issues associated with
    modern software development, followed by a discussion of an idealized
    workflow for product owners (i.e., using technical/formal languages to
    describe each component, including how those languages can be converted into
    softifacts).

3. Drasil:

    Take home: We can generalize and generate families of software artifacts by
    capturing and formalizing knowledge, using domain-specific languages to make
    them feel "natural."
    
    Alternative take home: Formalizing knowledge (captured in the form of a
    series of domain-specific languages) and their relationships allows us to
    form families of software artifacts, from which a user can create specific
    artifacts by making choices.

    Outline: General introduction to Drasils origins, research team, area, and
    goals, development methodology, architecture, and current issues.

4. Framing Theories:

    Take home: Since humans can infer knowledge from small "views" of a larger
    picture, transferring equational knowledge using equations is possible
    amongst those familiar with the specific "views". For example, transferring
    `y = m * x + b` is easy for us to recognize as the equation of a line, but
    changing the variables or the form (e.g., using point-slope form, intercept
    form, etc.) will make it more difficult. To formalize the translation of
    specific kinds of theories into various forms (e.g., different forms,
    teaching material, solving software, etc.), the implicit information, which
    we would infer from theories, needs to become explicit. Through making the
    implicit information, explicit, we are able to formalize more relationships
    between theories and other things (such as views and uses of them).

    Outline: Discussing the issue of theories encoded as expressions requiring
    too much inference (which, naturally, a computer cannot make without
    external information), and how we can make the required implicit information
    (that which is required for inference), explicit information.

5. Typing Expressions:

    Take home: Expression formation has specific rules we follow, or else the
    expression we write are invalid/incomprehensible (and these validity rules
    also apply to the programming language artifacts we generate). To ease the
    cognitive stress of writing valid/comprehensible expressions, we can create
    validation checks and make them static. In doing this, we will know which
    expressions are valid or not, before attempting to use them for code
    generation.

    Outline: Discussing the issue of stress associated with writing expressions
    without any sort of internal validation before external usage. Code
    generation requires expressions be well-formed, we can ease the cognitive
    stress associated with this by adding some sort of type checking mechanism.
    Along these lines, we will be discussing judgment rules and the syntax rules
    we will form.

6. Knowledge Management:

    Take home: In working with many domains of knowledge, we naturally work with
    many types of knowledge (chunks). As we continue to teach Drasil about each
    domain, the globe of types expands. However, the existing method of storing
    these chunks only allows for a limited number of types to be recorded
    (ChunkDB). To remedy this, we may ignore the type of stored chunks and rely
    on typed references that can interpret the chunks as originally needed.

    Outline: The majority of this chapter will be about the "take-home" message,
    and then continue to mention some residual issues: What makes up a chunk?
    What is a UID? How should information be encoded in Drasil?

7. Future Work:

    Take home: There are remaining issues from the previous chapters and more
    knowledge we may encode. The depth and breadth of domain knowledge covered
    can be generally improved, but we can also work towards what has been
    embedded in Haskell.

    Outline: N/A.

8. Conclusion:

    Take home & Outline: _I imagine the take-home message of the "Conclusion"
    chapter would be a re-statement of the "take home" of the "Introduction."
    The outline will likely need to be based on everything else._
