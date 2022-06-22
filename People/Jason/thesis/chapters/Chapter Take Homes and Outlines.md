# Chapter “Take-Homes” and Outlines

1. Introduction:

    Take home: A single universal mathematical expression language is
    insufficient to describe theories and all of their meta-level information.
    Additionally, we must add type information to languages so that we can make
    invalid expressions irrepresentable.

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
    development methodology.

    A “product owner” is any person involved with the creation of some software
    product. For example, a product owner might be the licensed “owners” of the
    software, the communicators of the requirements of the software, the authors
    that convert the requirements into software, or developers of shared
    libraries.

3. Drasil:

    Take home: Drasil is an exploration of the ideology discussed in Chapter 2.
    The goal of Drasil is to generalize and generate families of software
    artifacts by capturing and formalizing knowledge using domain-specific
    languages. Drasil generates scientific software that solves scientific
    problems laid out in a formal software requirements specification manual.
    
    Outline: General introduction to Drasils origins, research area, and goals,
    development methodology, architecture, and current issues.

4. Framing Theories:

    Take home: Explanation and rationalization of the content of theories and
    their relationship to other aspects of Drasil.

    Outline: Discussing the issue of theories encoded as expressions requiring
    too much inference (which, naturally, a computer cannot make without
    external information), and how we can make the required implicit information
    (that which is required for inference), explicit information.

5. Typing Expressions:

    Take home: Untyped expressions cause problems too late in the development
    process, so now they are typed.
    
    Outline: Discussion of type systems, the current issues faced, and how we
    can type the relevant expression language in Drasil.

6. 'Storing All The Knowledge':

    Take home: Solve the scalability problem of dozens of tables by using a
    “dynamically typed” approach and have a single database.

    Outline: The majority of this chapter will be about the “take-home” message,
    and then continue to mention some residual issues: What makes up a chunk?
    What is a UID? How should we encode information in Drasil?

7. Future Work:

    Take home: 
      - ModelKinds: the models can continue to be examined, and more relationships
        between them and 'code' can be made, so that they can find their way into
        the generated source code. 
          
      - Typing: more terms can be added, typed representations of external
        functions and variables can be added, and numeric expressions lack
        information about dimension and units.
  
      - ChunkDB: UIDs and the Chunk types themselves are left largely as
        'unknowns'.

    Outline: N/A.

8. Conclusion:

   Take home: Re-statement of achievements:
     - ModelKinds implementation
     - Typing the expression language
     - Enabled dynamic chunk registration, and typed UID references
