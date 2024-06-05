# Drasil
Welcome to the Drasil wiki! For more information about Drasil, check out our [project website](https://jacquescarette.github.io/Drasil/) or continue reading our [introduction to Drasil](#what-is-drasil-cont).

## Table of Contents
- [Getting Started with Drasil](#getting-started-with-drasil)
- [Contributing to Drasil](#contributing-to-drasil)
- [Building with Drasil](#building-with-drasil)
- [What is Drasil? (cont.)](#what-is-drasil-cont)
- [Issues we would like to return to](#issues-we-would-like-to-return-to)

## Getting Started with Drasil
Want to see what Drasil can do? Check out the [Quick Start Guide](https://github.com/JacquesCarette/Drasil#quick-start), or navigate to `Drasil/code` and use `make help` for a list of available commands!

To get the full features of Drasil, check out the instructions listed in the [New Workspace Setup](New-Workspace-Setup).

## Contributing to Drasil
If you're looking to contribute to Drasil, please have a look at the [Contributor's Guide](https://github.com/JacquesCarette/Drasil/wiki/Contributor's-Guide).

## Building with Drasil
If you're looking to build your project with Drasil, please have a look at the [Creating Your Project in Drasil Guide](https://github.com/JacquesCarette/Drasil/wiki/Creating-Your-Project-in-Drasil).

<!-- TODO: At some point we should document the requirements for Drasil and its design. -->

## What is Drasil? (cont.)
*Note: Please see the brief description in the [README](https://github.com/JacquesCarette/Drasil) before reading this.*

Drasil is a framework for generating all of the software artifacts from a stable knowledge base, focusing currently on scientific software. The main goals are to reduce knowledge duplication and improve traceability. The artifacts are generated from a common knowledge-base using recipes written in a Domain-Specific Language (DSL). These recipes allow us to specify which pieces of knowledge should be used in which artifacts, how to transform them, and more.

The main ideas behind Drasil stem from a combination of Literate Programming and using a rational document-driven design to create software. In other words, we want to enable the generation of software that is not only effective as a program, but as a means to communicate the inner workings of a program to other users and developers. This way, we can certify and trust new programs through its creation process. Using a structured design also increases developer productivity as new developers and users will find it easier to contribute, document, and use the software.

However, using a document-driven design process is often difficult due to the need for duplicate information. To use this method of creating software, the ideas written in the program need to be recorded twice: once for the computer to understand (as a program), and once for a developer to understand (through documentation). Manually synchronizing and coordinating information between documentation and software artifacts is difficult and time consuming. This headache of maintaining information only grows as credibility becomes harder to manage since there is now twice as much content for developers to check and revise. The chances of making a mistake within the code or documentation also becomes a lot greater and much more difficult to catch. The purpose of Drasil is to take all of the benefits of using a rational document-driven process to create software and simultaneously eliminate the problems described above.

To achieve this goal, we set out to generate all necessary software artifacts (code, documentation, requirement specifications, etc.) from only a single source of information. The keys to achieving this lie within how we [encode information](Information-Encoding), and how we [use that information](Recipes). The development of Drasil follows an example-driven process based on hand-made [case studies](https://github.com/smiths/caseStudies) that direct the creation of [data structures](Chunks) and recipes to encode information. We are currently in the process of improving our information encoding through the use of better recipes and a more automated database.

In a sense, the process outlined for the development of Drasil could be likened to the steps needed to mass-produce something in a factory. For example, in the automotive industry, when making something complex like a car, we first must learn how to make it ourselves. We experiment with creating it by hand, working out any oddities and solving various issues. Eventually, we develop a reliable procedure so that we can make a car without any issues. We then extend our range of testing so that we are 100% sure the end product is desirable. Then we take the used tools and develop better ones to help automate the process (ex. moving from using a normal screwdriver to something like a drill). From there, we realize we can make even better tools. Robots, conveyor belts, and laser cutters all increase the productivity of making a car by increasing the automation of work. After that, completely automating a factory to churn out new cars suddenly becomes within reach. Because we had started with a stable knowledge base and formed a reliable procedure that could use that knowledge, we could increase our net productivity and surpass our original objective. Even after that step, if we so desired, we could create a machine that could make a complete factory, and then a machine to make that one, and so on. 

Drasil, in a way, follows a similar process. We start with our examples, analyze some patterns within them, and then write a program to strictly follow those patterns. From there, we can extend the abilities of the program by giving it more information and more knowledge about usable patterns. Eventually, we can start generating artifacts by giving the computer recipes (many complex patterns) so that it can create documentation itself. Then, we give it a recipe for code, modules, requirements specifications, other forms of documents, etc., until it reaches the point where we only need to give knowledge and let the Drasil recipes give us all the desired artifacts. This is the current 'step' that Drasil is on. By increasing the automation of our recipes, we can extend to solve more complex problems all while maintaining that document-driven aspect. We transform the developers focus from telling the computer to do something to teaching the computer to do something. Eventually, we would like to reach the point where we can make Drasil by using Drasil, in the same way a robot in a factory could make other robots. All that would be needed is a solid base of well-understood knowledge and a domain-specific recipe powerful enough to fully use that knowledge.

To learn more about Drasil, please read our [collection of papers](Drasil-Papers-and-Documents) and take a look at our [Contributor's Guide](Contributor's-Guide).

## Issues we would like to return to

- Emphasis in text, likely for definitions: [#12](../issues/12) (and comments therein)
- Generation code for integration/summation [#72](../issues/72).
- Addition of friction and collisions to Game physics engine: [#298](../issues/298)
- Formatting options of lists: see side discussion in [#327](../issues/327)
- When we get back to code generation, esp. for SSP and GamePhys, some features are missing [#372](../issues/372)
- Add a Design Document [#417](../issues/417); should probably also use Grounded Theory do to it? [#430](../issues/430)
- Adding the option to use other documentation-tools, besides Doxygen, such as Javadoc, in generated code [#1794](../issues/1794)