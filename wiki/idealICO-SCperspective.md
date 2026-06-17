# Idealized Process from the Scientific Computing (Research Software) perspective

After reading Jacques draft of an idealized process, I decided to pause on that review and first collect my own thoughts.  I'm trying to do this independently so that my ideas aren't influenced by Jacques draft. :-)

This document is my initial thoughts on an idealized process from the perspective of someone creating research software.  At this time I'm not going to explicitly match up this process with a generative process.  I want to just think about it as if I was doing a new scientific computing problem.  However, I'm going to make an effort to be much more explicit on characterizing and naming the different types of decisions and information.  

I'm assuming that the scientific computing problem is an ICO (input calculate output) problem.  The running example of projectile motion will be used throughout.  The closest document that follows this process for projectile motion is the [SRS of the refined theories version of projectile motion](https://github.com/smiths/caseStudies/blob/master/CaseStudies/projectile/projectileSRS_RefinedTheories/Projectile_SRS.pdf).

This is intended to follow Parnas's faked rational process.  That is, the steps are how they would appear if we had perfect knowledge of where we were going to end up.  This might be different from the idealized process recorded by Jacques.  I can't tell from reviewing Jacques's steps whether the gathering of theories is just the relevant theories, or if the gathering includes theories that *might* be relevant.  In the current case iteration would surely have occurred, but the version presented as ideal is the final version.

This isn't a competing option to Jacques's option.  It is a different view of hopefully the same process.  I think we'll be able to compare and contrast and gain insights that improve both views and eventually get them to match.

This version is written as if all steps are done manually.  There is computer support, but not artifact generation.  Manually created artifacts, like a problem description and an SRS, are part of the output of the process.

This ideal process represents an exploded view of the [typical scientific computing process as described by ChatGPT](https://chatgpt.com/share/6a32ddd5-a8e0-83ea-9602-4473f869898c):

1. Problem Formulation
- Identify the scientific question.
- Define objectives, assumptions, and scope.
- Determine what quantities need to be predicted, explained, or optimized.
2. Mathematical Modeling
- Translate the scientific problem into mathematics.
- Develop equations, constraints, and relationships.
- Examples include differential equations, statistical models, optimization problems, or agent-based models.
3. Model Analysis
- Study properties of the mathematical model.
- Determine existence and uniqueness of solutions.
- Analyze stability, sensitivity, and expected behavior.
4. Numerical Method Selection
- Choose algorithms to approximate the mathematical solution.
- Examples include finite difference methods, finite element methods, Monte Carlo methods, or iterative solvers.
- Consider accuracy, stability, and computational cost.
5. Software Implementation
- Implement the numerical methods in software.
- Design data structures, workflows, and interfaces.
- Verify that the implementation correctly realizes the chosen algorithms.
6. Computation / Simulation
- Run simulations or computations.
- Manage computational resources such as clusters, GPUs, or cloud infrastructure.
- Generate output data.
7. Verification
- Ask: Did we solve the equations correctly?
- Check for programming errors.
- Compare against analytical solutions, benchmark problems, convergence studies, and test suites.
8. Validation
- Ask: Are we solving the correct equations?
- Compare results with experimental observations, measurements, or established theory.
- Assess whether the model adequately represents reality.
9. Data Analysis and Visualization
- Process and interpret computational results.
- Use statistical analysis, plots, and visualizations.
- Identify trends, anomalies, and scientific insights.
10. Scientific Interpretation
- Relate computational findings back to the original scientific question.
- Draw conclusions, formulate hypotheses, or make predictions.
11. Reproducibility and Dissemination
- Document methods, software, data, and computational environments.
- Publish results and share artifacts.
- Enable others to reproduce and build upon the work.

I'll go through the ChatGPT steps and make changes, additions and clarifications.

## Problem Formulation

| Step in the Process | Example from Projectile |
| --- | --- |
| Identify the scientific question | How far will the projectile go? |
| Identify context | The projectile is a ball used in sports on Earth |
| Rationale (Justification) for question and context | Sports are popular; we play them on Earth; the context of sports is a good motivator for teaching physics. |
| Define goal(s) | Determine the horizontal distance the projectile travels from the target. |
| Define scope | |
| Rationale for scope | |
| Determine what quantity needs to be predicted | |
| Write problem description document | |

From the ChatGPT process, I've changed the following:

- I say goals instead of objectives, since goals is the terminology used in the SRS template
- I split the single bullet point of define objectives (goals), assumptions, and scope into separate bullets because separation lets us focus on each item specifically
- The "define assumptions" sub-step has been moved to the next step ("Mathematical Modeling")

To the ChatGPT process, I've added the following:

- "define context" is a new sub-step
- sub-steps related to rationale information

## Mathematical Modeling
