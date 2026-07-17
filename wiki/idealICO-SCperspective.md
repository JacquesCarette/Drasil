# Idealized Process from the Scientific Computing (Research Software) perspective

After reading Jacques draft of an idealized process, I decided to pause on that review and first collect my own thoughts.  I'm trying to do this independently so that my ideas aren't influenced by Jacques draft. :-)

This document is my initial thoughts on an idealized process from the perspective of someone creating research software.  At this time I'm not going to explicitly match up this process with a generative process.  I want to just think about it as if I was doing a new scientific computing problem.  However, I'm going to make an effort to be much more explicit on characterizing and naming the different types of decisions and information.  

I'm assuming that the scientific computing problem is an ICO (input calculate output) problem.  The running example of projectile motion will be used throughout.  The closest document that follows this process for projectile motion is the [SRS of the refined theories version of projectile motion](https://github.com/smiths/caseStudies/blob/master/CaseStudies/projectile/projectileSRS_RefinedTheories/Projectile_SRS.pdf). Inspiration also comes from the little theories version of projectile motion started by Bill Farmer.

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

| Step in the Process | Example(s) from Projectile |
| --- | --- |
| Identify the scientific question | How far will the projectile go? |
| Identify context | The projectile is a ball used in sports on Earth; the software will be used for teaching physics |
| Rationale (justification) for question and context | Sports are popular; we play them on Earth; the context of sports is a good motivator for teaching physics. |
| Define goal(s) | Determine the horizontal distance the projectile travels from the target. |
| Determine what quantity needs to be predicted | The distance from the launch point |
| Define scope | <ul><li>We don't care about the orientation of the projectile</li><li>We don't care about forces acting on the projectile, only its kinematics</li><li>The projectile will travel a relatively short distance, consistent with the context of ball sports; that is, ballistic trajectories are outside the scope.</li></ul>|
| Rationale for scope decisions | For teaching purposes the scope is appropriate. |
| Write problem description document | We don't currently have an example of this document. |

From the ChatGPT process, I've changed the following:

- I say goals instead of objectives, since goals is the terminology used in the SRS template
- I split the single bullet point of define objectives (goals), assumptions, and scope into separate bullets because separation lets us focus on each item 
- The "define assumptions" sub-step has been moved to the next step ("Mathematical Modeling")
- I dropped "explained and optimized", choosing to just focus on "predicted" because the scope of this exercise is currently just for ICO problems.

To the ChatGPT process, I've added the following:

- "define context" is a new sub-step
- sub-steps related to rationale information have been added
- a written problem description document has been added

## Mathematical Modeling

| Step in the Process | Example(s) from Projectile |
| --- | --- |
| Make modelling decisions | 2D; Cartesian coordinate system; launcher is coincident with the origin; time starts at zero; launch velocity is positive; magnitude and angle representation for the initial velocity vector; etc.|
| Identify context theories | theory of complete ordered fields; natural, integers and rationals; single-variable calculus; Euclidean space; logarithm and exponential functions; trigonometry|
| Identify background theories, built on context theories | Kinematic equations relating position, velocity and acceleration in $n$-dimensions
| Identify generic theories, refined from background theories (there may be multiple levels of theory refinement) | Rectilinear motion of a body in one dimension under constant acceleration; conversion between magnitude/angle representation of a vector to the component representation|
| Refine generic theories into problem specific theories | Motion of a projectile in two dimensions using the magnitude and angle representation of velocity, gravity is a constant parameter|
| Refine problem specific theories into final theories | Motion of a projectile in two dimensions, gravity is set to Earth's gravity|
| Identify theories needed to provide rationale arguments | Error introduced by assuming a flat planet instead of accounting for curvature|
| Identify background theory assumptions | None |
| Identify generic theory assumptions | 1D acceleration is constant |
| Identify problem specific theory assumptions | In $x$ direction acceleration is zero, in $y$ direction acceleration is a constant; no obstructions impede the path of the projectile; the surface of the planet is assumed to be plat |
| Identify rationale assumptions | We only care about the motion of the centre of the mass of the projectile; the curvature of the celestial boy can be ignored, air drag is neglected, the rotation of the planet can be neglected |
| Identify data constraints | the magnitude of the launch velocity is greater than zero; the launch angle is between 0 and $\pi$/2.|
| Identify the properties of a correct solution | the horizontal distance between the launch point and the landing point is greater than zero|
| Present the rationale for the modelling decisions | rationale for Cartesian coordinate system is that the problem involves rectilinear motion and curvature is not considered, etc.|
| Present the rationale for the final theory assumptions | the rationale for considering a flat planet is that the scope is for problems with relatively small velocities and distances.  Using the rationale theory related to the error introduced by neglecting curvature shows that for typical values for sports relevant velocities the error is small.|

Compared to the ChatGPT version of the process, the above version adds much more on theory refinement, rationale and the properties of a correct solution.

I will look at the other steps in the ChatGPT SC process after comparing the first steps to what Jacques produced.