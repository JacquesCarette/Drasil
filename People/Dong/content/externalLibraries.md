# Background
## why external libraries
There are many reliable external libraries, and the Drasil team want to study how the drasil framework interact with external libraries.

## why

- what are they
    - an external library comes from an outside source. It is not originate from the source project. There are numerous libraries and most them are languages dependent. The drasil framework able to generate code for five different languages, Python, C#, Java, C++, and swift. There are many reliable external libraries for those five programming languages, and the Drasil team want to study how the drasil framework interact with external libraries. We believe the ODE is the most important one, because a lot of scientific problems use ODE as a model. Therefore, we pick ODE libraries as our study object.

- similarity of how libraries solve the ODEs
    Not all language has ODE libraries. We pick the following four libraries.
    - Python - scipy Library https://scipy.org/
    - C# - OSLO Library https://www.microsoft.com/en-us/research/project/open-solving-library-for-odes/
    - Java - Apache Commons Mathematics Library- https://commons.apache.org/proper/commons-math/
    - C++ - ODEINT Library - http://headmyshoulder.github.io/odeint-v2/

- initial value problem
    The ODE that we are going to solve changes over time. The equation represent a function, and it describe continuous change. What if we only interested on a partial solution. For example the change between time 0s to 60s. We can treat this question as an initial value problem (IVP). IVP provide a partial solution, and it produces a point with respect a time.

There are some commonality with those four libraries. First, all of them able to solve first order ODEs numerically. Second, they allow to use different algorithms to calculate the numerical solution. Last, it requires users to collect the targeted information.
- solve first order ode with shape of X' = AX + c
    - all libraries return points
    - some libraries return spline
        - https://github.com/JacquesCarette/Drasil/issues/2982
- algorithms A table

- collection information
    - how they solve ODEs
        - at the first iteration, use initial values to get the point.
        - the next iteration, it will use the previous iteration to get the point. 
    - we need to collect information from each iteration

- handle external libraries in Drasil - symbolic links
    - keep a copy of external libraries inside of the Drasil framework
        - it starts cause problems when more examples need external library source files
            - drasil currently don't create a makefile. each case study will have it's own file to store any axillary, for example, input.txt and any images. The input.txt is a file that a generated program will read. Necessary images will be displayed in SRS. We treat external libraries as an axillary file. Beside Python, there are related ODE libraries source file for the rest three programming language.Python requires users install scipy at the their local machine.

    - symbolic links serve as a temporary solution.
        - Drasil deployment runs in a linux environment, so we can use symbolic link https://en.wikipedia.org/wiki/Symbolic_link to the targeted file with external libraries.
        - https://github.com/JacquesCarette/Drasil/issues/2980
        - future project in https://github.com/JacquesCarette/Drasil/wiki/Potential-Projects#develop-a-strategy-for-working-with-shared-external-libraries
