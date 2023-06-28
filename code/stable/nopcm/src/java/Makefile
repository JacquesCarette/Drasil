# Project Name and Purpose: NoPCM - To investigate the heating of water in a solar water heating tank.

build: NoPCM/Control.class

NoPCM/Control.class: NoPCM/Control.java NoPCM/InputParameters.java NoPCM/Constants.java NoPCM/Calculations.java NoPCM/OutputFormat.java NoPCM/ODE.java NoPCM/ODEStepHandler.java
	javac -cp "lib/commons-math3-3.6.1.jar:." NoPCM/Control.java

run: build
	java -cp "lib/commons-math3-3.6.1.jar:." NoPCM.Control $(RUNARGS)

doc: doxConfig NoPCM/Control.java NoPCM/InputParameters.java NoPCM/Constants.java NoPCM/Calculations.java NoPCM/OutputFormat.java NoPCM/ODE.java NoPCM/ODEStepHandler.java
	doxygen doxConfig

.PHONY: build run doc
