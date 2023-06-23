# Project Name and Purpose: DblPendulum - To predict the motion of a pendulum.

build: DblPendulum/Control.class

DblPendulum/Control.class: DblPendulum/Control.java DblPendulum/InputParameters.java DblPendulum/Constants.java DblPendulum/Calculations.java DblPendulum/OutputFormat.java DblPendulum/ODE.java DblPendulum/ODEStepHandler.java
	javac -cp "lib/commons-math3-3.6.1.jar:." DblPendulum/Control.java

run: build
	java -cp "lib/commons-math3-3.6.1.jar:." DblPendulum.Control $(RUNARGS)

doc: doxConfig DblPendulum/Control.java DblPendulum/InputParameters.java DblPendulum/Constants.java DblPendulum/Calculations.java DblPendulum/OutputFormat.java DblPendulum/ODE.java DblPendulum/ODEStepHandler.java
	doxygen doxConfig

.PHONY: build run doc
