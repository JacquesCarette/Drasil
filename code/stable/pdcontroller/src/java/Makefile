build: PD_Controller/Control.class

PD_Controller/Control.class: PD_Controller/Control.java PD_Controller/InputParameters.java PD_Controller/Constants.java PD_Controller/Calculations.java PD_Controller/OutputFormat.java PD_Controller/ODE.java PD_Controller/ODEStepHandler.java
	javac -cp "lib/commons-math3-3.6.1.jar:." PD_Controller/Control.java

run: build
	java -cp "lib/commons-math3-3.6.1.jar:." PD_Controller.Control $(RUNARGS)

doc: doxConfig PD_Controller/Control.java PD_Controller/InputParameters.java PD_Controller/Constants.java PD_Controller/Calculations.java PD_Controller/OutputFormat.java PD_Controller/ODE.java PD_Controller/ODEStepHandler.java
	doxygen doxConfig

.PHONY: build run doc
