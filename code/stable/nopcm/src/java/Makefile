build: SWHS/Control.class

SWHS/Control.class: SWHS/Control.java SWHS/InputParameters.java SWHS/Constants.java SWHS/Calculations.java SWHS/OutputFormat.java SWHS/ODE.java SWHS/ODEStepHandler.java
	javac -cp "lib/commons-math3-3.6.1.jar:." SWHS/Control.java

run: build
	java -cp "lib/commons-math3-3.6.1.jar:." SWHS.Control $(RUNARGS)

doc: doxConfig SWHS/Control.java SWHS/InputParameters.java SWHS/Constants.java SWHS/Calculations.java SWHS/OutputFormat.java SWHS/ODE.java SWHS/ODEStepHandler.java
	doxygen doxConfig

.PHONY: build run doc
