build: SimpleODE/SimpleODE.class

SimpleODE/SimpleODE.class: SimpleODE/SimpleODE.java SimpleODE/T_ODE.java SimpleODE/T_StepHandler.java
	javac SimpleODE/SimpleODE.java

run: build
	java SimpleODE.SimpleODE $(RUNARGS)

.PHONY: build run
