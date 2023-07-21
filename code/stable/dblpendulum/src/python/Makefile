# Project Name: DblPendulum

# Project Purpose: Predict the motion of a pendulum.

build:

run: build
	python3 Control.py $(RUNARGS)

doc: doxConfig Control.py InputParameters.py Constants.py Calculations.py OutputFormat.py
	doxygen doxConfig

.PHONY: build run doc
