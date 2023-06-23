# Project Name and Purpose: DblPendulum - To predict the motion of a pendulum.

build:

run: build
	python3 Control.py $(RUNARGS)

doc: doxConfig Control.py InputParameters.py Constants.py Calculations.py OutputFormat.py
	doxygen doxConfig

.PHONY: build run doc
