# Project Name and Purpose: PD_Controller - To provide a model of a PD Controller that can be used for the tuning of the gain constants before the deployment of the controller.

build:

run: build
	python3 Control.py $(RUNARGS)

doc: doxConfig Control.py InputParameters.py Constants.py Calculations.py OutputFormat.py
	doxygen doxConfig

.PHONY: build run doc
