# Project Name and Purpose: NoPCM - To investigate the heating of water in a solar water heating tank.

build:

run: build
	python3 Control.py $(RUNARGS)

doc: doxConfig Control.py InputParameters.py Constants.py Calculations.py OutputFormat.py
	doxygen doxConfig

.PHONY: build run doc
