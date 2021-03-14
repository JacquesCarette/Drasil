build:

run: build
	python Control.py $(RUNARGS)

doc: doxConfig Control.py InputParameters.py Constants.py Calculations.py OutputFormat.py
	doxygen doxConfig

.PHONY: build run doc
