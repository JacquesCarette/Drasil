build:

run: build
	python Control.py $(RUNARGS)

doc: doxConfig Control.py InputParameters.py InputFormat.py InputConstraints.py OutputFormat.py Calculations.py
	doxygen doxConfig

.PHONY: build run doc
