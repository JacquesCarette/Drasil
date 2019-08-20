build:

run: build
	python Control.py $(RUNARGS)

doc: doxConfig Control.py InputParameters.py InputFormat.py DerivedValues.py InputConstraints.py OutputFormat.py Calculations.py ReadTable.py Interpolation.py
	doxygen doxConfig

.PHONY: build run doc
