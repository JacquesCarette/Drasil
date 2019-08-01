build:

run: build
	python Control.py $(RUNARGS)

doc: doxConfig Control.py InputParameters.py OutputFormat.py
	doxygen doxConfig

.PHONY: build run doc
