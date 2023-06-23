# Project Name and Purpose: Projectile - To predict whether a launched projectile hits its target.

build:

run: build
	python3 Control.py $(RUNARGS)

doc: doxConfig Control.py InputParameters.py Calculations.py OutputFormat.py
	doxygen doxConfig

.PHONY: build run doc
