# Project Name: Projectile

# Project Purpose: Predict whether a launched projectile hits its target.

build:

run: build
	python3 Projectile.py $(RUNARGS)

doc: doxConfig Projectile.py
	doxygen doxConfig

.PHONY: build run doc
