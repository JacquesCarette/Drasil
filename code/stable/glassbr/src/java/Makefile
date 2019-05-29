build: GlassBR/Control.class

GlassBR/Control.class: GlassBR/Control.java GlassBR/InputParameters.java GlassBR/DerivedValues.java GlassBR/InputConstraints.java GlassBR/OutputFormat.java GlassBR/Calculations.java GlassBR/ReadTable.java GlassBR/InputFormat.java GlassBR/Interpolation.java
	javac GlassBR/Control.java

run: build
	java GlassBR.Control $(RUNARGS)

.PHONY: build run
