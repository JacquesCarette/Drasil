ifeq "$(OS)" "Windows_NT"
	TARGET_EXTENSION=.exe
	RM=del
else
	UNAME_S := $(shell uname -s)
	ifeq ($(UNAME_S), Linux)
		TARGET_EXTENSION=
		RM=rm
	endif
	ifeq ($(UNAME_S), Darwin)
		TARGET_EXTENSION=
		RM=rm
	endif
endif

build: GlassBR/Control.class

GlassBR/Control.class: GlassBR/Control.java GlassBR/InputParameters.java GlassBR/DerivedValues.java GlassBR/InputConstraints.java GlassBR/OutputFormat.java GlassBR/Calculations.java GlassBR/ReadTable.java GlassBR/InputFormat.java GlassBR/Interpolation.java
	javac GlassBR/Control.java

run: build
	java GlassBR.Control $(RUNARGS)

.PHONY: build run
