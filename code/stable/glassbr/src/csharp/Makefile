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

build: GlassBR$(TARGET_EXTENSION)

GlassBR$(TARGET_EXTENSION): Control.cs InputParameters.cs DerivedValues.cs InputConstraints.cs OutputFormat.cs Calculations.cs ReadTable.cs InputFormat.cs Interpolation.cs
	mcs Control.cs InputParameters.cs DerivedValues.cs InputConstraints.cs OutputFormat.cs Calculations.cs ReadTable.cs InputFormat.cs Interpolation.cs -out:GlassBR$(TARGET_EXTENSION)

run: build
	./GlassBR$(TARGET_EXTENSION) $(RUNARGS)

.PHONY: build run
