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

GlassBR$(TARGET_EXTENSION): InputParameters.hpp DerivedValues.hpp InputConstraints.hpp OutputFormat.hpp Calculations.hpp ReadTable.hpp InputFormat.hpp Interpolation.hpp Control.cpp InputParameters.cpp DerivedValues.cpp InputConstraints.cpp OutputFormat.cpp Calculations.cpp ReadTable.cpp InputFormat.cpp Interpolation.cpp
	$(CXX) Control.cpp InputParameters.cpp DerivedValues.cpp InputConstraints.cpp OutputFormat.cpp Calculations.cpp ReadTable.cpp InputFormat.cpp Interpolation.cpp --std=c++11 -o GlassBR$(TARGET_EXTENSION)

run: build
	./GlassBR$(TARGET_EXTENSION) $(RUNARGS)

.PHONY: build run
