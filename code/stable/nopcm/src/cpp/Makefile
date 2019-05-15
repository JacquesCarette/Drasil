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

build: SWHS$(TARGET_EXTENSION)

SWHS$(TARGET_EXTENSION): InputParameters.hpp OutputFormat.hpp Calculations.hpp InputFormat.hpp Control.cpp InputParameters.cpp OutputFormat.cpp Calculations.cpp InputFormat.cpp
	$(CXX) Control.cpp InputParameters.cpp OutputFormat.cpp Calculations.cpp InputFormat.cpp --std=c++11 -o SWHS$(TARGET_EXTENSION)

run: build
	./SWHS$(TARGET_EXTENSION) $(RUNARGS)

.PHONY: build run
