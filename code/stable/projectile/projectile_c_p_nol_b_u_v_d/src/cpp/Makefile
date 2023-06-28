ifeq "$(OS)" "Windows_NT"
    TARGET_EXTENSION=.exe
else
    UNAME_S := $(shell uname -s)
    ifeq ($(UNAME_S), Linux)
        TARGET_EXTENSION=
    endif
    ifeq ($(UNAME_S), Darwin)
        TARGET_EXTENSION=
    endif
endif

# Project Name and Purpose: Projectile - To predict whether a launched projectile hits its target.

build: Projectile$(TARGET_EXTENSION)

Projectile$(TARGET_EXTENSION): InputParameters.hpp Calculations.hpp OutputFormat.hpp Control.cpp InputParameters.cpp Calculations.cpp OutputFormat.cpp
	"$(CXX)" Control.cpp InputParameters.cpp Calculations.cpp OutputFormat.cpp --std=c++11 -o Projectile$(TARGET_EXTENSION)

run: build
	./Projectile$(TARGET_EXTENSION) $(RUNARGS)

doc: doxConfig InputParameters.hpp Calculations.hpp OutputFormat.hpp Control.cpp
	doxygen doxConfig

.PHONY: build run doc
