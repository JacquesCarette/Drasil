ifeq "$(OS)" "Windows_NT"
    LIB_EXTENSION=.dll
else
    UNAME_S := $(shell uname -s)
    ifeq ($(UNAME_S), Linux)
        LIB_EXTENSION=.so
    endif
    ifeq ($(UNAME_S), Darwin)
        LIB_EXTENSION=.dylib
    endif
endif

build: Projectile$(LIB_EXTENSION)

Projectile$(LIB_EXTENSION): InputFormat.hpp InputConstraints.hpp OutputFormat.hpp Calculations.hpp InputFormat.cpp InputConstraints.cpp OutputFormat.cpp Calculations.cpp
	"$(CXX)" InputFormat.cpp InputConstraints.cpp OutputFormat.cpp Calculations.cpp --std=c++11 -shared -o Projectile$(LIB_EXTENSION)

doc: doxConfig InputFormat.hpp InputConstraints.hpp OutputFormat.hpp Calculations.hpp
	doxygen doxConfig

.PHONY: build doc
