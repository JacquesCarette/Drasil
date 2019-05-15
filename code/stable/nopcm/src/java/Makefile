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

build: SWHS/Control.class

SWHS/Control.class: SWHS/Control.java SWHS/InputParameters.java SWHS/OutputFormat.java SWHS/Calculations.java SWHS/InputFormat.java
	javac SWHS/Control.java

run: build
	java SWHS.Control $(RUNARGS)

.PHONY: build run
