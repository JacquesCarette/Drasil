ifeq "$(OS)" "Windows_NT"
    TARGET_EXTENSION=.exe
    CSC=csc
else
    UNAME_S := $(shell uname -s)
    ifeq ($(UNAME_S), Linux)
        TARGET_EXTENSION=
        CSC=mcs
    endif
    ifeq ($(UNAME_S), Darwin)
        TARGET_EXTENSION=
        CSC=mcs
    endif
endif

build: PatternTest$(TARGET_EXTENSION)

PatternTest$(TARGET_EXTENSION): PatternTest.cs Observer.cs
	$(CSC) -out:PatternTest$(TARGET_EXTENSION) PatternTest.cs Observer.cs

run: build
	./PatternTest$(TARGET_EXTENSION) $(RUNARGS)

.PHONY: build run
