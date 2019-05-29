build: SWHS/Control.class

SWHS/Control.class: SWHS/Control.java SWHS/InputParameters.java SWHS/OutputFormat.java SWHS/Calculations.java SWHS/InputFormat.java
	javac SWHS/Control.java

run: build
	java SWHS.Control $(RUNARGS)

.PHONY: build run
