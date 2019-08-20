build: SWHS/Control.class

SWHS/Control.class: SWHS/Control.java SWHS/InputParameters.java SWHS/OutputFormat.java
	javac SWHS/Control.java

run: build
	java SWHS.Control $(RUNARGS)

doc: doxConfig SWHS/Control.java SWHS/InputParameters.java SWHS/OutputFormat.java
	doxygen doxConfig

.PHONY: build run doc
