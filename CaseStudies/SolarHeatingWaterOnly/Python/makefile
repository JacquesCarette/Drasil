UNAME := $(shell uname)

ifeq ($(OS),Windows_NT)
	ifeq ($(UNAME),MINGW64_NT-10.0)
		RM=rm -f
		CP=cp
		SEP=/
	else
		RM=cmd //C del
		CP=cmd //C copy
		SEP=\\
	endif
else
	RM=rm -f
	CP=cp
	SEP=/
endif


PY=python
MAIN_SRC_FILE=main.py
SRC_FILES=parameters.py load_params.py verify_params.py energy.py verify_output.py plot.py output.py
INPUT_FILES = $(wildcard *.in)
OUTPUT_FILES = $(patsubst %.in, %.out, $(INPUT_FILES))
GRAPH_FILES = $(patsubst %.in, %.png, $(INPUT_FILES))
TST_DIR = Testing
TST_SRC_FILE=run_all_tests.py
TST_INPUT_FILES=$(wildcard P*.in)

.PHONY : results
results : $(OUTPUT_FILES) $(GRAPH_FILES)

%.png %.out : %.in $(MAIN_SRC_FILE) $(SRC_FILES)
	$(PY) $(MAIN_SRC_FILE) $*.in

.PHONY : test
test :

	$(CP) $(TST_DIR)$(SEP)inputFiles$(SEP)P01.in P01.in
	$(CP) $(TST_DIR)$(SEP)inputFiles$(SEP)P02.in P02.in
	$(CP) $(TST_DIR)$(SEP)inputFiles$(SEP)P03.in P03.in
	$(CP) $(TST_DIR)$(SEP)inputFiles$(SEP)P04.in P04.in
	$(CP) $(TST_DIR)$(SEP)inputFiles$(SEP)P05.in P05.in
	"$(MAKE)"
	$(PY) $(TST_DIR)$(SEP)$(TST_SRC_FILE)
	"$(MAKE)" clean
	"$(MAKE)" inputClean
	
.PHONY : clean
clean :
	$(RM) $(OUTPUT_FILES)
	$(RM) $(GRAPH_FILES)
	
.PHONY : inputClean
inputClean :
	$(RM) $(TST_INPUT_FILES)