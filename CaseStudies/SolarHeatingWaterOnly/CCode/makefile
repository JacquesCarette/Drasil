#SYS := $(shell gcc -dumpmachine)
#UNAME := $(shell uname)

ifeq ($(OSTYPE),cygwin)
	RM=rm -f
	TARGET_EXTENSION=
	CP=cp
	SEP=/
else ifeq ($(OS),Windows_NT)
	ifneq ($(shell echo "check_quotes"), "check_quotes")
	#ifneq (, $(findstring mingw, $(PATH)))
	#ifeq ($(UNAME), MINGW64_NT-10.0)
		RM=rm -f
		CP=cp
		SEP=/
	else
		RM=del
		CP=copy
		SEP=\\
	endif
	TARGET_EXTENSION=.exe
else
	RM=rm -f
	TARGET_EXTENSION=
	CP=cp
	SEP=/
endif

C_COMPILER=gcc

CFLAGS = -std=gnu99

TST_CFLAGS = -Wall
TST_CFLAGS += -Wextra
TST_CFLAGS += -Werror
TST_CFLAGS += -Wpointer-arith
TST_CFLAGS += -Wcast-align
TST_CFLAGS += -Wwrite-strings
TST_CFLAGS += -Wswitch-default
TST_CFLAGS += -Wunreachable-code
TST_CFLAGS += -Winit-self
TST_CFLAGS += -Wmissing-field-initializers
TST_CFLAGS += -Wno-unknown-pragmas
TST_CFLAGS += -Wstrict-prototypes
TST_CFLAGS += -Wundef
TST_CFLAGS += -Wold-style-definition
TST_CFLAGS += -Wmissing-prototypes
TST_CFLAGS += -Wmissing-declarations
TST_CFLAGS += -DUNITY_FIXTURES

SRC_FILES=main.c load_params.c verify_params.c energy1.c energy2.c energy3.c verify_output.c output.c plot.c
PROGNAME=swhs
TARGET=$(PROGNAME)$(TARGET_EXTENSION)
INC_DIR= -I include
#INC_DIR= -I include -I /usr/local/include #when CVODE in usr folder
LIB_DIRS= -L lib
LIB_LINKS= -lsundials_cvode -lsundials_nvecserial -lm

TST_DIR = Testing
TST_SRC_FILES =\
  $(TST_DIR)/src/test_runners/testAll.c \
  $(TST_DIR)/src/unity.c \
  $(TST_DIR)/src/unity_fixture.c \
  $(TST_DIR)/src/unrecommendedInputTest.c \
  $(TST_DIR)/src/invalidInputTest.c \
  $(TST_DIR)/src/loadParamsTest.c \
  $(TST_DIR)/src/energyTest.c \
  $(TST_DIR)/src/verifyOutputTest.c \
  $(TST_DIR)/src/compareFortranTest.c \
  $(TST_DIR)/src/compareMatlabTest.c \
  $(TST_DIR)/src/compareSolversTest.c \
  $(TST_DIR)/src/test_runners/unrecommendedInputTestRunner.c \
  $(TST_DIR)/src/test_runners/invalidInputTestRunner.c \
  $(TST_DIR)/src/test_runners/loadParamsTestRunner.c \
  $(TST_DIR)/src/test_runners/energyTestRunner.c \
  $(TST_DIR)/src/test_runners/verifyOutputTestRunner.c \
  $(TST_DIR)/src/test_runners/compareFortranTestRunner.c \
  $(TST_DIR)/src/test_runners/compareMatlabTestRunner.c \
  $(TST_DIR)/src/test_runners/compareSolversTestRunner.c \
  verify_params.c \
  load_params.c \
  energy1.c \
  energy2.c \
  energy3.c \
  verify_output.c \
  $(TST_DIR)/src/PCM_Error.c \
  $(TST_DIR)/src/linterp.c \
  $(TST_DIR)/src/copy_file.c

TST_INC_DIR = -I $(TST_DIR)/include
TST_TARGET = test$(TARGET_EXTENSION)
  

INPUT_FILES = $(wildcard *.in)
TST_INPUT_FILES = $(wildcard C*.in)
OUTPUT_FILES = $(patsubst %.in, %.out, $(INPUT_FILES))
GRAPH_FILES = $(patsubst %.in, %.png, $(INPUT_FILES))

.PHONY : results
results : $(OUTPUT_FILES) $(GRAPH_FILES)

$(TARGET) : $(SRC_FILES)
	$(C_COMPILER) $(CFLAGS) $(SRC_FILES) $(INC_DIR) $(LIB_DIRS) $(LIB_LINKS) -o $(TARGET)

%.png %.out : %.in $(TARGET)
	./$(TARGET) $*.in

.PHONY : test
test :
	
	$(CP) $(TST_DIR)$(SEP)src$(SEP)inputFiles$(SEP)C01.in C01.in
	$(CP) $(TST_DIR)$(SEP)src$(SEP)inputFiles$(SEP)C02.in C02.in
	$(CP) $(TST_DIR)$(SEP)src$(SEP)inputFiles$(SEP)C03.in C03.in
	$(CP) $(TST_DIR)$(SEP)src$(SEP)inputFiles$(SEP)C04.in C04.in
	$(CP) $(TST_DIR)$(SEP)src$(SEP)inputFiles$(SEP)C05.in C05.in
	"$(MAKE)"
	$(C_COMPILER) $(CFLAGS) $(TST_CFLAGS) $(INC_DIR) $(TST_INC_DIR) $(TST_SRC_FILES) $(LIB_DIRS) $(LIB_LINKS) -o $(TST_TARGET)
	./$(TST_TARGET)
	"$(MAKE)" clean
	"$(MAKE)" inputClean
	
.PHONY : clean
clean :
	$(RM) $(OUTPUT_FILES)
	$(RM) $(GRAPH_FILES)
	$(RM) $(TARGET)
	$(RM) $(TST_TARGET)
	
.PHONY : inputClean
inputClean :
	$(RM) $(TST_INPUT_FILES)
