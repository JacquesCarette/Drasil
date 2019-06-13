ifeq "$(OS)" "Windows_NT"
    # WINDOWS
	TARGET_EXTENSION=.exe
	RM=del
else
	UNAME_S := $(shell uname -s)
	ifeq ($(UNAME_S), Linux)
		# LINUX 
		TARGET_EXTENSION=
        RM=rm
	endif
	ifeq ($(UNAME_S), Darwin)
		# OSX
    	TARGET_EXTENSION=
        RM=rm
	endif
endif

TEST_DIR=test
C_COMPILER=gcc

CFLAGS = -std=c99

MODULES=src/calc.c
SRC_FILES=src/main.c $(MODULES)
INC_DIRS=-Iinclude
TEST_SRC_FILES=$(TEST_DIR)/src/unity.c $(TEST_DIR)/src/test_calc.c $(MODULES)
TEST_INC_DIRS=$(INC_DIRS) -I$(TEST_DIR)/include

PROG_NAME=hghc
PROG_TARGET=$(PROG_NAME)$(TARGET_EXTENSION)
TEST_TARGET=$(PROG_NAME)_test$(TARGET_EXTENSION)

.PHONY: all
all: clean prog test

.PHONY: prog
prog: $(PROG_TARGET) 

$(PROG_TARGET):
	$(C_COMPILER) $(CFLAGS) $(INC_DIRS) $(SRC_FILES) -o $(PROG_TARGET)

.PHONY: test
test: $(TEST_TARGET) 

$(TEST_TARGET): 
	$(C_COMPILER) $(CFLAGS) $(TEST_INC_DIRS) $(TEST_SRC_FILES) -o $(TEST_TARGET)

.PHONY: clean
clean:
	@-$(RM) $(PROG_TARGET) $(TEST_TARGET)

