# Compiler configurations:
CC=gcc
ifeq ($(SYSTEM),Darwin)
	CFLAGS=-std=c11 -Wall -Werror
else
	CFLAGS=-std=gnu99 -Wall -Werror
endif
DEBUG_FLAG=-g
INC_PATH=-Iinclude
LIB_PATH=-Llib
CHIP_LIB=-lchipmunk
MATH_LIB=-lm

# File and directory operations:
MKDIR=mkdir -p $(1)
RMDIR=rm -rf $(1)
RM=rm -f $(1)
RM_ALL=rm -f $(1) ;

# Print message with newline:
NEWLINE=echo
ECHO_NL=echo $(1) && $(NEWLINE)

# For 'make lib':
LIB_DIR=lib
LIB_NAME=lib/libchipmunk.a
LIB_README=lib/README.md
SRC_FILES=$(wildcard src/*.c)
OBJ_FILES=$(patsubst src/%.c,src/%.o,$(SRC_FILES))

# For 'make prog':
MAIN_FILE=example/Main.c
OUT_MAIN=$(patsubst %.c, %, $(MAIN_FILE))

# For 'make test':
SRC_TESTS=$(wildcard tests/*.c)
OUT_TESTS=$(patsubst tests/%.c,tests/%,$(SRC_TESTS))
UNITY_SRC=tests/Unity/src/unity.c
UNITY_PATH=-Itests/Unity/src
RESULT_FILE=tests/results.txt
RESET_RESULT=>$(RESULT_FILE)

# Compile sequences:
define compile_src
$(CC) -o $(patsubst src/%.c,src/%.o,$(1)) -c $(CFLAGS) $(1) ;
endef

define run_test
$(call ECHO_NL,Running test: $(patsubst tests/%,%,$(1))) ;
echo TEST: $(patsubst tests/%,%,$(1)) >> $(2) ;
$(NEWLINE) >> $(2) ;
$(CC) -o $(1) $(CFLAGS) $(INC_PATH) $(UNITY_PATH) $(patsubst tests/%, \
	tests/%.c,$(1)) $(UNITY_SRC) $(LIB_PATH) $(CHIP_LIB) $(MATH_LIB) ;
./$(TEST) 2> /dev/null | tee -a $(RESULT_FILE) ;
$(NEWLINE) | tee -a $(RESULT_FILE) ;
endef
