# System detection.
ifeq ($(OS), Windows_NT)
	ifneq (,$(findstring /cygdrive/,$(PATH)))
		SYSTEM=Cygwin
	else ifneq (,$(findstring mingw,$(PATH)))
		SYSTEM=MSYS
	else
		SYSTEM=Windows
	endif
else
	SYSTEM=$(shell uname -s)
endif

# Include appropriate configurations.
ifeq ($(SYSTEM),Windows)
	include winconfig.mk
else
	include config.mk
endif

## prog: Generate the test program executable. Default make instruction.
.PHONY : prog
prog : $(LIB_NAME)
	@$(call ECHO_NL,Running target: prog...)

# Generate executable for the 'Main' sample program.
	@echo Compiling Main executable...
	@$(CC) -o $(OUT_MAIN) $(CFLAGS) $(DEBUG_FLAG) $(INC_PATH) $(MAIN_FILE) \
	$(LIB_PATH) $(CHIP_LIB) $(MATH_LIB)
	@echo The Main executable has been generated in the /example directory.
	@$(call ECHO_NL,To run: go to the /example directory and run ./Main \
	for Unix or Main.exe for Windows.)

## all: Builds all targets (prog and test).
.PHONY : all
all : prog test

## lib: Builds the Chipmunk static library (libchipmunk.a) in the lib directory.
lib : $(LIB_NAME)
$(LIB_NAME) :
	@$(call ECHO_NL,Running target: lib...)

# Creates the 'lib' directory if not already present, and inserts a
# README.md file for information.
	@echo Creating 'lib' directory...
	@$(call MKDIR,$(LIB_DIR))
	@echo This directory contains **libchipmunk.a**, the Chipmunk static \
	library. > $(LIB_README)

# Compiles all source files in the /src directory.
	@echo Compiling source files...
	@$(foreach SRC,$(SRC_FILES),$(call compile_src,$(SRC))) $(NEWLINE)

# Compiles the object files into a static library in /lib.
	@echo Compiling library from object files...
	@ar rcs $(LIB_NAME) $(OBJ_FILES)

# Deletes the object files.
	@echo Deleting object files...
	@$(foreach OBJ,$(OBJ_FILES),$(call RM_ALL,$(OBJ))) $(NEWLINE)
	@$(call ECHO_NL,Static library libchipmunk.a compiled in /lib.)

## test: Build and run all test cases.
.PHONY : test
test : $(LIB_NAME)
	@$(call ECHO_NL,Running target: test...)

# Resets result file every time test is re-run.
	@$(RESET_RESULT)

# Compile each test source, display results to the screen and print to
# result file. Any Chipmunk warnings printed to stderr is discarded.
	@$(foreach TEST,$(OUT_TESTS),$(call run_test,$(TEST),$(RESULT_FILE))) \
		$(NEWLINE)
	@$(call RM,_)

## clean: Remove ALL auto-generated files. Returns directory to pre-build state.
.PHONY : clean
clean :
	@$(call ECHO_NL,Running target: clean...)

	@echo Removing the Chipmunk library...
	@$(call RMDIR,$(LIB_DIR))

	@echo Removing 'Main' executable...
	@$(call RM,$(OUT_MAIN))

	@echo Removing test files and result file...
	@$(foreach TEST,$(OUT_TESTS),$(call RM_ALL,$(TEST))) $(NEWLINE)
	@$(call RM,$(RESULT_FILE))

	@$(call ECHO_NL,All generated files have been removed.)

## help: Show all targets for this Makefile.
.PHONY : help
help : Makefile
	@sed -n 's/^##//p' $<
