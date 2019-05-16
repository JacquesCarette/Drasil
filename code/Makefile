PACKAGES = lang build code theory database printers gen data docLang example

BUILD_P_SUFFIX = _build
DOC_P_SUFFIX = _doc

BUILD_PACKAGES = $(addsuffix $(BUILD_P_SUFFIX), $(PACKAGES))
DOC_PACKAGES = $(addsuffix $(DOC_P_SUFFIX), $(PACKAGES))

PACKAGE_GEN_TARGET = BUILD DOC

TINY_DIR  = Tiny
GLASSBR_DIR = GlassBR
NOPCM_DIR = NoPCM
SWHS_DIR  = SWHS
SSP_DIR   = SSP
GAMEPHYS_DIR  = Chipmunk

TINY_EXE  = tiny
GLASSBR_EXE = glassbr
NOPCM_EXE = nopcm
SWHS_EXE  = swhs
SSP_EXE   = ssp
GAMEPHYS_EXE  = chipmunkdocs

EXAMPLES = tiny glassbr nopcm swhs ssp gamephys

GEN_E_SUFFIX = _gen
TEST_E_SUFFIX = _diff
MOVE_DF_E_SUFFIX = _prog
TEX_E_SUFFIX = _tex
CODE_E_SUFFIX = _code

GEN_EXAMPLES = $(addsuffix $(GEN_E_SUFFIX), $(EXAMPLES))
TEST_EXAMPLES =  $(addsuffix $(TEST_E_SUFFIX), $(EXAMPLES))
MOVE_DF_EXAMPLES =  $(addsuffix $(MOVE_DF_E_SUFFIX), $(EXAMPLES))
TEX_EXAMPLES = $(addsuffix $(TEX_E_SUFFIX), $(EXAMPLES))
CODE_EXAMPLES = $(addsuffix $(CODE_E_SUFFIX), $(EXAMPLES))

EXAMPLE_GEN_TARGET = GEN TEST MOVE_DF TEX CODE

GENNED_FOLDERS = $(LOG_FOLDER_NAME) build
CLEAN_GF_PREFIX = clean_
CLEAN_FOLDERS = $(addprefix $(CLEAN_GF_PREFIX), $(GENNED_FOLDERS))

ALL_EXPANDED_TARGETS = $(foreach P, $(PACKAGE_GEN_TARGET), $($(P)_PACKAGES)) $(foreach E, $(EXAMPLE_GEN_TARGET), $($(E)_EXAMPLES)) $(CLEAN_FOLDERS)

# make variables/configuration
DIFF = diff -r -X ../.gitignore -x '*.txt'
LOG_SUFFIX = _log.log
MIN_STACK_VER = 1.9.1  # Version which adds --interleaved-output flag
CACHED_MSV_FILE = .drasil-min-stack-ver
SCRIPT_FOLDER = scripts/
LOG_FOLDER_NAME = logs
LOG_FOLDER = $(LOG_FOLDER_NAME)

# make command line options
#  GHC debug options
PROFALL = --executable-profiling --library-profiling
PROFEXEC = +RTS -xc -P

#  GHC build options
GHCTHREADS += 2
override GHCFLAGS += -Wall -j$(GHCTHREADS)
override stackArgs += --ghc-options="$(GHCFLAGS)"

#  Output amount control
NOISY=no
SUMMARIZE_TEX=no

all: test

debug: stackArgs+=$(PROFALL) 
debug: EXECARGS+=$(PROFEXEC) 
debug: test

check_stack:
	 @MIN_STACK_VER=$(MIN_STACK_VER) CACHED_MSV_FILE=$(CACHED_MSV_FILE) $(SHELL) $(SCRIPT_FOLDER)check_stack.sh

$(filter %$(BUILD_P_SUFFIX), $(BUILD_PACKAGES)): %$(BUILD_P_SUFFIX): check_stack
	stack install -j3 $(stackArgs) drasil-$* --dump-logs --interleaved-output

packages: $(BUILD_PACKAGES)

%$(GEN_E_SUFFIX): EXAMPLE=$(shell echo $* | tr a-z A-Z)
%$(GEN_E_SUFFIX): EDIR=$($(EXAMPLE)_DIR)
%$(GEN_E_SUFFIX): EEXE=$($(EXAMPLE)_EXE)
$(filter %$(GEN_E_SUFFIX), $(GEN_EXAMPLES)): %$(GEN_E_SUFFIX): example$(BUILD_P_SUFFIX)
	mkdir -p build/$(EDIR)
	cd build/$(EDIR) && stack exec -- $(EEXE) $(EXECARGS)

%$(TEST_E_SUFFIX): EXAMPLE=$(shell echo $* | tr a-z A-Z)
%$(TEST_E_SUFFIX): EDIR=$($(EXAMPLE)_DIR)
$(filter %$(TEST_E_SUFFIX), $(TEST_EXAMPLES)): %$(TEST_E_SUFFIX): %$(GEN_E_SUFFIX)
	@mkdir -p $(LOG_FOLDER)
	- $(DIFF) ./stable/$*/ ./build/$(EDIR)/ > $(LOG_FOLDER)$(EDIR)$(LOG_SUFFIX) 2>&1

test: $(GEN_EXAMPLES) $(TEST_EXAMPLES)
	@echo ----------------------------
	@echo Make complete, checking logs
	@echo ----------------------------
	LOG_FOLDER=$(LOG_FOLDER) LOG_SUFFIX=$(LOG_SUFFIX) $(SHELL) $(SCRIPT_FOLDER)log_check.sh $(NOISY)

$(filter %$(MOVE_DF_E_SUFFIX), $(MOVE_DF_EXAMPLES)): %$(MOVE_DF_E_SUFFIX): %$(GEN_E_SUFFIX)

$(GLASSBR_EXE)$(MOVE_DF_E_SUFFIX): $(GLASSBR_EXE)$(GEN_E_SUFFIX)
	test -d ./build/$(GLASSBR_DIR)/src/python && cp ./datafiles/$(GLASSBR_DIR)/*.txt ./build/$(GLASSBR_DIR)/src/python/
	test -d ./build/$(GLASSBR_DIR)/src/java && cp ./datafiles/$(GLASSBR_DIR)/*.txt ./build/$(GLASSBR_DIR)/src/java/
	test -d ./build/$(GLASSBR_DIR)/src/csharp && cp ./datafiles/$(GLASSBR_DIR)/*.txt ./build/$(GLASSBR_DIR)/src/csharp/
	test -d ./build/$(GLASSBR_DIR)/src/cpp && cp ./datafiles/$(GLASSBR_DIR)/*.txt ./build/$(GLASSBR_DIR)/src/cpp/

prog: $(MOVE_DF_EXAMPLES)

$(filter %$(DOC_P_SUFFIX), $(DOC_PACKAGES)): %$(DOC_P_SUFFIX): check_stack
	stack haddock drasil-$* $(haddockArgs)

docs: $(DOC_PACKAGES)

%$(TEX_E_SUFFIX): EXAMPLE=$(shell echo $* | tr a-z A-Z)
%$(TEX_E_SUFFIX): EDIR=$($(EXAMPLE)_DIR)
$(filter %$(TEX_E_SUFFIX), $(TEX_EXAMPLES)): %$(TEX_E_SUFFIX): %$(GEN_E_SUFFIX)
	EDIR=$(EDIR) SUMMARIZE_TEX=$(SUMMARIZE_TEX) MAKE=$(MAKE) $(SHELL) $(SCRIPT_FOLDER)tex_build.sh

tex: $(TEX_EXAMPLES)

%$(CODE_E_SUFFIX): EXAMPLE=$(shell echo $* | tr a-z A-Z)
%$(CODE_E_SUFFIX): EDIR=$($(EXAMPLE)_DIR)
$(filter %$(CODE_E_SUFFIX), $(CODE_EXAMPLES)): %$(CODE_E_SUFFIX): %$(MOVE_DF_E_SUFFIX)
	EDIR=$(EDIR) MAKE=$(MAKE) $(SHELL) $(SCRIPT_FOLDER)code_build.sh

code: $(CODE_EXAMPLES)

$(filter $(CLEAN_GF_PREFIX)%, $(CLEAN_FOLDERS)): $(CLEAN_GF_PREFIX)%:
	- rm -r ./$*

clean: $(CLEAN_FOLDERS)
	- stack clean
	- rm $(CACHED_MSV_FILE)

.PHONY: clean code tex doc debug prog test check_stack all $(ALL_EXPANDED_TARGETS)
