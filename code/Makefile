PACKAGES = lang build code theory database printers gen data docLang example
DPACKAGES = $(addprefix drasil-, $(PACKAGES))
BUILD_PACKAGES = $(addprefix build_, $(PACKAGES))
DOCS_PACKAGES = $(addsuffix -docs, $(PACKAGES))

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
BUILD_EXAMPLES = $(addsuffix _build, $(EXAMPLES))
TEST_EXAMPLES =  $(addsuffix _diff, $(EXAMPLES))
MOVE_DF_EXAMPLES =  $(addsuffix _prog, $(EXAMPLES))
TEX_EXAMPLES = $(addsuffix _tex, $(EXAMPLES))
CODE_EXAMPLES = $(addsuffix _code, $(EXAMPLES))

# make variables/configuration
DIFF = diff --strip-trailing-cr --ignore-all-space -r -X ../.gitignore -x '*.txt'
LOG_SUFFIX = _log.log
MIN_STACK_VER = 1.9.1  # Version which adds --interleaved-output flag
CACHED_MSV_FILE = .drasil-min-stack-ver

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

check_stack: FORCE
	 @MIN_STACK_VER=$(MIN_STACK_VER) CACHED_MSV_FILE=$(CACHED_MSV_FILE) sh check_stack.sh

test: $(BUILD_EXAMPLES) $(TEST_EXAMPLES)
	@echo ----------------------------
	@echo Make complete, checking logs
	@echo ----------------------------
	sh log_check.sh $(NOISY)

prog: $(MOVE_DF_EXAMPLES)

FORCE: ;

debug: stackArgs+=$(PROFALL) 
debug: EXECARGS+=$(PROFEXEC) 
debug: test

$(filter build_%, $(BUILD_PACKAGES)): build_%: check_stack
	stack install -j3 $(stackArgs) drasil-$* --dump-logs --interleaved-output

%_build: EXAMPLE=$(shell echo $* | tr a-z A-Z)
%_build: EDIR=$($(EXAMPLE)_DIR)
%_build: EEXE=$($(EXAMPLE)_EXE)
$(filter %_build, $(BUILD_EXAMPLES)): %_build: build_example
	mkdir -p build/$(EDIR)
	cd build/$(EDIR) && stack exec -- $(EEXE) $(EXECARGS)

%_diff: EXAMPLE=$(shell echo $* | tr a-z A-Z)
%_diff: EDIR=$($(EXAMPLE)_DIR)
$(filter %_diff, $(TEST_EXAMPLES)): %_diff: %_build
	- $(DIFF) ./stable/$*/ ./build/$(EDIR)/ > $(EDIR)$(LOG_SUFFIX) 2>&1

$(filter %_prog, $(MOVE_DF_EXAMPLES)): %_prog: %_build

glassbr_prog: glassbr_build
	test -d ./build/$(GLASSBR_DIR)/src/python && cp ./datafiles/$(GLASSBR_DIR)/*.txt ./build/$(GLASSBR_DIR)/src/python/
	test -d ./build/$(GLASSBR_DIR)/src/java && cp ./datafiles/$(GLASSBR_DIR)/*.txt ./build/$(GLASSBR_DIR)/src/java/
	test -d ./build/$(GLASSBR_DIR)/src/csharp && cp ./datafiles/$(GLASSBR_DIR)/*.txt ./build/$(GLASSBR_DIR)/src/csharp/
	test -d ./build/$(GLASSBR_DIR)/src/cpp && cp ./datafiles/$(GLASSBR_DIR)/*.txt ./build/$(GLASSBR_DIR)/src/cpp/

docs: $(DOCS_PACKAGES)

$(filter %-docs, $(DOCS_PACKAGES)): %-docs: check_stack
	stack haddock drasil-$* $(haddockArgs)

%_tex: EXAMPLE=$(shell echo $* | tr a-z A-Z)
%_tex: EDIR=$($(EXAMPLE)_DIR)
$(filter %_tex, $(TEX_EXAMPLES)): %_tex: %_build
	EDIR=$(EDIR) SUMMARIZE_TEX=$(SUMMARIZE_TEX) MAKE=$(MAKE) sh ./tex_build.sh

tex: $(TEX_EXAMPLES)

%_code: EXAMPLE=$(shell echo $* | tr a-z A-Z)
%_code: EDIR=$($(EXAMPLE)_DIR)
$(filter %_code, $(CODE_EXAMPLES)): %_code: %_prog
	EDIR=$(EDIR) MAKE=$(MAKE) sh ./code_build.sh

code: $(CODE_EXAMPLES)

clean: clean_build
	- stack clean
	- rm $(CACHED_MSV_FILE)

clean_build: clean_logs
	- rm -r ./build

clean_logs:
	- rm -r ./logs
