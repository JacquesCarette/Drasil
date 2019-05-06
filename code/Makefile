TINY_PREF  = Tiny_
GLASSBR_PREF = GlassBR_
NOPCM_PREF = NoPCM_
SWHS_PREF  = SWHS_
SSP_PREF   = SSP_
GAMEPHYS_PREF  = Chipmunk_

TINY_DIR  = Tiny
GLASSBR_DIR = GlassBR
NOPCM_DIR = NoPCM
SWHS_DIR  = SWHS
SSP_DIR   = SSP
GAMEPHYS_DIR  = Chipmunk

PACKAGES = lang build code printers gen data docLang example
DPACKAGES = $(addprefix drasil-, $(PACKAGES))
BUILD_PACKAGES = $(addprefix build_, $(PACKAGES))
DOCS_PACKAGES = $(addsuffix -docs, $(PACKAGES))

TINY_EXE  = tiny
GLASSBR_EXE = glassbr
NOPCM_EXE = nopcm
SWHS_EXE  = swhs
SSP_EXE   = ssp
GAMEPHYS_EXE  = chipmunkdocs

EXAMPLES = tiny glassbr nopcm swhs ssp gamephys
BUILD_EXAMPLES = $(addsuffix _build, $(EXAMPLES))
TEX_EXAMPLES = $(addsuffix _tex, $(EXAMPLES))
CODE_EXAMPLES = $(addsuffix _code, $(EXAMPLES))

DIRS = $(foreach dir, $(addsuffix _DIR,$(shell echo $(EXAMPLES) | tr a-z A-Z)), $($(dir))) $(DPACKAGES)
DIFF = diff --strip-trailing-cr --ignore-all-space

TESTS =  $(addsuffix _diff, $(EXAMPLES))
PROGS =  $(addsuffix _prog, $(EXAMPLES))

PROFALL = --executable-profiling --library-profiling
PROFEXEC = +RTS -xc -P

GHCTHREADS += 2
override GHCFLAGS += -Wall -j$(GHCTHREADS)
override stackArgs += --ghc-options="$(GHCFLAGS)"

NOISY=no
SUMMARIZE_TEX=no

MIN_STACK_VER = 1.9.1  # Version which adds --interleaved-output flag
CACHED_MSV_FILE = .drasil-min-stack-ver

all: test

check_stack: FORCE
	 @MIN_STACK_VER=$(MIN_STACK_VER) CACHED_MSV_FILE=$(CACHED_MSV_FILE) sh check_stack.sh

test: $(PROGS) $(TESTS)
	@echo ----------------------------
	@echo Make complete, checking logs
	@echo ----------------------------
	sh log_check.sh $(NOISY)

prog: $(PROGS)

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
%_diff: EPREF=$($(EXAMPLE)_PREF)
$(filter %_diff, $(TESTS)): %_diff: %_build
	- $(DIFF) ./stable/$*/SRS/$(EPREF)SRS.tex ./build/$(EDIR)/SRS/$(EPREF)SRS.tex > $(EPREF)log.log 2>&1
	- $(DIFF) ./stable/$*/Website/$(EPREF)SRS.css ./build/$(EDIR)/Website/$(EPREF)SRS.css >> $(EPREF)log.log 2>&1
	- $(DIFF) ./stable/$*/Website/$(EPREF)SRS.html ./build/$(EDIR)/Website/$(EPREF)SRS.html >> $(EPREF)log.log 2>&1

%_prog: PDIR=$($(shell echo $* | tr a-z A-Z)_DIR)
$(filter %_prog, $(PROGS)): %_prog: %_build

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
%_tex: EPREF=$($(EXAMPLE)_PREF)
$(filter %_tex, $(TEX_EXAMPLES)): %_tex: %_build
	EDIR=$(EDIR) EPREF=$(EPREF) SUMMARIZE_TEX=$(SUMMARIZE_TEX) MAKE=$(MAKE) sh ./tex_build.sh

tex: $(TEX_EXAMPLES)

%_code: EXAMPLE=$(shell echo $* | tr a-z A-Z)
%_code: EDIR=$($(EXAMPLE)_DIR)
$(filter %_code, $(CODE_EXAMPLES)): %_code: %_build
	EDIR=$(EDIR) MAKE=$(MAKE) sh ./code_build.sh

code: $(CODE_EXAMPLES)

clean: clean_build
	- stack clean
	- rm $(CACHED_MSV_FILE)

clean_build: clean_logs
	- rm -r ./build

clean_logs:
	- rm *.log
