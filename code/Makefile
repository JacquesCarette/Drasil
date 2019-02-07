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

PACKAGES = lang code printers gen data docLang example
DPACKAGES = $(addprefix drasil-, $(PACKAGES))
BUILD_PACKAGES = $(addprefix build_, $(PACKAGES))

TINY_EXE  = tiny
GLASSBR_EXE = glassbr
NOPCM_EXE = nopcm
SWHS_EXE  = swhs
SSP_EXE   = ssp
GAMEPHYS_EXE  = chipmunkdocs

EXAMPLES = tiny glassbr nopcm swhs ssp gamephys
BUILD_EXAMPLES = $(addsuffix _build, $(EXAMPLES))

DIRS = $(foreach dir, $(addsuffix _DIR,$(shell echo $(EXAMPLES) | tr a-z A-Z)), $($(dir))) $(DPACKAGES)
DIFF = diff --strip-trailing-cr --ignore-all-space

TESTS =  $(addsuffix _diff, $(EXAMPLES))
PROGS =  $(addsuffix _prog, $(EXAMPLES))

PROFALL = --executable-profiling --library-profiling
PROFEXEC = +RTS -xc -P

NOISY=no

all: test

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

$(filter build_%, $(BUILD_PACKAGES)): build_%: FORCE
	stack install -j3 --ghc-options -j2 $(stackArgs) drasil-$* --dump-logs --interleaved-output

%_build: EXAMPLE=$(shell echo $* | tr a-z A-Z)
%_build: EDIR=$($(EXAMPLE)_DIR)
%_build: EEXE=$($(EXAMPLE)_EXE)
$(filter %_build, $(BUILD_EXAMPLES)): %_build: build_example
	mkdir -p build/$(EDIR)
	cd build/$(EDIR) && stack exec -- $(EEXE) $(EXECARGS)

tiny_diff: tiny_build
	- $(DIFF) ./stable/tiny/SRS/$(TINY_PREF)SRS.tex ./build/$(TINY_DIR)/SRS/$(TINY_PREF)SRS.tex > $(TINY_PREF)log.log 2>&1
	- $(DIFF) ./stable/tiny/Website/$(TINY_PREF)SRS.css ./build/$(TINY_DIR)/Website/$(TINY_PREF)SRS.css >> $(TINY_PREF)log.log 2>&1
	- $(DIFF) ./stable/tiny/Website/$(TINY_PREF)SRS.html ./build/$(TINY_DIR)/Website/$(TINY_PREF)SRS.html >> $(TINY_PREF)log.log 2>&1

glassbr_diff: glassbr_build
	- $(DIFF) ./stable/glassbr/SRS/$(GLASSBR_PREF)SRS.tex ./build/$(GLASSBR_DIR)/SRS/$(GLASSBR_PREF)SRS.tex > $(GLASSBR_PREF)log.log 2>&1
	- $(DIFF) ./stable/glassbr/Website/$(GLASSBR_PREF)SRS.css ./build/$(GLASSBR_DIR)/Website/$(GLASSBR_PREF)SRS.css >> $(GLASSBR_PREF)log.log 2>&1
	- $(DIFF) ./stable/glassbr/Website/$(GLASSBR_PREF)SRS.html ./build/$(GLASSBR_DIR)/Website/$(GLASSBR_PREF)SRS.html >> $(GLASSBR_PREF)log.log 2>&1

nopcm_diff: nopcm_build
	- $(DIFF) ./stable/nopcm/SRS/$(NOPCM_PREF)SRS.tex ./build/$(NOPCM_DIR)/SRS/$(NOPCM_PREF)SRS.tex > $(NOPCM_PREF)log.log 2>&1
	- $(DIFF) ./stable/nopcm/Website/$(NOPCM_PREF)SRS.css ./build/$(NOPCM_DIR)/Website/$(NOPCM_PREF)SRS.css >> $(NOPCM_PREF)log.log 2>&1
	- $(DIFF) ./stable/nopcm/Website/$(NOPCM_PREF)SRS.html ./build/$(NOPCM_DIR)/Website/$(NOPCM_PREF)SRS.html >> $(NOPCM_PREF)log.log 2>&1

swhs_diff: swhs_build
	- $(DIFF) ./stable/swhs/SRS/$(SWHS_PREF)SRS.tex ./build/$(SWHS_DIR)/SRS/$(SWHS_PREF)SRS.tex > $(SWHS_PREF)log.log 2>&1
	- $(DIFF) ./stable/swhs/Website/$(SWHS_PREF)SRS.css ./build/$(SWHS_DIR)/Website/$(SWHS_PREF)SRS.css >> $(SWHS_PREF)log.log 2>&1
	- $(DIFF) ./stable/swhs/Website/$(SWHS_PREF)SRS.html ./build/$(SWHS_DIR)/Website/$(SWHS_PREF)SRS.html >> $(SWHS_PREF)log.log 2>&1

ssp_diff: ssp_build
	- $(DIFF) ./stable/ssp/SRS/$(SSP_PREF)SRS.tex ./build/$(SSP_DIR)/SRS/$(SSP_PREF)SRS.tex > $(SSP_PREF)log.log 2>&1
	- $(DIFF) ./stable/ssp/Website/$(SSP_PREF)SRS.css ./build/$(SSP_DIR)/Website/$(SSP_PREF)SRS.css >> $(SSP_PREF)log.log 2>&1
	- $(DIFF) ./stable/ssp/Website/$(SSP_PREF)SRS.html ./build/$(SSP_DIR)/Website/$(SSP_PREF)SRS.html >> $(SSP_PREF)log.log 2>&1

gamephys_diff: gamephys_build
	- $(DIFF) ./stable/gamephys/SRS/$(GAMEPHYS_PREF)SRS.tex ./build/$(GAMEPHYS_DIR)/SRS/$(GAMEPHYS_PREF)SRS.tex > $(GAMEPHYS_PREF)log.log 2>&1
	- $(DIFF) ./stable/gamephys/Website/$(GAMEPHYS_PREF)SRS.css ./build/$(GAMEPHYS_DIR)/Website/$(GAMEPHYS_PREF)SRS.css >> $(GAMEPHYS_PREF)log.log 2>&1
	- $(DIFF) ./stable/gamephys/Website/$(GAMEPHYS_PREF)SRS.html ./build/$(GAMEPHYS_DIR)/Website/$(GAMEPHYS_PREF)SRS.html >> $(GAMEPHYS_PREF)log.log 2>&1

tiny_prog: tiny_build

glassbr_prog: glassbr_build
	test -d ./build/$(GLASSBR_DIR)/src/python && cp ./datafiles/$(GLASSBR_DIR)/*.txt ./build/$(GLASSBR_DIR)/src/python/
	test -d ./build/$(GLASSBR_DIR)/src/java && cp ./datafiles/$(GLASSBR_DIR)/*.txt ./build/$(GLASSBR_DIR)/src/java/
	test -d ./build/$(GLASSBR_DIR)/src/csharp && cp ./datafiles/$(GLASSBR_DIR)/*.txt ./build/$(GLASSBR_DIR)/src/csharp/
	test -d ./build/$(GLASSBR_DIR)/src/cpp && cp ./datafiles/$(GLASSBR_DIR)/*.txt ./build/$(GLASSBR_DIR)/src/cpp/

nopcm_prog: nopcm_build

swhs_prog: swhs_build

ssp_prog: ssp_build

gamephys_prog: gamephys_build

docs: lang-docs code-docs data-docs example-docs gen-docs printers-docs 

lang-docs:
	stack haddock drasil-lang $(haddockArgs)

code-docs:
	stack haddock drasil-code $(haddockArgs)

data-docs:
	stack haddock drasil-data $(haddockArgs)

example-docs:
	stack haddock drasil-example $(haddockArgs)

gen-docs:
	stack haddock drasil-gen $(haddockArgs)

printers-docs:
	stack haddock drasil-printers $(haddockArgs)

docLang-docs:
	stack haddock drasil-docLang $(haddockArgs)

clean: clean_build
	- stack clean

clean_build: clean_logs
	- rm -r ./build

clean_logs:
	- rm *.log
