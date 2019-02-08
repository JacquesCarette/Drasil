TINY_PREF  = Tiny_
GLASS_PREF = GlassBR_
NoPCM_PREF = NoPCM_
SWHS_PREF  = SWHS_
SSP_PREF   = SSP_
GAME_PREF  = Chipmunk_

TINY_DIR  = Tiny
GLASS_DIR = GlassBR
NoPCM_DIR = NoPCM
SWHS_DIR  = SWHS
SSP_DIR   = SSP
GAME_DIR  = Chipmunk

PACKAGES = lang code printers gen data docLang example
DPACKAGES = $(addprefix drasil-, $(PACKAGES))
BUILD_PACKAGES = $(addprefix build_, $(PACKAGES))

TINY_EXE  = tiny
GLASS_EXE = glassbr
NoPCM_EXE = nopcm
SWHS_EXE  = swhs
SSP_EXE   = ssp
GAME_EXE  = chipmunkdocs

DIRS = $(TINY_DIR) $(GLASS_DIR) $(NoPCM_DIR) $(SWHS_DIR) $(SSP_DIR) $(GAME_DIR) $(DPACKAGES)
DIFF = diff --strip-trailing-cr --ignore-all-space

TESTS = tiny_diff glassbr_diff nopcm_diff swhs_diff ssp_diff gamephys_diff
PROGS = tiny_prog glassbr_prog nopcm_prog swhs_prog ssp_prog gamephys_prog

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

tiny_build: build_example
	mkdir -p build/$(TINY_DIR)
	cd build/$(TINY_DIR) && stack exec -- $(TINY_EXE) $(EXECARGS)

glassbr_build: tiny_build
	mkdir -p build/$(GLASS_DIR)
	cd build/$(GLASS_DIR) && stack exec -- $(GLASS_EXE) $(EXECARGS)

nopcm_build: glassbr_build
	mkdir -p build/$(NoPCM_DIR)
	cd build/$(NoPCM_DIR) && stack exec -- $(NoPCM_EXE) $(EXECARGS)

swhs_build: nopcm_build
	mkdir -p build/$(SWHS_DIR)
	cd build/$(SWHS_DIR) && stack exec -- $(SWHS_EXE) $(EXECARGS)

ssp_build: swhs_build
	mkdir -p build/$(SSP_DIR)
	cd build/$(SSP_DIR) && stack exec -- $(SSP_EXE) $(EXECARGS)

gamephys_build: ssp_build
	mkdir -p build/$(GAME_DIR)
	cd build/$(GAME_DIR) && stack exec -- $(GAME_EXE) $(EXECARGS)

tiny_diff: tiny_build
	- $(DIFF) ./stable/tiny/SRS/$(TINY_PREF)SRS.tex ./build/$(TINY_DIR)/SRS/$(TINY_PREF)SRS.tex > $(TINY_PREF)log.log 2>&1
	- $(DIFF) ./stable/tiny/Website/$(TINY_PREF)SRS.css ./build/$(TINY_DIR)/Website/$(TINY_PREF)SRS.css >> $(TINY_PREF)log.log 2>&1
	- $(DIFF) ./stable/tiny/Website/$(TINY_PREF)SRS.html ./build/$(TINY_DIR)/Website/$(TINY_PREF)SRS.html >> $(TINY_PREF)log.log 2>&1

glassbr_diff: glassbr_build
	- $(DIFF) ./stable/glassbr/SRS/$(GLASS_PREF)SRS.tex ./build/$(GLASS_DIR)/SRS/$(GLASS_PREF)SRS.tex > $(GLASS_PREF)log.log 2>&1
	- $(DIFF) ./stable/glassbr/Website/$(GLASS_PREF)SRS.css ./build/$(GLASS_DIR)/Website/$(GLASS_PREF)SRS.css >> $(GLASS_PREF)log.log 2>&1
	- $(DIFF) ./stable/glassbr/Website/$(GLASS_PREF)SRS.html ./build/$(GLASS_DIR)/Website/$(GLASS_PREF)SRS.html >> $(GLASS_PREF)log.log 2>&1

nopcm_diff: nopcm_build
	- $(DIFF) ./stable/nopcm/SRS/$(NoPCM_PREF)SRS.tex ./build/$(NoPCM_DIR)/SRS/$(NoPCM_PREF)SRS.tex > $(NoPCM_PREF)log.log 2>&1
	- $(DIFF) ./stable/nopcm/Website/$(NoPCM_PREF)SRS.css ./build/$(NoPCM_DIR)/Website/$(NoPCM_PREF)SRS.css >> $(NoPCM_PREF)log.log 2>&1
	- $(DIFF) ./stable/nopcm/Website/$(NoPCM_PREF)SRS.html ./build/$(NoPCM_DIR)/Website/$(NoPCM_PREF)SRS.html >> $(NoPCM_PREF)log.log 2>&1

swhs_diff: swhs_build
	- $(DIFF) ./stable/swhs/SRS/$(SWHS_PREF)SRS.tex ./build/$(SWHS_DIR)/SRS/$(SWHS_PREF)SRS.tex > $(SWHS_PREF)log.log 2>&1
	- $(DIFF) ./stable/swhs/Website/$(SWHS_PREF)SRS.css ./build/$(SWHS_DIR)/Website/$(SWHS_PREF)SRS.css >> $(SWHS_PREF)log.log 2>&1
	- $(DIFF) ./stable/swhs/Website/$(SWHS_PREF)SRS.html ./build/$(SWHS_DIR)/Website/$(SWHS_PREF)SRS.html >> $(SWHS_PREF)log.log 2>&1

ssp_diff: ssp_build
	- $(DIFF) ./stable/ssp/SRS/$(SSP_PREF)SRS.tex ./build/$(SSP_DIR)/SRS/$(SSP_PREF)SRS.tex > $(SSP_PREF)log.log 2>&1
	- $(DIFF) ./stable/ssp/Website/$(SSP_PREF)SRS.css ./build/$(SSP_DIR)/Website/$(SSP_PREF)SRS.css >> $(SSP_PREF)log.log 2>&1
	- $(DIFF) ./stable/ssp/Website/$(SSP_PREF)SRS.html ./build/$(SSP_DIR)/Website/$(SSP_PREF)SRS.html >> $(SSP_PREF)log.log 2>&1

gamephys_diff: gamephys_build
	- $(DIFF) ./stable/gamephys/SRS/$(GAME_PREF)SRS.tex ./build/$(GAME_DIR)/SRS/$(GAME_PREF)SRS.tex > $(GAME_PREF)log.log 2>&1
	- $(DIFF) ./stable/gamephys/Website/$(GAME_PREF)SRS.css ./build/$(GAME_DIR)/Website/$(GAME_PREF)SRS.css >> $(GAME_PREF)log.log 2>&1
	- $(DIFF) ./stable/gamephys/Website/$(GAME_PREF)SRS.html ./build/$(GAME_DIR)/Website/$(GAME_PREF)SRS.html >> $(GAME_PREF)log.log 2>&1

tiny_prog: tiny_build

glassbr_prog: glassbr_build
	test -d ./build/$(GLASS_DIR)/src/python && cp ./datafiles/$(GLASS_DIR)/*.txt ./build/$(GLASS_DIR)/src/python/
	test -d ./build/$(GLASS_DIR)/src/java && cp ./datafiles/$(GLASS_DIR)/*.txt ./build/$(GLASS_DIR)/src/java/
	test -d ./build/$(GLASS_DIR)/src/csharp && cp ./datafiles/$(GLASS_DIR)/*.txt ./build/$(GLASS_DIR)/src/csharp/
	test -d ./build/$(GLASS_DIR)/src/cpp && cp ./datafiles/$(GLASS_DIR)/*.txt ./build/$(GLASS_DIR)/src/cpp/

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
