SWHS_PREF = SWHS_
NoPCM_PREF = NoPCM_
TINY_PREF = Tiny_
SSP_PREF = SSP_
GLASS_PREF = GlassBR_
GAME_PREF = Chipmunk_

SWHS_DIR = SWHS
NoPCM_DIR = NoPCM
TINY_DIR = Tiny
SSP_DIR = SSP
GLASS_DIR = GlassBR
GAME_DIR = Chipmunk

LANG_DIR  = drasil-lang
CODE_DIR  = drasil-code
PRINT_DIR = drasil-printers
GEN_DIR   = drasil-gen
DATA_DIR  = drasil-data
DOCL_DIR  = drasil-docLang
EXAM_DIR  = drasil-example

GAME_EXE = chipmunkdocs
TINY_EXE = tiny
SWHS_EXE = swhs
NoPCM_EXE  = nopcm
SSP_EXE  = ssp
GLASS_EXE= glassbr

DIRS = $(SWHS_DIR) $(NoPCM_DIR) $(TINY_DIR) $(SSP_DIR) $(GLASS_DIR) $(GAME_DIR) $(DATA_DIR) $(LANG_DIR) $(DOCL_DIR) $(EXAM_DIR) $(CODE_DIR) $(GEN_DIR) $(PRINT_DIR)
DIFF = diff --strip-trailing-cr --ignore-all-space

TESTS = swhs_diff tiny_diff ssp_diff glassbr_diff gamephys_diff nopcm_diff
PROGS = swhs_prog tiny_prog ssp_prog glassbr_prog gamephys_prog nopcm_prog

all: build build_code build_data test

test: $(PROGS) $(TESTS)
	@echo ----------------------------
	@echo Make complete, checking logs
	@echo ----------------------------
	sh log_check.sh

prog: $(PROGS)

FORCE: ;

build: FORCE
	mkdir -p $(LANG_DIR)
	cd $(LANG_DIR) && stack build -j2 $(stackArgs) drasil-lang
	stack install $(stackArgs) drasil-lang

build_code: build
	mkdir -p $(CODE_DIR)
	cd $(CODE_DIR) && stack build -j2 $(stackArgs) drasil-code
	stack install $(stackArgs) drasil-code

build_print: build_code
	mkdir -p $(PRINT_DIR)
	cd $(PRINT_DIR) && stack build -j2 $(stackArgs) drasil-printers
	stack install $(stackArgs) drasil-printers

build_gen: build_print
	mkdir -p $(GEN_DIR)
	cd $(GEN_DIR) && stack build -j2 $(stackArgs) drasil-gen
	stack install $(stackArgs) drasil-gen

build_data: build_gen
	mkdir -p $(DATA_DIR)
	cd $(DATA_DIR) && stack build -j2 $(stackArgs) drasil-data
	stack install $(stackArgs) drasil-data

build_docl: build_data
	mkdir -p $(DOCL_DIR)
	cd $(DOCL_DIR) && stack build -j2 $(stackArgs) drasil-docLang
	stack install $(stackArgs) drasil-docLang

build_exam: build_docl
	mkdir -p $(EXAM_DIR)
	cd $(EXAM_DIR) && stack build -j2 $(stackArgs) drasil-example
	stack install $(stackArgs) drasil-example --force-dirty

swhs_build: build_exam
	mkdir -p build/$(SWHS_DIR)
	cd build/$(SWHS_DIR) && stack exec $(SWHS_EXE)

tiny_build: build_exam
	mkdir -p build/$(TINY_DIR)
	cd build/$(TINY_DIR) && stack exec $(TINY_EXE)

ssp_build: build_exam
	mkdir -p build/$(SSP_DIR)
	cd build/$(SSP_DIR) && stack exec $(SSP_EXE)

glassbr_build: build_exam
	mkdir -p build/$(GLASS_DIR)
	cd build/$(GLASS_DIR) && stack exec $(GLASS_EXE)

gamephys_build: build_exam
	mkdir -p build/$(GAME_DIR)
	cd build/$(GAME_DIR) && stack exec $(GAME_EXE)

nopcm_build: build_exam
	mkdir -p build/$(NoPCM_DIR)
	cd build/$(NoPCM_DIR) && stack exec $(NoPCM_EXE)

swhs_diff: swhs_build
	- $(DIFF) ./stable/swhs/SRS/$(SWHS_PREF)SRS.tex ./build/$(SWHS_DIR)/SRS/$(SWHS_PREF)SRS.tex > $(SWHS_PREF)log.log 2>&1
	- $(DIFF) ./stable/swhs/Website/$(SWHS_PREF)SRS.css ./build/$(SWHS_DIR)/Website/$(SWHS_PREF)SRS.css >> $(SWHS_PREF)log.log 2>&1
	- $(DIFF) ./stable/swhs/Website/$(SWHS_PREF)SRS.html ./build/$(SWHS_DIR)/Website/$(SWHS_PREF)SRS.html >> $(SWHS_PREF)log.log 2>&1

tiny_diff: tiny_build
	- $(DIFF) ./stable/tiny/SRS/$(TINY_PREF)SRS.tex ./build/$(TINY_DIR)/SRS/$(TINY_PREF)SRS.tex > $(TINY_PREF)log.log 2>&1
	- $(DIFF) ./stable/tiny/Website/$(TINY_PREF)SRS.css ./build/$(TINY_DIR)/Website/$(TINY_PREF)SRS.css >> $(TINY_PREF)log.log 2>&1
	- $(DIFF) ./stable/tiny/Website/$(TINY_PREF)SRS.html ./build/$(TINY_DIR)/Website/$(TINY_PREF)SRS.html >> $(TINY_PREF)log.log 2>&1

ssp_diff: ssp_build
	- $(DIFF) ./stable/ssp/SRS/$(SSP_PREF)SRS.tex ./build/$(SSP_DIR)/SRS/$(SSP_PREF)SRS.tex > $(SSP_PREF)log.log 2>&1
	- $(DIFF) ./stable/ssp/Website/$(SSP_PREF)SRS.css ./build/$(SSP_DIR)/Website/$(SSP_PREF)SRS.css >> $(SSP_PREF)log.log 2>&1
	- $(DIFF) ./stable/ssp/Website/$(SSP_PREF)SRS.html ./build/$(SSP_DIR)/Website/$(SSP_PREF)SRS.html >> $(SSP_PREF)log.log 2>&1

glassbr_diff: glassbr_build
	- $(DIFF) ./stable/glassbr/SRS/$(GLASS_PREF)SRS.tex ./build/$(GLASS_DIR)/SRS/$(GLASS_PREF)SRS.tex > $(GLASS_PREF)log.log 2>&1
	- $(DIFF) ./stable/glassbr/Website/$(GLASS_PREF)SRS.css ./build/$(GLASS_DIR)/Website/$(GLASS_PREF)SRS.css >> $(GLASS_PREF)log.log 2>&1
	- $(DIFF) ./stable/glassbr/Website/$(GLASS_PREF)SRS.html ./build/$(GLASS_DIR)/Website/$(GLASS_PREF)SRS.html >> $(GLASS_PREF)log.log 2>&1

gamephys_diff: gamephys_build
	- $(DIFF) ./stable/gamephys/SRS/$(GAME_PREF)SRS.tex ./build/$(GAME_DIR)/SRS/$(GAME_PREF)SRS.tex > $(GAME_PREF)log.log 2>&1
	- $(DIFF) ./stable/gamephys/Website/$(GAME_PREF)SRS.css ./build/$(GAME_DIR)/Website/$(GAME_PREF)SRS.css >> $(GAME_PREF)log.log 2>&1
	- $(DIFF) ./stable/gamephys/Website/$(GAME_PREF)SRS.html ./build/$(GAME_DIR)/Website/$(GAME_PREF)SRS.html >> $(GAME_PREF)log.log 2>&1

nopcm_diff: nopcm_build
	- $(DIFF) ./stable/nopcm/SRS/$(NoPCM_PREF)SRS.tex ./build/$(NoPCM_DIR)/SRS/$(NoPCM_PREF)SRS.tex > $(NoPCM_PREF)log.log 2>&1
	- $(DIFF) ./stable/nopcm/Website/$(NoPCM_PREF)SRS.css ./build/$(NoPCM_DIR)/Website/$(NoPCM_PREF)SRS.css >> $(NoPCM_PREF)log.log 2>&1
	- $(DIFF) ./stable/nopcm/Website/$(NoPCM_PREF)SRS.html ./build/$(NoPCM_DIR)/Website/$(NoPCM_PREF)SRS.html >> $(NoPCM_PREF)log.log 2>&1

swhs_prog: swhs_build

tiny_prog: tiny_build

ssp_prog: ssp_build

glassbr_prog: glassbr_build
	test -d ./build/$(GLASS_DIR)/src/python && cp ./datafiles/$(GLASS_DIR)/*.txt ./build/$(GLASS_DIR)/src/python/
	test -d ./build/$(GLASS_DIR)/src/java && cp ./datafiles/$(GLASS_DIR)/*.txt ./build/$(GLASS_DIR)/src/java/
	test -d ./build/$(GLASS_DIR)/src/csharp && cp ./datafiles/$(GLASS_DIR)/*.txt ./build/$(GLASS_DIR)/src/csharp/
	test -d ./build/$(GLASS_DIR)/src/cpp && cp ./datafiles/$(GLASS_DIR)/*.txt ./build/$(GLASS_DIR)/src/cpp/

gamephys_prog: gamephys_build

nopcm_prog: nopcm_build

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
	- cd $(LANG_DIR)  && stack clean
	- cd $(CODE_DIR)  && stack clean
	- cd $(PRINT_DIR) && stack clean
	- cd $(GEN_DIR)   && stack clean
	- cd $(DATA_DIR)  && stack clean
	- cd $(DOCL_DIR)  && stack clean
	- cd $(EXAM_DIR)  && stack clean

clean_build: clean_logs
	- rm -r ./build

clean_logs:
	- rm *.log
