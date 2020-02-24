# For documentation on how to add to this, see Issue #1371

SHELL=/bin/bash

# List of all known packages
PACKAGES = lang utils build gool code theory database printers gen data docLang example

# and associated package-level suffixes
BUILD_P_SUFFIX = _build
DOC_P_SUFFIX = _doc
GRAPH_P_SUFFIX = _graph

BUILD_PACKAGES = $(addsuffix $(BUILD_P_SUFFIX), $(PACKAGES))
DOC_PACKAGES = $(addsuffix $(DOC_P_SUFFIX), $(PACKAGES))
GRAPH_PACKAGES = $(addsuffix $(GRAPH_P_SUFFIX), $(PACKAGES))

PACKAGE_GEN_TARGET = BUILD DOC GRAPH

#####

# Current list of examples
SRC_EXAMPLES = glassbr nopcm projectile
EXAMPLES = $(SRC_EXAMPLES) hghc swhs ssp gamephysics template

# where they live
HGHC_DIR  = HGHC
GLASSBR_DIR = GlassBR
NOPCM_DIR = NoPCM
SWHS_DIR  = SWHS
SSP_DIR   = SSP
GAMEPHYSICS_DIR  = GamePhysics
PROJECTILE_DIR = Projectile
TEMPLATE_DIR = Template

MULTI_SRC_DIRS = $(PROJECTILE_DIR) # Directories for examples with multiple src versions

# and the name of their executables
HGHC_EXE  = hghc
GLASSBR_EXE = glassbr
NOPCM_EXE = nopcm
SWHS_EXE  = swhs
SSP_EXE   = ssp
GAMEPHYSICS_EXE  = gamephysics
PROJECTILE_EXE = projectile
TEMPLATE_EXE = template

# and the associated per-example information generated
GEN_E_SUFFIX = _gen
TEST_E_SUFFIX = _diff
TEX_E_SUFFIX = _tex
CODE_E_SUFFIX = _code
DCP_E_SUFFIX = _deploy_code_path

GEN_EXAMPLES = $(addsuffix $(GEN_E_SUFFIX), $(EXAMPLES))
TEST_EXAMPLES =  $(addsuffix $(TEST_E_SUFFIX), $(EXAMPLES))
TEX_EXAMPLES = $(addsuffix $(TEX_E_SUFFIX), $(EXAMPLES))
CODE_EXAMPLES = $(addsuffix $(CODE_E_SUFFIX), $(SRC_EXAMPLES))
DCP_EXAMPLES = $(addsuffix $(DCP_E_SUFFIX), $(EXAMPLES))

EXAMPLE_GEN_TARGET = GEN TEST TEX CODE DCP

GENNED_FOLDERS = $(LOG_FOLDER_NAME) $(BUILD_FOLDER_NAME) $(GRAPH_FOLDER_NAME) $(DEPLOY_FOLDER_NAME)
CLEAN_GF_PREFIX = clean_
CLEAN_FOLDERS = $(addprefix $(CLEAN_GF_PREFIX), $(GENNED_FOLDERS))

ALL_EXPANDED_TARGETS = $(foreach P, $(PACKAGE_GEN_TARGET), $($(P)_PACKAGES)) $(foreach E, $(EXAMPLE_GEN_TARGET), $($(E)_EXAMPLES)) $(CLEAN_FOLDERS)

#####

# make variables/configuration
DIFF = diff --strip-trailing-cr -r -X ../.gitignore -x '*.txt'
LOG_SUFFIX = _log.log
MIN_STACK_VER = 1.9.1  # Version which adds --interleaved-output flag
CACHED_MSV_FILE = .drasil-min-stack-ver
BUILD_FOLDER_NAME = build
BUILD_FOLDER = $(BUILD_FOLDER_NAME)/
EXAMPLE_CODE_SUBFOLDER = src/
LOG_FOLDER_NAME = logs
LOG_FOLDER = $(LOG_FOLDER_NAME)/
SCRIPT_FOLDER = scripts/
GRAPH_FOLDER_NAME = graphs
GRAPH_FOLDER = $(GRAPH_FOLDER_NAME)/
DEPLOY_FOLDER_NAME = deploy
DEPLOY_FOLDER = $(DEPLOY_FOLDER_NAME)/
DEPLOY_CODE_PATH_KV_SEP = :
# make command line options
#  GHC debug options
PROFALL = --executable-profiling --library-profiling
PROFEXEC = +RTS -xc -P

#  GHC build options
GHCTHREADS += 2
override GHCFLAGS += -Wall -j$(GHCTHREADS)
override stackArgs += -j3 --ghc-options="$(GHCFLAGS)" --dump-logs --interleaved-output

#  Output amount control
NOISY=no
SUMMARIZE_TEX=no

# actual targets
all: test

debug: stackArgs+=$(PROFALL) 
debug: EXECARGS+=$(PROFEXEC) 
debug: test

# sanity check -- are we using the right stack version?
check_stack:
	 @MIN_STACK_VER="$(MIN_STACK_VER)" CACHED_MSV_FILE="$(CACHED_MSV_FILE)" "$(SHELL)" "$(SCRIPT_FOLDER)"check_stack.sh

check_dot:
	@BIN=dot DOWNLOAD_LOCATION=https://www.graphviz.org/download/ "$(SHELL)" "$(SCRIPT_FOLDER)"check_binary.sh

packages: $(BUILD_PACKAGES)

# Actually build all the packages
$(filter %$(BUILD_P_SUFFIX), $(BUILD_PACKAGES)): %$(BUILD_P_SUFFIX): check_stack
	stack install $(stackArgs) "drasil-$*"

%$(GEN_E_SUFFIX): EXAMPLE=$(shell echo $* | tr a-z A-Z)
%$(GEN_E_SUFFIX): EDIR=$($(EXAMPLE)_DIR)
%$(GEN_E_SUFFIX): EEXE=$($(EXAMPLE)_EXE)
$(filter %$(GEN_E_SUFFIX), $(GEN_EXAMPLES)): %$(GEN_E_SUFFIX): example$(BUILD_P_SUFFIX)
	@mkdir -p "$(BUILD_FOLDER)$(EDIR)"
	cd "$(BUILD_FOLDER)$(EDIR)" && stack exec -- "$(EEXE)" $(EXECARGS)

%$(TEST_E_SUFFIX): EXAMPLE=$(shell echo $* | tr a-z A-Z)
%$(TEST_E_SUFFIX): EDIR=$($(EXAMPLE)_DIR)
$(filter %$(TEST_E_SUFFIX), $(TEST_EXAMPLES)): %$(TEST_E_SUFFIX): %$(GEN_E_SUFFIX)
	@mkdir -p "$(LOG_FOLDER)"
	- $(DIFF) "stable/$*/" "$(BUILD_FOLDER)$(EDIR)"/ > "$(LOG_FOLDER)$(EDIR)$(LOG_SUFFIX)" 2>&1

# actually run the tests
test: $(TEST_EXAMPLES)
	@echo ----------------------------
	@echo Make complete, checking logs
	@echo ----------------------------
	@LOG_FOLDER="$(LOG_FOLDER)" LOG_SUFFIX="$(LOG_SUFFIX)" NOISY=$(NOISY) "$(SHELL)" "$(SCRIPT_FOLDER)"log_check.sh

prog: $(GEN_EXAMPLES)

# generate pretty dot graphs too
graphmod: check_stack
	stack install dotgen graphmod

$(filter %$(GRAPH_P_SUFFIX), $(GRAPH_PACKAGES)): %$(GRAPH_P_SUFFIX): check_stack check_dot graphmod
	@mkdir -p $(GRAPH_FOLDER)
	find "drasil-$*" -name '*.hs' -print | grep -v stack | xargs stack exec -- graphmod -q -p --no-cluster | dot -Tpdf > "$(GRAPH_FOLDER)drasil-$*".pdf

graphs: $(GRAPH_PACKAGES)
###

# use stack to build the documentation too
$(filter %$(DOC_P_SUFFIX), $(DOC_PACKAGES)): %$(DOC_P_SUFFIX): check_stack
	stack haddock "drasil-$*" $(haddockArgs)

docs: $(DOC_PACKAGES)

%$(TEX_E_SUFFIX): EXAMPLE=$(shell echo $* | tr a-z A-Z)
%$(TEX_E_SUFFIX): EDIR=$($(EXAMPLE)_DIR)
$(filter %$(TEX_E_SUFFIX), $(TEX_EXAMPLES)): %$(TEX_E_SUFFIX): %$(GEN_E_SUFFIX)
	EDIR="$(EDIR)" BUILD_FOLDER="$(BUILD_FOLDER)" SUMMARIZE_TEX=$(SUMMARIZE_TEX) MAKE="$(MAKE)" "$(SHELL)" "$(SCRIPT_FOLDER)"tex_build.sh

tex: $(TEX_EXAMPLES)

%$(CODE_E_SUFFIX): EXAMPLE=$(shell echo $* | tr a-z A-Z)
%$(CODE_E_SUFFIX): EDIR=$($(EXAMPLE)_DIR)
$(filter %$(CODE_E_SUFFIX), $(CODE_EXAMPLES)): %$(CODE_E_SUFFIX): %$(GEN_E_SUFFIX)
	@EDIR="$(EDIR)" BUILD_FOLDER="$(BUILD_FOLDER)" EXAMPLE_CODE_SUBFOLDER="$(EXAMPLE_CODE_SUBFOLDER)" \
	MULTI_SRC_DIRS="$(MULTI_SRC_DIRS)" TARGET=$(TARGET) \
	MAKE="$(MAKE)" "$(SHELL)" "$(SCRIPT_FOLDER)"code_build.sh

code: $(CODE_EXAMPLES)

doxygen: TARGET=doc
doxygen: $(CODE_EXAMPLES)

%$(DCP_E_SUFFIX): EXAMPLE=$(shell echo $* | tr a-z A-Z)
%$(DCP_E_SUFFIX): EDIR=$($(EXAMPLE)_DIR)
$(filter %$(DCP_E_SUFFIX), $(DCP_EXAMPLES)): %$(DCP_E_SUFFIX):
	@EDIR="$(EDIR)" BUILD_FOLDER="$(BUILD_FOLDER)" EXAMPLE_CODE_SUBFOLDER="$(EXAMPLE_CODE_SUBFOLDER)" EXAMPLE=$* \
	MULTI_SRC_DIRS="$(MULTI_SRC_DIRS)" DEPLOY_CODE_PATH_KV_SEP="$(DEPLOY_CODE_PATH_KV_SEP)" \
	"$(SHELL)" "$(SCRIPT_FOLDER)"code_path.sh

deploy_code_path: $(DCP_EXAMPLES)

deploy_lite:
	@mkdir -p "$(DEPLOY_FOLDER)"
	@BUILD_FOLDER="$(BUILD_FOLDER)" DEPLOY_FOLDER="$(DEPLOY_FOLDER)" GRAPH_FOLDER="$(GRAPH_FOLDER)" \
	MULTI_SRC_DIRS="$(MULTI_SRC_DIRS)" DEPLOY_CODE_PATH_KV_SEP="$(DEPLOY_CODE_PATH_KV_SEP)" MAKE="$(MAKE)" "$(SHELL)" "$(SCRIPT_FOLDER)"deploy_stage.sh

# This rule is for use with developing deployment layout locally. As part of it, it ensures all needed
# dependencies exist. One of the downsides is we ensure all files are re-generated which means the TeX
# files appear newer and thus PDF's are regenerated. If you want to "just generate the structure,
# everything exists," (or you've run `deploy` once already) then `deploy_lite` does just that and is
# what `deploy.bash` calls. 
deploy: graphs docs tex doxygen
	"$(MAKE)" deploy_lite


# follow two are for cleaning things out
$(filter $(CLEAN_GF_PREFIX)%, $(CLEAN_FOLDERS)): $(CLEAN_GF_PREFIX)%:
	- rm -r "./$*"

clean: $(CLEAN_FOLDERS)
	- stack clean
	- rm "$(CACHED_MSV_FILE)"

.PHONY: clean code tex doc debug prog test graphs graphmod check_stack graphs deploy_code_path deploy deploy_lite all $(ALL_EXPANDED_TARGETS)
