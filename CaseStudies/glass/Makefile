ifdef ComSpec
	RM = del /F /Q
	SEP2 = \\
	SEP1 = $(strip $(SEP2))
	COM_SEP = &
else
	RM = rm -rf
	SEP2 = /
	SEP1 = $(strip $(SEP2))
	COM_SEP = ;
endif

# PY = python3.4 #for systems where version 3 is not the default
PY = python3.4
PYFLAGS = -m
PYDOC = $(PY) -m pydoc
PYDOCFLAGS = -w

DOCDIR = doc

MAIN_MOD = Implementation.mainfun
MAIN_EXE = $(PY) $(PYFLAGS) $(MAIN_MOD)

SOURCE = $(wildcard Implementation$(SEP1)*.py)
MODULE = $(patsubst Implementation$(SEP1)%.py, Implementation.%, $(SOURCE))

TEST_SRC = $(wildcard Test$(SEP1)test*$(SEP1)test*.py)
TEST_MOD = $(subst $(SEP1),.,$(basename $(TEST)))
TEST_INP = $(wildcard Test$(SEP1)Inputfiles$(SEP1)*.txt)
TEST_ALL = Test.allTests

.PHONY: all prog doc test clean

prog : $(SOURCE)
	$(MAIN_EXE) defaultInput.txt

doc :
	mkdir -p $(DOCDIR)
	$(PYDOC) $(PYDOCFLAGS) $(MODULE)
	mv *.html $(DOCDIR)
	@echo The generated files have been moved \
	to a new directory /glass/doc

test :
	@$(foreach TEST, $(TEST_SRC), \
		echo === Running test: $(TEST) $(COM_SEP) \
		$(PY) $(PYFLAGS) unittest $(TEST_MOD) $(COM_SEP) \
	)
	$(PY) $(PYFLAGS) $(TEST_ALL)

all : prog doc test

clean :
	$(RM) *.pyc $(DOCDIR)
