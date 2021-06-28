all: run

build:
	stack build --ghc-options="-Wall"

run: build
	stack exec -- website

clean:
	-rm -rf ./Website
	-stack clean

.PHONY: all build clean run