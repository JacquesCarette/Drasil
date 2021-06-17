all: run

build:
	stack build --ghc-options="-Wall"

run: build
	stack exec

clean:
	-rm -r _site
	-rm -r _cache
	-stack clean

.PHONY: all build clean run