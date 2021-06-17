all: run

build:
	stack build --ghc-options="-Wall" website

run: build
	stack exec --website

clean:
	-rm -r _site
	-rm -r _cache
	-stack clean

.PHONY: all build clean run