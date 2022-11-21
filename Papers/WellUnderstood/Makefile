.PHONY: build clean open

build:
	latexmk -pdflatex=pdflatex -output-directory=build -pdf "wu.tex"

open:
	xdg-open build/wu.pdf

clean:
	rm -rf build/
