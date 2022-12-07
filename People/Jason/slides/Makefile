.PHONY: help system_requirements build debug clean open

help:
	@echo "Build:"
	@echo "  - build: Build a fresh copy of the slides in the build/ folder."
	@echo "  - debug: Same as 'build', but pauses build on errors for easier debugging."
	@echo "  - clean: Clean working TeX build artifacts."
	@echo ""
	@echo "Supplementary Information:"
	@echo "  - help               : View this help guide."
	@echo "  - system_requirements: List system requirements."

system_requirements:
	@echo "System Requirements: LaTeX (latexmk + LuaLaTeX), Pygments"

build: # standard build -- '-output-directory=build' is a special name and is referenced from '\usepackage{minted}'region in 'slides.tex'
	latexmk -output-directory=build -pdflatex=lualatex -pdf -interaction=nonstopmode slides.tex --shell-escape

debug: # for finding hard issues, this is an interactive version of 'build'
	latexmk -output-directory=build -pdflatex=lualatex -pdf slides.tex --shell-escape

clean:
	rm -rf build/
	rm -rf svg-inkscape/

open:
	xdg-open build/slides.pdf
