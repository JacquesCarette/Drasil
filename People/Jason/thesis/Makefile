.PHONY: help system_requirements getting_started build clean

help:
	@echo "Build:"
	@echo "  - build: Build a fresh copy of the thesis in the build/ folder, soft linked to local 'thesis.pdf'."
	@echo "  - clean: Clean working TeX build artifacts."
	@echo ""
	@echo "Supplementary Information:"
	@echo "  - help               : View this help guide."
	@echo "  - system_requirements: List system requirements."
	@echo "  - getting_started    : Information about getting started with this thesis."

system_requirements:
	@echo "System Requirements: LaTeX (latexmk + PDFLaTeX), Pygments"

getting_started:
	@echo "0. Install system requirements ('make system_requirements' will display them)."
	@echo "1. Edit 'latexmkrc' for your timezone."
	@echo "2. Edit 'manifest.tex' for your metadata."
	@echo "3. Add chapter snippets to 'chapters/', and '\input' each one in 'chapters.tex'."
	@echo "4. Add references to 'references.bib'."

build: # '-output-directory=build' is a special name and is referenced from '\usepackage{minted}'region in 'thesis.tex'
	latexmk -output-directory=build -pdf -interaction=nonstopmode thesis.tex --shell-escape

clean:
	rm -rf build/
