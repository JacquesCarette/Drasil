help:
	@echo "System Requirements: LaTeX (latexmk + PDFLaTeX), Pygments"
	@echo "Targets:"
	@echo "  - help : View this help guide."
	@echo "  - build: Build a fresh copy of the thesis in the build/ folder, soft linked to local 'thesis.pdf'."
	@echo "  - clean: Clean working TeX build artifacts."

build:
	latexmk -output-directory=build -pdf -interaction=nonstopmode thesis.tex --shell-escape

clean:
	rm -rf build
