LATEX_CMD = pdflatex
LATEX_BIB = bibtex

SRC = Thesis_Main.tex
BIBSRC = Thesis_Main

RMDIR = rm -rf

run: build

build: 
	$(LATEX_CMD) -shell-escape $(SRC)

bib: 
	$(LATEX_BIB) $(BIBSRC)

clean:	
	@- $(RMDIR) *.ps *.dvi *.aux *.toc *.idx *.ind *.ilg *.log *.out *.brf *.blg *.bbl *.lof *.lot

# pdflatex Thesis_Main.tex
# bibtex Thesis_Main
# pdflatex Thesis_Main.tex
# pdflatex Thesis_Main.tex
