LATEX_CMD = pdflatex
SRC = Thesis_Main.tex

RMDIR = rm -rf

run: build clean

build: 
	$(LATEX_CMD) $(SRC)

clean:	
	@- $(RMDIR) *.ps *.dvi *.aux *.toc *.idx *.ind *.ilg *.log *.out *.brf *.blg *.bbl *.lof *.lot