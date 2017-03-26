.SUFFIXES:
.SUFFIXES: .pdf .tex

.tex.pdf :
	pdflatex $<
	pdflatex $<


matlab= /usr/local/MATLAB/R2012a/bin/matlab

FILES =  main.pdf temperature1.pdf temperature2.pdf temperature3.pdf event1.pdf event2.pdf energy1.pdf energy2.pdf energy3.pdf  load_params.pdf plots.pdf output.pdf

all: latex $(FILES)

latex:
	$(matlab) < document.m

pdf: $(FILE)

clean-all:
	rm -f *.dvi *.log *.bak *.aux *.bbl *.blg *.idk *.ps *.eps *.pdf *.toc *.out *.tex *~

clean:
	rm -f *.log *.bak *.aux *.bbl *.blg *.idk *.pdf *.toc *.out *~ 
