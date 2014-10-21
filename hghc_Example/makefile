CC= gcc
CFLAGS = -g -c -Wall -ansi

all:	code lpdoc

code: 
	ctangle hghc_LP
	gcc $(CFLAGS) hghc_LP.c

lpdoc:	srs	  
	cweave hghc_LP.w
	pdflatex hghc_LP

srs:
	pdflatex hghc_SRS

clean:
	@-$(RM) *.aux *.idx *.out *.scn *.toc *.c *.log fuel_temp *.o *.bbl *.blg *.gz hghc_LP.tex

