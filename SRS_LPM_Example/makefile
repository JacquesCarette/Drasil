CC= gcc
CFLAGS = -g -c -Wall -ansi

code: 
	ctangle FP_SRS_LPM
	gcc $(CFLAGS) fuel_temp.c
pdf:	  
	cweave FP_SRS_LPM.w
	pdflatex FP_SRS_LPM
	bibtex FP_SRS_LPM        
	pdflatex FP_SRS_LPM

clean:
	@-$(RM) *.aux *.idx *.out *.scn *.toc *.c *.log fuel_temp *.o *.bbl *.blg

