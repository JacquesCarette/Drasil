SWHS_PREF = SWHS_
NoPCM_PREF = NoPCM_
TINY_PREF = $(empty)
SSP_PREF = SSP_
GLASS_PREF = GlassBR_
GAME_PREF = Chipmunk_

GAME_EXE = chipmunkdocs
TINY_EXE = tiny
SWHS_EXE = swhs
NoPCM_EXE  = nopcm
SSP_EXE  = ssp
GLASS_EXE= glassbr


DIFF = diff --strip-trailing-cr

TESTS = swhs tiny ssp glassbr gamephys nopcm

all: $(TESTS)
	sh log_check.sh

swhs:
	stack build :$(SWHS_EXE)
	stack exec $(SWHS_EXE)
	- $(DIFF) ./stable/swhs/$(SWHS_PREF)SRS.tex ./SRS/$(SWHS_PREF)SRS.tex &> $(SWHS_PREF)log.log
	- $(DIFF) ./stable/swhs/$(SWHS_PREF)MG.tex ./MG/$(SWHS_PREF)MG.tex >> $(SWHS_PREF)log.log 2>&1
	- $(DIFF) ./stable/swhs/$(SWHS_PREF)SRS.css ./Website/$(SWHS_PREF)SRS.css >> $(SWHS_PREF)log.log 2>&1
	- $(DIFF) ./stable/swhs/$(SWHS_PREF)SRS.html ./Website/$(SWHS_PREF)SRS.html >> $(SWHS_PREF)log.log 2>&1
	
tiny:
	stack build :$(TINY_EXE)
	stack exec $(TINY_EXE)
	- $(DIFF) ./stable/tiny/$(TINY_PREF)SRS.tex ./SRS/$(TINY_PREF)SRS.tex &> $(TINY_PREF)log.log
	- $(DIFF) ./stable/tiny/$(TINY_PREF)MG.tex ./MG/$(TINY_PREF)MG.tex >> $(TINY_PREF)log.log 2>&1
	- $(DIFF) ./stable/tiny/$(TINY_PREF)SRS.css ./Website/$(TINY_PREF)SRS.css >> $(TINY_PREF)log.log 2>&1
	- $(DIFF) ./stable/tiny/$(TINY_PREF)SRS.html ./Website/$(TINY_PREF)SRS.html >> $(TINY_PREF)log.log 2>&1
	
ssp:
	stack build :$(SSP_EXE)
	stack exec $(SSP_EXE)
	- $(DIFF) ./stable/ssp/$(SSP_PREF)SRS.tex ./SRS/$(SSP_PREF)SRS.tex &> $(SSP_PREF)log.log
	- $(DIFF) ./stable/ssp/$(SSP_PREF)MG.tex ./MG/$(SSP_PREF)MG.tex >> $(SSP_PREF)log.log 2>&1
	- $(DIFF) ./stable/ssp/$(SSP_PREF)SRS.css ./Website/$(SSP_PREF)SRS.css >> $(SSP_PREF)log.log 2>&1
	- $(DIFF) ./stable/ssp/$(SSP_PREF)SRS.html ./Website/$(SSP_PREF)SRS.html >> $(SSP_PREF)log.log 2>&1
	
glassbr:
	stack build :$(GLASS_EXE)
	stack exec $(GLASS_EXE)
	- $(DIFF) ./stable/glassbr/$(GLASS_PREF)SRS.tex ./SRS/$(GLASS_PREF)SRS.tex &> $(GLASS_PREF)log.log
	- $(DIFF) ./stable/glassbr/$(GLASS_PREF)MG.tex ./MG/$(GLASS_PREF)MG.tex >> $(GLASS_PREF)log.log 2>&1
	- $(DIFF) ./stable/glassbr/$(GLASS_PREF)SRS.css ./Website/$(GLASS_PREF)SRS.css >> $(GLASS_PREF)log.log 2>&1
	- $(DIFF) ./stable/glassbr/$(GLASS_PREF)SRS.html ./Website/$(GLASS_PREF)SRS.html >> $(GLASS_PREF)log.log 2>&1
	
gamephys:
	stack build :$(GAME_EXE)
	stack exec $(GAME_EXE)
	- $(DIFF) ./stable/gamephys/$(GAME_PREF)SRS.tex ./SRS/$(GAME_PREF)SRS.tex &> $(GAME_PREF)log.log
	- $(DIFF) ./stable/gamephys/$(GAME_PREF)MG.tex ./MG/$(GAME_PREF)MG.tex >> $(GAME_PREF)log.log 2>&1
	- $(DIFF) ./stable/gamephys/$(GAME_PREF)SRS.css ./Website/$(GAME_PREF)SRS.css >> $(GAME_PREF)log.log 2>&1
	- $(DIFF) ./stable/gamephys/$(GAME_PREF)SRS.html ./Website/$(GAME_PREF)SRS.html >> $(GAME_PREF)log.log 2>&1
	
nopcm:
	stack build :$(NoPCM_EXE)
	stack exec $(NoPCM_EXE)
	- $(DIFF) ./stable/nopcm/$(NoPCM_PREF)SRS.tex ./SRS/$(NoPCM_PREF)SRS.tex &> $(NoPCM_PREF)log.log
	- $(DIFF) ./stable/nopcm/$(NoPCM_PREF)SRS.css ./Website/$(NoPCM_PREF)SRS.css >> $(NoPCM_PREF)log.log 2>&1
	- $(DIFF) ./stable/nopcm/$(NoPCM_PREF)SRS.html ./Website/$(NoPCM_PREF)SRS.html >> $(NoPCM_PREF)log.log 2>&1
  
docs:
	stack haddock drasil
	
clean: clean_logs
	rm -r ./SRS ./Website ./MIS ./MG ./C++ .stack-work
	
clean_logs:
	rm *.log
