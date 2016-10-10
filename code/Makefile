SWHS_PREF = SWHS_
PCM_PREF = PCM_
TINY_PREF = $(empty)
SSP_PREF = SSP_
GLASS_PREF = GlassBR_
GAME_PREF = Chipmunk_

DIFF = diff --strip-trailing-cr

TESTS = swhs tiny ssp glassbr gamephys pcm

all: $(TESTS)
	sh log_check.sh

swhs:
	cabal build swhs
	./dist/build/swhs/swhs
	- $(DIFF) ./stable/swhs/$(SWHS_PREF)SRS.tex ./SRS/$(SWHS_PREF)SRS.tex &> $(SWHS_PREF)log.log
	- $(DIFF) ./stable/swhs/$(SWHS_PREF)MG.tex ./MG/$(SWHS_PREF)MG.tex >> $(SWHS_PREF)log.log 2>&1
	- $(DIFF) ./stable/swhs/$(SWHS_PREF)SRS.css ./Website/$(SWHS_PREF)SRS.css >> $(SWHS_PREF)log.log 2>&1
	- $(DIFF) ./stable/swhs/$(SWHS_PREF)SRS.html ./Website/$(SWHS_PREF)SRS.html >> $(SWHS_PREF)log.log 2>&1
	
tiny:
	cabal build tiny
	./dist/build/tiny/tiny
	- $(DIFF) ./stable/tiny/$(TINY_PREF)SRS.tex ./SRS/$(TINY_PREF)SRS.tex &> $(TINY_PREF)log.log
	- $(DIFF) ./stable/tiny/$(TINY_PREF)MG.tex ./MG/$(TINY_PREF)MG.tex >> $(TINY_PREF)log.log 2>&1
	- $(DIFF) ./stable/tiny/$(TINY_PREF)SRS.css ./Website/$(TINY_PREF)SRS.css >> $(TINY_PREF)log.log 2>&1
	- $(DIFF) ./stable/tiny/$(TINY_PREF)SRS.html ./Website/$(TINY_PREF)SRS.html >> $(TINY_PREF)log.log 2>&1
	
ssp:
	cabal build ssp
	./dist/build/ssp/ssp
	- $(DIFF) ./stable/ssp/$(SSP_PREF)SRS.tex ./SRS/$(SSP_PREF)SRS.tex &> $(SSP_PREF)log.log
	- $(DIFF) ./stable/ssp/$(SSP_PREF)MG.tex ./MG/$(SSP_PREF)MG.tex >> $(SSP_PREF)log.log 2>&1
	- $(DIFF) ./stable/ssp/$(SSP_PREF)SRS.css ./Website/$(SSP_PREF)SRS.css >> $(SSP_PREF)log.log 2>&1
	- $(DIFF) ./stable/ssp/$(SSP_PREF)SRS.html ./Website/$(SSP_PREF)SRS.html >> $(SSP_PREF)log.log 2>&1
	
glassbr:
	cabal build glassbr
	./dist/build/glassbr/glassbr
	- $(DIFF) ./stable/glassbr/$(GLASS_PREF)SRS.tex ./SRS/$(GLASS_PREF)SRS.tex &> $(GLASS_PREF)log.log
	- $(DIFF) ./stable/glassbr/$(GLASS_PREF)MG.tex ./MG/$(GLASS_PREF)MG.tex >> $(GLASS_PREF)log.log 2>&1
	- $(DIFF) ./stable/glassbr/$(GLASS_PREF)SRS.css ./Website/$(GLASS_PREF)SRS.css >> $(GLASS_PREF)log.log 2>&1
	- $(DIFF) ./stable/glassbr/$(GLASS_PREF)SRS.html ./Website/$(GLASS_PREF)SRS.html >> $(GLASS_PREF)log.log 2>&1
	
gamephys:
	cabal build chipmunkdocs
	./dist/build/chipmunkdocs/chipmunkdocs
	- $(DIFF) ./stable/gamephys/$(GAME_PREF)SRS.tex ./SRS/$(GAME_PREF)SRS.tex &> $(GAME_PREF)log.log
	- $(DIFF) ./stable/gamephys/$(GAME_PREF)MG.tex ./MG/$(GAME_PREF)MG.tex >> $(GAME_PREF)log.log 2>&1
	- $(DIFF) ./stable/gamephys/$(GAME_PREF)SRS.css ./Website/$(GAME_PREF)SRS.css >> $(GAME_PREF)log.log 2>&1
	- $(DIFF) ./stable/gamephys/$(GAME_PREF)SRS.html ./Website/$(GAME_PREF)SRS.html >> $(GAME_PREF)log.log 2>&1
	
pcm:
	cabal build pcm
	./dist/build/pcm/pcm
	- $(DIFF) ./stable/pcm/$(PCM_PREF)SRS.tex ./SRS/$(PCM_PREF)SRS.tex &> $(PCM_PREF)log.log
	- $(DIFF) ./stable/pcm/$(PCM_PREF)SRS.css ./Website/$(PCM_PREF)SRS.css >> $(PCM_PREF)log.log 2>&1
	- $(DIFF) ./stable/pcm/$(PCM_PREF)SRS.html ./Website/$(PCM_PREF)SRS.html >> $(PCM_PREF)log.log 2>&1
	
clean: clean_logs
	rm -r ./SRS ./Website ./MIS ./MG ./Code ./dist
	
clean_logs:
	rm *.log
