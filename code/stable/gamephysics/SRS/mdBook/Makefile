build: assets
	mdbook build

server: assets
	mdbook serve --open

assets:
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/gamephysics/allvsall.svg" "src/assets/allvsall.svg"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/gamephysics/allvsr.svg" "src/assets/allvsr.svg"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/gamephysics/avsa.svg" "src/assets/avsa.svg"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/gamephysics/avsall.svg" "src/assets/avsall.svg"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/gamephysics/refvsref.svg" "src/assets/refvsref.svg"
	-mkdir -p "src/assets" && cp "../../../../datafiles/gamephysics/sysctx.png" "src/assets/sysctx.png"

.PHONY: build server assets
