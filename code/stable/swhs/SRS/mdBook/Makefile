build: assets
	mdbook build

server: assets
	mdbook serve --open

assets:
	-mkdir -p "src/assets" && cp "../../../../datafiles/swhs/SystemContextFigure.png" "src/assets/SystemContextFigure.png"
	-mkdir -p "src/assets" && cp "../../../../datafiles/swhs/Tank.png" "src/assets/Tank.png"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/swhs/allvsall.svg" "src/assets/allvsall.svg"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/swhs/allvsr.svg" "src/assets/allvsr.svg"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/swhs/avsa.svg" "src/assets/avsa.svg"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/swhs/avsall.svg" "src/assets/avsall.svg"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/swhs/refvsref.svg" "src/assets/refvsref.svg"

.PHONY: build server assets
