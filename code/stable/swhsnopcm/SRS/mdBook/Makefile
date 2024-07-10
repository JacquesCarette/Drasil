build: assets
	mdbook build

server: assets
	mdbook serve --open

assets:
	-mkdir -p "src/assets" && cp "../../../../datafiles/swhsnopcm/SystemContextFigure.png" "src/assets/SystemContextFigure.png"
	-mkdir -p "src/assets" && cp "../../../../datafiles/swhsnopcm/TankWaterOnly.png" "src/assets/TankWaterOnly.png"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/swhsnopcm/allvsall.svg" "src/assets/allvsall.svg"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/swhsnopcm/allvsr.svg" "src/assets/allvsr.svg"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/swhsnopcm/avsa.svg" "src/assets/avsa.svg"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/swhsnopcm/avsall.svg" "src/assets/avsall.svg"
	-mkdir -p "src/assets" && cp "../../../../traceygraphs/swhsnopcm/refvsref.svg" "src/assets/refvsref.svg"

.PHONY: build server assets
