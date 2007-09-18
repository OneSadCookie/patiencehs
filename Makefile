all: patience

patience: $(shell find Source -name '*.hs')
	mkdir -p Build
	ghc -O --make Source/CommandLine/Main.hs -iSource/Common -odir Build -hidir Build -o patience

clean:
	rm -rf Build
	rm -f patience
