all: patience

patience: $(shell ls *.hs)
	mkdir -p Build
	ghc -O --make Main -odir Build -hidir Build -o patience

clean:
	rm -rf Build
	rm -f patience
