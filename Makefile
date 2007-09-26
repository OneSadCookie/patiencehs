HSFILES := $(shell find Source -name '*.hs')
HS_PFLAGS := -prof -auto-all
HS_OFLAGS := 
HS_CFLAGS := --make -iSource
HSFLAGS := $(HS_OFLAGS) $(HS_CFLAGS) $(HS_LFLAGS) $(HS_PFLAGS)
PATIENCE := patience
BUILD := build

all: $(PATIENCE)

$(PATIENCE): $(HSFILES)
	mkdir -p $(BUILD)/CommandLine
	ghc $(HSFLAGS) Source/CommandLine/Main.hs -odir $(BUILD)/CommandLine -hidir $(BUILD)/CommandLine -o $(PATIENCE)

clean:
	rm -rf $(BUILD)
	rm -f $(PATIENCE)
