RELEASE := 0

ifeq ($(RELEASE),1)
	HS_OFLAGS := -O2
	HS_LFLAGS := -optl-dead_strip
	SUFFIX :=
	BUILD := Build/Release
else
	HS_OFLAGS :=
	HS_LFLAGS :=
	SUFFIX := -debug
	BUILD := Build/Debug
endif	

HSFILES := $(shell find Source -name '*.hs')
HS_CFLAGS := --make -iSource/Common
HSFLAGS := $(HS_OFLAGS) $(HS_CFLAGS) $(HS_LFLAGS)

PATIENCE := patience$(SUFFIX)
GTK_PATIENCE := gtk-patience$(SUFFIX)

all: patience gtk-patience

patience: $(PATIENCE)

gtk-patience: $(GTK_PATIENCE)

$(PATIENCE): $(HSFILES)
	mkdir -p $(BUILD)/CommandLine
	ghc $(HSFLAGS) Source/CommandLine/Main.hs -odir $(BUILD)/CommandLine -hidir $(BUILD)/CommandLine -o $(PATIENCE)

$(GTK_PATIENCE): $(HSFILES)
	mkdir -p $(BUILD)/GTK
	ghc $(HSFLAGS) Source/GTK/Main.hs -odir $(BUILD)/GTK -hidir $(BUILD)/GTK -o $(GTK_PATIENCE)

clean:
	rm -rf Build
	rm -f $(PATIENCE) $(GTK_PATIENCE)
