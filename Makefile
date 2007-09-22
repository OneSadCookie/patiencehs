RELEASE := 0

ifeq ($(PROFILE),1)
	HS_PFLAGS := -prof -auto-all
	SUFFIX := -profile
	BUILD := Build/Profile
else
	HS_PFLAGS := 
	SUFFIX := 
	BUILD := Build
endif

ifeq ($(RELEASE),1)
	HS_OFLAGS := -O2
	HS_LFLAGS := -optl-dead_strip
	SUFFIX := $(SUFFIX)
	BUILD := $(BUILD)/Release
else
	HS_OFLAGS := 
	HS_LFLAGS := 
	SUFFIX := $(SUFFIX)-debug
	BUILD := $(BUILD)/Debug
endif	

HSFILES := $(shell find Source -name '*.hs')
HS_CFLAGS := --make -iSource/Common
HSFLAGS := $(HS_OFLAGS) $(HS_CFLAGS) $(HS_LFLAGS) $(HS_PFLAGS)

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
