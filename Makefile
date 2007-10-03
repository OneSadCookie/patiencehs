BUILD := build

HSFILES := $(shell find Source -name '*.hs')
HS_PFLAGS := -prof -auto-all
HS_OFLAGS := -O2
HS_CFLAGS := --make -iSource
HSFLAGS := $(HS_OFLAGS) $(HS_CFLAGS) $(HS_LFLAGS) $(HS_PFLAGS)

HFILES := $(shell find Source -name '*.h')
MFILES := $(shell find Source -name '*.m')
M_CFLAGS := -Wall -W -Wno-unused-parameter -Wnewline-eof -Werror
M_OFLAGS := -g -O2
MFLAGS := $(M_CFLAGS) $(M_OFLAGS)
LINKFLAGS := -framework Cocoa

PATIENCE := patience

APP_BUNDLE := Patience.app
APP_CONTENTS := $(APP_BUNDLE)/Contents
APP_EXE := $(APP_CONTENTS)/MacOS/Patience
APP_RESOURCES := $(APP_CONTENTS)/Resources
APP_INFO_PLIST := $(APP_CONTENTS)/Info.plist
APP_PKGINFO := $(APP_CONTENTS)PkgInfo

APP_NIB := $(APP_RESOURCES)/English.lproj/MainMenu.nib/keyedobjects.nib

all: $(PATIENCE) patience_app

$(PATIENCE): $(HSFILES)
	mkdir -p $(BUILD)/CommandLine
	ghc $(HSFLAGS) Source/CommandLine/Main.hs -odir $(BUILD)/CommandLine -hidir $(BUILD)/CommandLine -o $(PATIENCE)

patience_app: $(APP_EXE) $(APP_INFO_PLIST) $(APP_PKGINFO) resources

resources: $(APP_NIB)

$(APP_EXE): $(MFILES) $(HFILES) $()
	mkdir -p `dirname $@`
	gcc $(MFLAGS) $(MFILES) -o $(APP_EXE) $(LINKFLAGS)

$(APP_INFO_PLIST): Resources/Info.plist
	mkdir -p `dirname $@`
	cp $< $@

$(APP_PKGINFO):
	mkdir -p `dirname $@`
	echo "APPL????" > $(APP_PKGINFO)

$(APP_NIB): Resources/English.lproj/MainMenu.xib
	mkdir -p `dirname $@`
	ibtool $< --compile $@

clean:
	rm -rf $(BUILD)
	rm -f $(PATIENCE)
	rm -f *.prof
