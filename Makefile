BUILD := build

HSFILES := $(shell find Source -name '*.hs')
HS_PFLAGS := -prof -auto-all
HS_OFLAGS := 
HS_CFLAGS := --make -iSource
HSFLAGS := $(HS_OFLAGS) $(HS_CFLAGS) $(HS_LFLAGS) $(HS_PFLAGS)

HFILES := $(shell find Source -name '*.h')
MFILES := $(shell find Source -name '*.m')
OFILES := $(patsubst Source/%.m,build/%.o,$(MFILES))
M_IFLAGS := -I/Volumes/Files/Unix/lib/ghc-6.6.1/include -Ibuild/Cocoa
M_CFLAGS := -Wall -W -Wno-unused-parameter -Wnewline-eof -Werror
M_OFLAGS := -g -O2
MFLAGS := $(M_IFLAGS) $(M_CFLAGS) $(M_OFLAGS)
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

build/%.o: Source/%.m $(HFILES) $(BUILD)/Cocoa/Main_stub.h
	mkdir -p `dirname $@`
	gcc -c $(MFLAGS) $< -o $@

build/%_stub.h: Source/%.hs
	mkdir -p `dirname $@`
	ghc -c $(HSFLAGS) $< -stubdir $(BUILD)/Cocoa

$(APP_EXE): $(OFILES) $(HFILES) $(HSFILES)
	mkdir -p `dirname $@`
	mkdir -p $(BUILD)/Cocoa
	rm -f $@
	ghc $(HSFLAGS) Source/Cocoa/Main.hs -stubdir $(BUILD)/Cocoa -odir $(BUILD)/Cocoa -hidir $(BUILD)/Cocoa $(OFILES) -o $(APP_EXE) $(LINKFLAGS)

$(APP_INFO_PLIST): Resources/Info.plist
	mkdir -p `dirname $@`
	cp $< $@

$(APP_PKGINFO):
	mkdir -p `dirname $@`
	echo "APPL????" > $(APP_PKGINFO)

#$(APP_NIB): Resources/English.lproj/MainMenu.xib
$(APP_NIB): Resources/English.lproj/MainMenu.nib/keyedobjects.nib
	mkdir -p `dirname $@`
	#ibtool $< --compile $@
	cp $< $@

clean:
	rm -rf $(BUILD)
	rm -f $(PATIENCE)
	rm -f *.prof
	rm -rf $(APP_BUNDLE)

