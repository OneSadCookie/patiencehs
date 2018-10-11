GHC_VERSION := $(shell ghc --version | tail -c 6)
CABAL_EXE := ./dist-newstyle/build/x86_64-osx/ghc-$(GHC_VERSION)/PatienceHS-0.1.0.0/x/PatienceHS/build/PatienceHS/PatienceHS
APP := Patience.app
CONTENTS := $(APP)/Contents
INFO_PLIST := $(CONTENTS)/Info.plist
PKGINFO := $(CONTENTS)/PkgInfo
RESOURCES := $(CONTENTS)/Resources
EXE := $(CONTENTS)/MacOS/Patience

all: $(EXE) $(INFO_PLIST) $(PKGINFO)
	rsync -av Resources/ $(RESOURCES)

clean:
	rm -rf dist-newstyle
	rm -rf Patience.app

archive: all
	tar cjf Patience.tar.bz2 Patience.app

$(CABAL_EXE): $(glob Source/*/*.{hs,h,m})
	cabal new-build

$(EXE): $(CABAL_EXE)
	@mkdir -p `dirname $@`
	cp $< $@

$(PKGINFO):
	@mkdir -p `dirname $@`
	echo -n 'APPL????' > $@

$(INFO_PLIST): Info.plist
	@mkdir -p `dirname $@`
	cp $< $@

.PHONY: all clean archive
