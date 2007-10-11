V := 6.6.1

all: ghc-$(V)/bin/i386-apple-darwin/ghc-$(V) ghc-$(V)/bin/powerpc-apple-darwin/ghc-$(V)
	scons -j`sysctl -n hw.ncpu | xargs perl -e 'print int(1.5 * shift);'`

clean:
	rm -rf build

squeaky: clean
	rm -rf ghc-$(V)

ghc-$(V)/bin/%/ghc: ghc-$(V)-%.tar.bz2
	tar xjmf $<

ghc-$(V)/bin/%/ghc-$(V): ghc-$(V)/bin/%/ghc
	cd ghc-$(V) && \
	./configure --host=`basename \`dirname $@\`` && \
	make in-place

archive: all
	tar cjf Patience.tar.bz2 Patience.app

.SECONDARY:
