GHCFLAGS=-Wall -Wno-tabs -Wno-orphans -Wno-name-shadowing -XHaskell2010 -O2 -threaded
HLINTFLAGS=-XHaskell2010 -XCPP -i 'Use camelCase' -i 'Use String' -i 'Use head' -i 'Use string literal' -i 'Use list comprehension'
VERSION=0.0.1

.PHONY: all shell clean doc install

all: report.html doc dist/build/libHScheogram-$(VERSION).a dist/cheogram-$(VERSION).tar.gz

install: dist/build/libHScheogram-$(VERSION).a
	cabal install

shell:
	ghci $(GHCFLAGS)

report.html: Main.hs
	-hlint $(HLINTFLAGS) --report $^

doc: dist/doc/html/cheogram/index.html README

README: cheogram.cabal
	tail -n+$$(( `grep -n ^description: $^ | head -n1 | cut -d: -f1` + 1 )) $^ > .$@
	head -n+$$(( `grep -n ^$$ .$@ | head -n1 | cut -d: -f1` - 1 )) .$@ > $@
	-printf ',s/        //g\n,s/^.$$//g\n,s/\\\\\\//\\//g\nw\nq\n' | ed $@
	$(RM) .$@

dist/doc/html/cheogram/index.html: dist/setup-config Main.hs
	cabal haddock --hyperlink-source

dist/setup-config: cheogram.cabal
	cabal configure

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) -r dist

dist/build/libHScheogram-$(VERSION).a: dist/setup-config Main.hs
	cabal build --ghc-options="$(GHCFLAGS)"

dist/cheogram-$(VERSION).tar.gz: README dist/setup-config Main.hs
	cabal check
	cabal sdist
