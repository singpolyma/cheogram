GHCFLAGS=-Wall -Wno-tabs -Wno-orphans -Wno-name-shadowing -XHaskell2010 -O -threaded
HLINTFLAGS=-XHaskell2010 -XCPP -i 'Use camelCase' -i 'Use String' -i 'Use head' -i 'Use string literal' -i 'Use list comprehension'

.PHONY: all shell clean

all: report.html cheogram

cheogram: Main.hs Adhoc.hs Config.hs ConfigureDirectMessageRoute.hs DB.hs IQManager.hs RedisURL.hs StanzaRec.hs UniquePrefix.hs Util.hs JidSwitch.hs VCard4.hs
	ghc -dynamic -package monads-tf -o cheogram $(GHCFLAGS) Main.hs

report.html: Main.hs Adhoc.hs Config.hs ConfigureDirectMessageRoute.hs DB.hs IQManager.hs RedisURL.hs StanzaRec.hs UniquePrefix.hs Util.hs JidSwitch.hs VCard4.hs
	-hlint $(HLINTFLAGS) --report $^

shell:
	ghci $(GHCFLAGS)

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) cheogram report.html
