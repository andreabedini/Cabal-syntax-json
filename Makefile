cabal2json: $(wildcard *.hs)
	ghc --make -clear-package-db -global-package-db -hide-all-packages -package base -package bytestring -package Cabal -package Cabal-syntax -o $@ $^

.phony: clean
clean:
	rm -f cabal2json *.hi *.o
