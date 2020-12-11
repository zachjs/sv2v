.PHONY: all sv2v clean test

all: sv2v

sv2v:
	mkdir -p bin
	stack install --install-ghc --local-bin-path bin

clean:
	stack clean
	rm -rf bin

test:
	(cd test && ./run-all.sh)

coverage:
	stack install --local-bin-path bin --ghc-options=-fhpc
	rm -f test/*/sv2v.tix
	make test
	stack exec hpc -- sum test/*/sv2v.tix --union --output=.hpc/combined.tix
	stack exec hpc -- markup .hpc/combined.tix --destdir=.hpc
