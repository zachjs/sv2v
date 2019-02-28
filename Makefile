.PHONY: all sv2v clean

all: sv2v

sv2v:
	mkdir -p bin
	stack install --install-ghc --local-bin-path bin

clean:
	stack clean
	rm -rf bin
