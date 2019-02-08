.PHONY: all sv2v clean

all: sv2v

Language/SystemVerilog/Parser/Lex.hs: Language/SystemVerilog/Parser/Lex.x
	alex -g Language/SystemVerilog/Parser/Lex.x -o Language/SystemVerilog/Parser/Lex.hs

Language/SystemVerilog/Parser/Parse.hs: Language/SystemVerilog/Parser/Parse.y
	happy -agc Language/SystemVerilog/Parser/Parse.y -o Language/SystemVerilog/Parser/Parse.hs

sv2v: Language/SystemVerilog/Parser/Lex.hs Language/SystemVerilog/Parser/Parse.hs
	cabal install

clean:
	rm -f Language/SystemVerilog/Parser/Lex.hs
	rm -f Language/SystemVerilog/Parser/Parse.hs
