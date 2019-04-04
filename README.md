# sv2v: SystemVerilog to Verilog

sv2v converts SystemVerilog ([IEEE 1800-2017]) to Verilog ([IEEE 1364-2005]),
with an emphasis on supporting synthesizable language constructs.

[IEEE 1800-2017]: https://ieeexplore.ieee.org/servlet/opac?punumber=8299593
[IEEE 1364-2005]: https://ieeexplore.ieee.org/servlet/opac?punumber=10779

The primary goal of this project is to create a completely free and open-source
tool for converting SystemVerilog to Verilog. While methods for performing this
conversion already exist, they generally either rely on commercial tools, or are
pretty incomplete.

This project was originally developed to target [Yosys], and so allows for
disabling the conversion of those [SystemVerilog features which Yosys supports].

[Yosys]: http://www.clifford.at/yosys/
[SystemVerilog features which Yosys supports]: https://github.com/YosysHQ/yosys#supported-features-from-systemverilog


## Dependencies

All of sv2v's dependencies are free and open-source.

* Build Dependencies
    * [Haskell Stack](https://www.haskellstack.org/) - Haskell build system
    * Haskell dependencies are managed in `sv2v.cabal`
* Test Dependencies
    * [Icarus Verilog](http://iverilog.icarus.com) - for Verilog simulation
    * [shUnit2](https://github.com/kward/shunit2) - test framework


## Installation

### Pre-built binaries

We plan on releasing pre-built binaries in the future.

### Building from source

You must have [Stack] installed to build sv2v.

[Stack]: https://www.haskellstack.org/

```
git clone https://github.com/zachjs/sv2v
cd sv2v
stack setup
make
```

This creates the executable at `./bin/sv2v`. You can install the binary by
running `stack install`.


## Usage

sv2v takes in a list of files and prints the converted Verilog to `stdout`.
Users may specify include search paths, define macros during preprocessing, and
exclude some of the conversion.

Below is the current usage printout. This interface is subject to change.

```
sv2v [OPTIONS] [FILES]

Common flags:
  -e --exclude=CONV         exclude a particular conversion (always,
                            interface, logic)
  -i --incdir=DIR           add directory to include search path
  -d --define=NAME[=VALUE]  define a macro for preprocessing
  -? --help                 Display help message
  -V --version              Print version information
     --numeric-version      Print just the version number
```


## SystemVerilog Frontend

This project contains a preprocessor and lexer, a parser, and an abstract syntax
tree representation for a subset of the SystemVerilog specification. The parser
is not very strict. The AST allows for the representation of syntactically (and
semantically) invalid Verilog. The goal is to be more general in the
representation to enable more standardized and straightforward conversion
procedures. This could be extended into an independent and more fully-featured
frontend if there is significant interest.


## Testing

The current test suite is limited. Tests can be run with `make test`.


## License

See the LICENSE file for copyright and licensing information.
