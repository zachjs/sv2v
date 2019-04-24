# sv2v: SystemVerilog to Verilog

sv2v converts SystemVerilog ([IEEE 1800-2017]) to Verilog ([IEEE 1364-2005]),
with an emphasis on supporting synthesizable language constructs.

[IEEE 1800-2017]: https://ieeexplore.ieee.org/servlet/opac?punumber=8299593
[IEEE 1364-2005]: https://ieeexplore.ieee.org/servlet/opac?punumber=10779

The primary goal of this project is to create a completely free and open-source
tool for converting SystemVerilog to Verilog. While methods for performing this
conversion already exist, they generally either rely on commercial tools, or are
limited in scope.

This project was originally developed to target [Yosys], and so allows for
disabling the conversion of (passing through) those [SystemVerilog features
which Yosys supports].

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

You must have [Stack] installed to build sv2v. Then you can:

[Stack]: https://www.haskellstack.org/

```
git clone https://github.com/zachjs/sv2v.git
cd sv2v
make
```

This creates the executable at `./bin/sv2v`. Stack takes care of installing
exact (compatible) versions of the compiler and sv2v's build dependencies.

You can install the binary to your local bin path (typically `~/.local/bin`) by
running `stack install`, or copy over the executable manually.


## Usage

sv2v takes in a list of files and prints the converted Verilog to `stdout`.
Users may specify `include` search paths, define macros during preprocessing,
and exclude some of the conversions.

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


## Supported Features

sv2v supports most synthesizable SystemVerilog features. Current notable
exceptions include `export`, interfaces _with parameter bindings_, and complex
(non-identifier) `modport` expressions. Assertions are also supported, but are
simply dropped during conversion.

If you find a bug or have a feature request, please create an issue. Preference
will be given to issues which include examples or test cases.


## SystemVerilog Front End

This project contains a preprocessor and lexer, a parser, and an abstract syntax
tree representation for a subset of the SystemVerilog specification. The parser
is not very strict. The AST allows for the representation of syntactically (and
semantically) invalid Verilog. The goal is to be more general in the
representation to enable more standardized and straightforward conversion
procedures. This could be extended into an independent and more fully-featured
front end if there is significant interest.


## Testing

The current test suite is limited. Tests can be run with `make test`.


## Acknowledgements

This project was originally forked from [Tom Hawkin's Verilog parser]. While the
front end has changed substantially to parse a different language, his project
was a great starting point.

[Tom Hawkin's Verilog parser]: https://github.com/tomahawkins/verilog

Reid Long was invaluable in developing this tool, providing significant tests
and advice, and isolating many bugs. His projects can be found
[here](https://bitbucket.org/ReidLong/).

Edric Kusuma helped me with the ins and outs of SystemVerilog, with which I had
no prior experience, and has also helped with test cases.


## License

See the [LICENSE file](LICENSE) for copyright and licensing information.
