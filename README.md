# sv2v: SystemVerilog to Verilog

sv2v is a tool for converting SystemVerilog ([IEEE 1800-2017]) to ([IEEE
1364-2005]), with an emphasis on supporting synthesizable SystemVerilog
features. This project was originally developed to target [Yosys], and so allows
for disabling the conversion of those [SystemVerilog features which Yosys
supports].

[IEEE 1800-2017]: https://ieeexplore.ieee.org/servlet/opac?punumber=8299593
[IEEE 1364-2005]: https://ieeexplore.ieee.org/servlet/opac?punumber=10779
[Yosys]: http://www.clifford.at/yosys/
[SystemVerilog features which Yosys supports]: https://github.com/YosysHQ/yosys#supported-features-from-systemverilog


## Installation

### Pre-built binaries

We plan on releasing pre-built binaries in the near future.

### Building from source

You must have [Stack] installed to build sv2v.

[Stack]: https://www.haskellstack.org/

```
git clone https://github.com/zachjs/sv2v
cd sv2v
stack setup
make
```

This creates the executable at `./bin/sv2v` You can install the binary by
running `stack install`.


## Usage

The interface for this tool has not yet been finalized. Currently, running `sv2v
path/to/file.sv` will output the converted file to `stdout`.

```
sv2v [OPTIONS] [FILES]

Common flags:
  -e --exclude=CONV     conversion to exclude (always, interface, logic); can
                        be specified multiple times
  -i --incdir=DIR       add directory to include search path
  -? --help             Display help message
  -V --version          Print version information
     --numeric-version  Print just the version number
```


## SystemVerilog Parser/AST

This project contains a basic preprocessor, lexer, parser, and abstract syntax
tree for a subset of synthesizable SystemVerilog. The parser is not extremely
strict, and the AST allows for the representation of syntactically (and
semantically) invalid Verilog. The goal is to be more general in the
representation to enable more standardized and straightforward conversion
procedures. This could be extended into an independent and more fully-featured
parser if there is significant interest.


## License

See the LICENSE file for copyright and licensing information.
