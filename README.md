# sv2v: SystemVerilog to Verilog

sv2v is a tool for converting synthesizable SystemVerilog into Verilog that is
synthesizable by tools with more limited feature sets. This project is primarily
focused on converting SystemVerilog into the subset of Verilog supported by
[Yosys]. However, sv2v also has support for targeting the [limited subset of
Verilog] supported by [VTR]. In the long term, we hope for sv2v to be more
configurable and extensible so that it can be used with new and different
toolchains and as Verilog support evolves.

[Yosys]: http://www.clifford.at/yosys/
[limited subset of Verilog]: https://docs.verilogtorouting.org/en/latest/odin/#verilog-synthesizable-keyword-support
[VTR]: https://github.com/verilog-to-routing/vtr-verilog-to-routing


## Installation

### Pre-built binaries

Given the infamy of Haskell's build system, we aim to release pre-built binaries
and installation files. This has not been done yet.

### Building from source

You must have [Stack] installed to build sv2v.

[Stack]: https://www.haskellstack.org/

```
git clone https://github.com/zachjs/sv2v
cd sv2v
stack setup
make
```

This creates the executable at `./bin/sv2v`


## Usage

The interface for this tool has not yet been finalized. Currently, running `sv2v
path/to/file.sv` will output the converted file to `stdout`.

```
sv2v [OPTIONS] [FILE]

Common flags:
  -t --target=TARGET    target sythesizer (yosys, vtr; defaults to yosys)
  -? --help             Display help message
```


## VTR Support

sv2v can target VTR by specifying `--target=vtr` on the command line. Note that
VTR does not support `generate` blocks, and this tool is not capable of
converting those at this time.


## SystemVerilog Parser/AST

This project contains a basic preprocessor, lexer, parser, and abstract syntax
tree for a subset of synthesizable SystemVerilog. The parser is not extremely
strict, and the AST allows for the representation of syntactically (and
semantically) invalid Verilog. The goal is to be more general in the
representation to enable more standardized and straightforward conversion
procedures. This could be extended into an independent and more fully-featured
parer if there is significant interest.


## License

See the LICENSE file for copyright and licensing information.
