# sv2v: SystemVerilog to Verilog

sv2v is a tool for converting synthesizable SystemVerilog into Verilog that is
synthesizable by tools with more limited feature sets. This project was
originally created for converting SystemVerilog into the [limited subset of
Verilog] supported by [VTR]. However, sv2v is intended to be configurable and
extensible so that it can be used with new and different toolchains and as
Verilog keyword support evolves.

[limited subset of Verilog]: https://vtr-verilog-to-routing.readthedocs.io/en/latest/odin/index.html#verilog-hdl-file-keyword-support
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

The interface for this tool has not yet been finalized. Currently, running
`bin/sv2v path/to/file.sv` will output the converted file to `stdout`.


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
