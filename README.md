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
disabling the conversion of (passing through) those [SystemVerilog features that
Yosys supports].

[Yosys]: https://yosyshq.net/yosys/
[SystemVerilog features that Yosys supports]: https://github.com/YosysHQ/yosys#supported-features-from-systemverilog

The idea for this project was shared with me while I was an undergraduate at
Carnegie Mellon University as part of a joint Computer Science and Electrical
and Computer Engineering research project on open hardware under Professors [Ken
Mai] and [Dave Eckhardt]. I have greatly enjoyed collaborating with the team at
CMU since January 2019, even after my graduation the following May.

[Ken Mai]: https://engineering.cmu.edu/directory/bios/mai-kenneth.html
[Dave Eckhardt]: https://www.cs.cmu.edu/~davide/


## Dependencies

All of sv2v's dependencies are free and open-source.

* Build Dependencies
    * [Haskell Stack](https://www.haskellstack.org/) - Haskell build system
    * Haskell dependencies are managed in `sv2v.cabal`
* Test Dependencies
    * [Icarus Verilog](https://steveicarus.github.io/iverilog/) - for Verilog
      simulation
    * [shUnit2](https://github.com/kward/shunit2) - test framework
    * Python 3.x - for evaluating certain test cases


## Installation

### Pre-built binaries

Binaries for Ubuntu, macOS, and Windows are available on the [releases page]. If
your system is not covered, or you would like to build the latest commit, simple
instructions for building from source are below.

[releases page]: https://github.com/zachjs/sv2v/releases

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

sv2v takes in a list of files and prints the converted Verilog to `stdout` by
default. Users should typically pass all of their SystemVerilog source files to
sv2v at once so it can properly resolve packages, interfaces, type parameters,
etc., across files. Using `--write=adjacent` will create a converted `.v` for
every `.sv` input file rather than printing to `stdout`. `--write`/`-w` can also
be used to specify a path to a `.v` output file. Undefined modules and
interfaces can be automatically loaded from library directories using
`--libdir`/`-y`.

Users may specify `include` search paths, define macros during preprocessing,
and exclude some of the conversions. Specifying `-` as an input file will read
from `stdin`.

Below is the current usage printout.

```
sv2v [OPTIONS] [FILES]

Preprocessing:
  -I --incdir=DIR           Add a directory to the include search path
  -y --libdir=DIR           Add a directory to the library search path used
                            when looking for undefined modules and interfaces
  -D --define=NAME[=VALUE]  Define a macro for preprocessing
     --siloed               Lex input files separately, so macros from
                            earlier files are not defined in later files
     --skip-preprocessor    Disable preprocessing of macros, comments, etc.
Conversion:
     --pass-through         Dump input without converting
  -E --exclude=CONV         Exclude a particular conversion (Always, Assert,
                            Interface, Logic, SeverityTask, or UnbasedUnsized)
  -v --verbose              Retain certain conversion artifacts
  -w --write=MODE/FILE/DIR  How to write output; default is 'stdout'; use
                            'adjacent' to create a .v file next to each input;
                            use a path ending in .v to write to a file; use a
                            path to an existing directory to create a .v within
                            for each converted module
     --top=NAME             Remove uninstantiated modules except the given
                            top module; can be used multiple times
Other:
     --oversized-numbers    Disable standard-imposed 32-bit limit on unsized
                            number literals (e.g., 'h1_ffff_ffff, 4294967296)
     --dump-prefix=PATH     Create intermediate output files with the given
                            path prefix; used for internal debugging
     --bugpoint=SUBSTR      Reduce the input by pruning modules, wires, etc.,
                            that aren't needed to produce the given output or
                            error substring when converted
     --help                 Display this help message
     --version              Print version information
     --numeric-version      Print just the version number
```


## Supported Features

sv2v supports most synthesizable SystemVerilog features. Current notable
exceptions include `defparam` on interface instances, certain synthesizable
usages of parameterized classes, and the `bind` keyword. Assertions are also
supported, but are simply dropped during conversion.

If you find a bug or have a feature request, please [create an issue].
Preference will be given to issues that include examples or test cases.

[create an issue]: https://github.com/zachjs/sv2v/issues/new


## SystemVerilog Front End

This project contains a preprocessor, lexer, and parser, and an abstract syntax
tree representation for a subset of the SystemVerilog specification. The parser
is not very strict. The AST allows for the representation of syntactically (and
semantically) invalid Verilog. The goal is to be more general in the
representation to enable more standardized and straightforward conversion
procedures. This could be extended into an independent and more fully-featured
front end if there is significant interest.


## Testing

Once the [test dependencies] are installed, tests can be run with `make test`.
GitHub Actions is used to [automatically test] commits. Please review the [test
documentation] for guidance on adding, debugging, and interpreting tests.

[test dependencies]: #dependencies
[test documentation]: test/README.md
[automatically test]: https://github.com/zachjs/sv2v/actions

There is also a [SystemVerilog compliance suite] that tests open-source tools'
SystemVerilog support. Although not every test in the suite is applicable, it
has been a valuable asset in finding edge cases.

[SystemVerilog compliance suite]: https://github.com/chipsalliance/sv-tests


## Acknowledgements

This project was originally forked from [Tom Hawkin's Verilog parser]. While the
front end has changed substantially to support the larger SystemVerilog
standard, his project was a great starting point.

[Tom Hawkin's Verilog parser]: https://github.com/tomahawkins/verilog

Reid Long was invaluable in developing this tool, providing significant tests
and advice, and isolating many bugs.

Edric Kusuma helped me with the ins and outs of SystemVerilog, with which I had
no prior experience, and has also helped with test cases.

Since sv2v's public release, many people have taken the time to file detailed
bug reports and feature requests. I greatly appreciate their help in furthering
the project.
