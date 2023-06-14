# sv2v tests

Tests are run via `make test` or by running `run-all.sh` directly. Each
sub-folder here (except `lib`) is a suite of tests using shUnit2 as a test
framework. Every suite contains a `run.sh` that performs its tests when run from
within the suite's folder. The tests are run against the compiled `bin/sv2v` in
the root of the repository.


## Regular test suites

These primary test suites collectively contain hundreds of test cases, each of
which is converted using sv2v and simulated using iverilog. The test cases in
these suites are automatically discovered. Each test case `foo` consists of
three components:

1. **`foo.sv`** is the SystemVerilog file to be converted by sv2v.
2. **`foo.v`** is a manually-produced Verilog file that should behave
   equivalently to `foo.sv`. For Verilog-compatible test cases, this file may be
   omitted, in which case the runner uses the original `foo.sv` instead.
3. **`foo_tb.v`** is a Verilog testbench provided to iverilog to exercise both
   `foo.v` and the converted `foo.sv`. If this file is not present, the runner
   assumes both `foo.sv` and `foo.v` contain a `module top`.

For each test, the simulation results are compared by log output (e.g., from
`$display` or `$monitor`) and by top-level VCD (excluding parameters). The test
is repeated with the conversion performed in `--verbose` mode.

All three source files are run through sv2v repeatedly to perform the following
additional consistency checks:

* Each file should convert with no errors or warnings.
* When the converted output is converted again, it should not change.
* `sv2v file.sv` should exactly match `sv2v --pass-through file.sv | sv2v -`.
* The converted output should not contain certain SystemVerilog-only constructs
  that iverilog happens to support in Verilog-2005 mode, such as `$bits`.

Each regular test suite has a particular focus:

* `basic` contains Verilog-compatible tests without a `.v`
* `core` contains standard tests with at least a `.sv` and a `.v`
* `lex` contains tests for lexing and preprocessing (e.g., macros)
* `relong` contains tests provided by Reid Long in 2019

Rarely, a file may have an adjacent `.pat` file (e.g., [unneeded_scope.sv.pat])
used to assert the presence or omission of specific substrings in the converted
output. This can be used to check aspects of sv2v's conversion behavior that are
not apparent in simulation.

[unneeded_scope.sv.pat]: core/unneeded_scope.sv.pat


## Specialized test suites

The `error` suite tests inputs that should cause sv2v to output an error. The
suite runs each `.sv` file in the folder and ensures that sv2v returns a
non-zero exit code and outputs something to `stderr`. Each test may contain a
`// pattern: ...` and a `// location: ...` flag at the top. When present, the
test runner checks if the `stderr` contains the given error and location
information.

The `nosim` suite simply checks that each of the `.sv` files can be converted by
sv2v, without evaluating the converted output. This is used to provide limited
coverage of language constructs that sv2v can output but that are not supported
by `iverilog`.

The remaining test suites have a custom `run.sh` that defines a list of test
procedures that may not correspond directly to the other files in the folder.
Many of these suites test a particular feature of the sv2v CLI.

* `define` tests `-D`/`--define`
* `dump` tests `--dump-prefix`
* `help` ensures the `--help` output in the README is up to date
* `keyword` tests `begin_keywords` version specifiers
* `number` generates and tests short number literals
* `search` tests `-y`/`--libdir`
* `siloed` tests `--siloed` and default compilation unit behavior
* `truncate` tests number literal truncation and `--oversized-numbers`
* `warning` tests conversion warnings
* `write` tests `-w`/`--write`


## Adding new tests

Most tests for bug fixes or new language features are added to the `core` suite,
with a `.sv` and a hand-converted `.v`. New CLI features typically get a new
specialized test suite. All test files use 4 spaces for indentation. Macros or
`.vh` files may be used to reduce duplicated code where appropriate.


## Debugging test failures

* There may be incompatibilities with a specific version of iverilog. Please see
  the [main GitHub Actions workflow] to determine what version of iverilog is
  currently used in CI.
* It is often helpful to run a subset of the tests in the large suites (e.g.,
  `core` and `error`) by passing a list of file or base names to `run.sh`. For
  example, `./run.sh missing_join` or `./run.sh interface_*.sv`.
* Review [functions.sh] to disambiguate test failure messages.
* Many test cases can be run outside of the test harness, e.g.,
  ```
  bin/sv2v test/core/inc.sv > foo.v && iverilog -g2005 foo.v && ./a.out
  iverilog -g2005 test/core/inc.v && ./a.out
  rm foo.v a.out # when done
  ```
* Consider diffing the output against a prior build of sv2v, e.g., `diff <(sv2v
  test/core/inc.sv) <(bin/sv2v test/core/inc.sv)`.

[functions.sh]: lib/functions.sh
[main GitHub Actions workflow]: ../.github/workflows/main.yaml
