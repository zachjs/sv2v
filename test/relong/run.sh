#!/bin/sh

assertExists() {
    file=$1
    [ -f "$file" ]
    assertTrue "$file does not exist" $?
}

# USAGE: simulate <vcd-outfile> <top-module> <file> [<file> ...]
simulate() {
    # arguments
    sim_outfile="$1"; shift
    sim_top="$1"; shift
    # compile the files
    sim_prog="$SHUNIT_TMPDIR/simprog.exe"
    iverilog \
        -o "$sim_prog" \
        -g2005 \
        -DTEST_VCD="\"$sim_outfile\"" \
        -DTEST_TOP=$sim_top \
        "tb_dumper.v" \
        "$@"
    assertTrue "iverilog on $1 failed" $?
    # run the simulation
    $sim_prog > /dev/null
    assertTrue "simulating $1 failed" $?
    # remove the date from the VCD
    sed -i.orig -e "1,3d" "$sim_outfile"
}

runTest() {
    test=$1
    assertNotNull "test not specified" $test

    sv="$test.sv"
    ve="$test.v"
    tb="${test}_tb.v"

    assertExists $sv
    assertExists $ve
    assertExists $sv

    # convert the SystemVerilog source file
    cv="$SHUNIT_TMPDIR/conv-$test.v"
    ../../bin/sv2v $sv 2> /dev/null > $cv
    assertTrue "conversion failed" $?
    assertExists $cv

    ref_vcd="$SHUNIT_TMPDIR/ref.vcd"
    gen_vcd="$SHUNIT_TMPDIR/gen.vcd"

    # simulate and compare the two files
    simulate "$ref_vcd" top "$ve" "$tb"
    simulate "$gen_vcd" top "$cv" "$tb"
    diff "$ref_vcd" "$gen_vcd" > /dev/null
    assertTrue "VCDs are different" $?
}

suite() {
    for test in `ls *.sv | sed -e "s_\.sv\\\$__"`; do
        eval "test_$test() { runTest \"$test\"; }"
        suite_addTest "test_$test"
    done
}

. shunit2
