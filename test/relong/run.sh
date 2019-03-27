#!/bin/sh

SV2V=../../bin/sv2v

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

assertConverts() {
    ac_file="$1"
    ac_temp="$SHUNIT_TMPDIR/ac-conv-temp.v"
    $SV2V "$ac_file" 2> /dev/null > "$ac_temp"
    assertTrue "1st conversion of $ac_file failed" $?
    $SV2V "$ac_temp" 2> /dev/null > /dev/null
    assertTrue "2nd conversion of $ac_file failed" $?
}

runTest() {
    test=$1
    assertNotNull "test not specified" $test

    sv="$test.sv"
    ve="$test.v"
    tb="${test}_tb.v"

    assertExists $sv
    assertExists $ve
    assertExists $tb

    assertConverts "$sv"
    assertConverts "$ve"
    assertConverts "$tb"

    # convert the SystemVerilog source file
    cv="$SHUNIT_TMPDIR/conv-$test.v"
    $SV2V $sv 2> /dev/null > $cv
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
