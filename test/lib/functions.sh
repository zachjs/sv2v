#!/bin/bash

SCRIPT_DIR=`dirname "${BASH_SOURCE[0]}"`
SV2V="$SCRIPT_DIR/../../bin/sv2v"

assertExists() {
    file=$1
    [ -f "$file" ]
    assertTrue "$file does not exist" $?
}

# USAGE: simulate <vcd-file> <log-file> <top-module> <file> [<file> ...]
simulate() {
    # arguments
    sim_vcd="$1"; shift
    sim_log="$1"; shift
    sim_top="$1"; shift
    # compile the files
    sim_prog="$SHUNIT_TMPDIR/simprog.exe"
    iv_output=`iverilog \
        -Wall \
        -Wno-select-range \
        -o "$sim_prog" \
        -g2005 \
        -DTEST_VCD="\"$sim_vcd\"" \
        -DTEST_TOP=$sim_top \
        "$SCRIPT_DIR/tb_dumper.v" \
        "$@" 2>&1`
    assertTrue "iverilog on $1 failed" $?
    assertNull "iverilog emitted warnings:" "$iv_output"
    if [ "$iv_output" != "" ]; then
        echo "$iv_output"
    fi
    # run the simulation
    $sim_prog > $sim_log
    assertTrue "simulating $1 failed" $?
    # remove the date from the VCD
    sed -i.orig -e "1,3d" "$sim_vcd"
    # remove the "opened file..." prompt from the log
    sed -i.orig -e "1,1d" "$sim_log"
}

assertConverts() {
    ac_file="$1"
    ac_tmpa="$SHUNIT_TMPDIR/ac-conv-tmpa.v"
    ac_tmpb="$SHUNIT_TMPDIR/ac-conv-tmpb.v"
    ac_tmpc="$SHUNIT_TMPDIR/ac-conv-tmpc.v"
    $SV2V "$ac_file" 2> /dev/null > "$ac_tmpa"
    assertTrue "1st conversion of $ac_file failed" $?
    $SV2V "$ac_tmpa" 2> /dev/null > "$ac_tmpb"
    assertTrue "2nd conversion of $ac_file failed" $?
    $SV2V "$ac_tmpb" 2> /dev/null > "$ac_tmpc"
    assertTrue "3rd conversion of $ac_file failed" $?
    diff "$ac_tmpb" "$ac_tmpc" > /dev/null
    assertTrue "conversion of $ac_file not stable after the second iteration" $?
    # using sed to remove quoted strings
    filtered=`sed -E 's/"([^"]|\")+"//g' "$ac_tmpa"`
    echo "$filtered" | grep "\$bits" > /dev/null
    assertFalse "conversion of $ac_file still contains \$bits" $?
    echo "$filtered" | grep "\]\[" > /dev/null
    assertFalse "conversion of $ac_file still contains multi-dim arrays" $?
    echo "$filtered" | egrep "\s(int\|bit\|logic\|byte\|struct\|enum\|longint\|shortint)\s"
    assertFalse "conversion of $ac_file still contains SV types" $?
}

# convert SystemVerilog source file(s)
convert() {
    out_file="$1"; shift
    $SV2V "$@" 2> /dev/null > "$out_file"
    assertTrue "conversion failed" $?
    assertExists "$out_file"
}

simpleTest() {
    sv="$1"
    ve="$2"
    tb="$3"

    assertNotNull "SystemVerilog file not specified" $sv
    assertNotNull "Verilog file not specified" $ve
    assertNotNull "Testbench not specified" $tb

    # some tests don't have a separate testbench, instead having the top-level
    # module defined in both of the input files
    if [ ! -f "$tb" ]; then
        tb="$SCRIPT_DIR/empty.v"
    fi

    assertExists $sv
    assertExists $ve
    assertExists $tb

    assertConverts "$sv"
    assertConverts "$ve"
    assertConverts "$tb"

    cv="$SHUNIT_TMPDIR/conv.v"
    convert "$cv" "$sv"

    simulateAndCompare "$ve" "$cv" "$tb"
}

simulateAndCompare() {
    ve="$1"
    cv="$2"
    tb="$3"

    ref_vcd="$SHUNIT_TMPDIR/ref.vcd"
    gen_vcd="$SHUNIT_TMPDIR/gen.vcd"
    ref_log="$SHUNIT_TMPDIR/ref.log"
    gen_log="$SHUNIT_TMPDIR/gen.log"

    # simulate and compare the two files
    simulate "$ref_vcd" "$ref_log" top "$ve" "$tb"
    simulate "$gen_vcd" "$gen_log" top "$cv" "$tb"
    diff "$ref_vcd" "$gen_vcd" > /dev/null
    assertTrue "VCDs are different" $?
    output=`diff "$ref_log" "$gen_log"`
    assertTrue "Simulation outputs differ:\n$output" $?
}

runTest() {
    test="$1"
    simpleTest "${test}.sv" "${test}.v" "${test}_tb.v"
}
