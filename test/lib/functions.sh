#!/bin/bash

SCRIPT_DIR=`dirname "${BASH_SOURCE[0]}"`
SV2V="$SCRIPT_DIR/../../bin/sv2v +RTS -N1 -RTS"

assertExists() {
    file=$1
    [ -f "$file" ]
    assertTrue "$file does not exist" $?
}

# USAGE: simulate <vcd-file> <log-file> <top-module> <file> [<file> ...]
simulate() {
    # arguments
    sim_vcd=$1
    sim_log=$2
    sim_top=$3
    shift 3
    # compile the files
    sim_prog=$SHUNIT_TMPDIR/simprog.exe
    iv_output=`iverilog \
        -Wall \
        -Wno-select-range \
        -Wno-anachronisms \
        -o $sim_prog \
        -g2005 \
        -DTEST_VCD="\"$sim_vcd\"" \
        -DTEST_TOP=$sim_top \
        $SCRIPT_DIR/tb_dumper.v \
        "$@" 2>&1`
    assertTrue "iverilog on $1 failed" $?
    if [ -n "$iv_output" ]; then
        assertNull "iverilog emitted warnings:" "$iv_output"
        echo "$iv_output"
    fi
    # run the simulation
    $sim_prog > $sim_log.temp
    assertTrue "simulating $1 failed" $?
    assertExists $sim_vcd
    # remove the date from the VCD
    sed -i.orig -e "1,3d" $sim_vcd
    # remove extraneous log lines
    cat $sim_log.temp | grep -v "VCD info: dumpfile" > $sim_log
}

assertConverts() {
    ac_file=$1
    ac_tmpa=$SHUNIT_TMPDIR/ac-conv-tmpa.v
    $SV2V $ac_file 2> /dev/null > $ac_tmpa
    assertTrue "1st conversion of $ac_file failed" $?
    ac_tmpb=$SHUNIT_TMPDIR/ac-conv-tmpb.v
    $SV2V $ac_tmpa 2> /dev/null > $ac_tmpb
    assertTrue "2nd conversion of $ac_file failed" $?
    if [ -n "$(diff $ac_tmpa $ac_tmpb)" ]; then
        ac_tmpc=$SHUNIT_TMPDIR/ac-conv-tmpc.v
        $SV2V $ac_tmpb 2> /dev/null > $ac_tmpc
        assertTrue "3rd conversion of $ac_file failed" $?
        diff $ac_tmpb $ac_tmpc > /dev/null
        assertTrue "conversion of $ac_file not stable after the second iteration" $?
    fi
    $SV2V -v $ac_file 2> /dev/null > /dev/null
    assertTrue "verbose conversion of $ac_file failed" $?
    # using sed to remove quoted strings
    filtered=`sed -E 's/"([^"]|\")+"//g' $ac_tmpa`
    # check for various things iverilog accepts which we don't want to output
    PATTERNS="\$bits\|\$dimensions\|\$unpacked_dimensions\|\$left\|\$right\|\$low\|\$high\|\$increment\|\$size"
    echo "$filtered" | grep "$PATTERNS" > /dev/null
    assertFalse "conversion of $ac_file still contains dimension queries" $?
    echo "$filtered" | egrep "\s(int\|bit\|logic\|byte\|struct\|enum\|longint\|shortint)\s"
    assertFalse "conversion of $ac_file still contains SV types" $?
    echo "$filtered" | grep "[^\$a-zA-Z_]unsigned" > /dev/null
    assertFalse "conversion of $ac_file still contains unsigned keyword" $?
}

# convert SystemVerilog source file(s)
convert() {
    out_file=$1; shift
    $SV2V "$@" 2> /dev/null > $out_file
    assertTrue "conversion failed" $?
}

simpleTest() {
    sv=$1
    ve=$2
    tb=$3

    assertConverts $sv

    # some tests use inputs compatible with iverilog directly and so omit the
    # reference manually converted file
    if [ ! -f $ve ]; then
        ve=$sv
    else
        assertConverts $ve
    fi

    # some tests don't have a separate testbench, instead having the top-level
    # module defined in both of the input files
    if [ ! -f $tb ]; then
        tb=$SCRIPT_DIR/empty.v
    else
        assertConverts $tb
    fi

    cv=$SHUNIT_TMPDIR/conv.v
    convert $cv $sv

    simulateAndCompare $ve $cv $tb
}

simulateAndCompare() {
    ve=$1
    cv=$2
    tb=$3

    ref_vcd=$SHUNIT_TMPDIR/ref.vcd
    gen_vcd=$SHUNIT_TMPDIR/gen.vcd
    ref_log=$SHUNIT_TMPDIR/ref.log
    gen_log=$SHUNIT_TMPDIR/gen.log

    # simulate and compare the two files
    simulate $ref_vcd $ref_log top $ve $tb
    simulate $gen_vcd $gen_log top $cv $tb
    output=`diff $ref_vcd $gen_vcd`
    assertTrue "VCDs are different:\n$output" $?
    output=`diff $ref_log $gen_log`
    assertTrue "Simulation outputs differ:\n$output" $?

    rm -f $ref_vcd
    rm -f $gen_vcd
    rm -f $ref_log
    rm -f $gen_log
}

runTest() {
    test=$1
    simpleTest "${test}.sv" "${test}.v" "${test}_tb.v"
}
