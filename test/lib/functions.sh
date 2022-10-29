#!/bin/bash

SCRIPT_DIR=`dirname "${BASH_SOURCE[0]}"`
SV2V="$SCRIPT_DIR/../../bin/sv2v +RTS -N1 -RTS"

# USAGE: simulate <vcd-file> <log-file> <top-module> <file> [<file> ...]
simulate() {
    # arguments
    sim_vcd=$1
    sim_log=$2
    sim_top=$3
    shift 3
    # compile the files
    sim_vcd_tmp=$SHUNIT_TMPDIR/simvcdtmp
    sim_prog=$SHUNIT_TMPDIR/simprog.exe
    iv_output=`iverilog \
        -Wall \
        -Wno-portbind \
        -o $sim_prog \
        -g2005 \
        -gstrict-expr-width \
        -DTEST_VCD="\"$sim_vcd_tmp\"" \
        -DTEST_TOP=$sim_top \
        "$@" \
        $SCRIPT_DIR/tb_dumper.v \
        2>&1`
    if [ $? -ne 0 ]; then
        fail "iverilog on $1 failed:\n$iv_output"
        return
    elif [ "$EXPECT_IVERILOG_WARNINGS" != "0" ]; then
        assertNull "iverilog on $1 emitted warnings:\n$iv_output" "$iv_output"
    else
        assertNotNull "iverilog on $1 did not emit any warnings" "$iv_output"
    fi
    # run the simulation
    $sim_prog > $sim_log
    assertTrue "simulating $1 failed" $?
    # remove the date from the VCD
    sed -e "1,3d" < $sim_vcd_tmp > $sim_vcd
}

assertConverts() {
    ac_file=$1

    ac_tmpa=$SHUNIT_TMPDIR/ac-conv-tmpa.v
    convert "1st conversion of $ac_file" $ac_tmpa $ac_file

    # check for un/expected output in the converted result
    ac_pats=$ac_file.pat
    if [ -f $ac_pats ]; then
        while read line; do
            rule=${line:0:6}
            pattern=${line:7}
            grep -G "$pattern" < $ac_tmpa > /dev/null
            matches=$?
            if [ $rule == "affirm" ]; then
                assertTrue "conversion of $ac_file does not contain $pattern" $matches
            elif [ $rule == "reject" ]; then
                assertFalse "conversion of $ac_file contains $pattern" $matches
            else
                fail "unknown rule type: '$rule'"
            fi
        done < $ac_pats
    fi

    ac_tmpb=$SHUNIT_TMPDIR/ac-conv-tmpb.v
    convert "2nd conversion of $ac_file" $ac_tmpb $ac_tmpa

    diff $ac_tmpa $ac_tmpb > /dev/null
    assertTrue "conversion of $ac_file not stable after the first iteration" $?

    ac_tmpc=$SHUNIT_TMPDIR/ac-conv-tmpc.v
    convert "pass through of $ac_file" $ac_tmpc --pass-through $ac_file

    ac_tmpd=$SHUNIT_TMPDIR/ac-conv-tmpd.v
    convert "conversion of pass through of $ac_file" $ac_tmpd $ac_tmpc

    diff $ac_tmpa $ac_tmpd > /dev/null
    assertTrue "pass through then conversion differs for $ac_file" $?

    # using sed to remove quoted strings
    filtered=`sed -E 's/"([^"]|\")+"//g' $ac_tmpa`
    # check for various things iverilog accepts which we don't want to output
    prefix="conversion of $ac_file still contains"
    assertNotMatch "$prefix dimension queries" "$filtered" \
        '\$bits|\$dimensions|\$unpacked_dimensions|\$left|\$right|\$low|\$high|\$increment|\$size'
    assertNotMatch "$prefix SystemVerilog types" "$filtered" \
        '[[:space:]](int|bit|logic|byte|struct|enum|longint|shortint)[[:space:]]'
    assertNotMatch "$prefix unsigned keyword" "$filtered" \
        '[^\$a-zA-Z_]unsigned'
}

extractFlag() {
    raw_line=`grep -m1 "^\/\/ $1: " $2`
    to_drop=$((${#1}+5))
    flag="${raw_line:to_drop}"
}

assertMatch() {
    if [[ ! "$2" =~ $3 ]]; then
        fail "$1 doesn't match\nexpected: $3\nactual: $2"
    fi
}

assertNotMatch() {
    if [[ "$2" =~ $3 ]]; then
        fail "$1"
    fi
}

# convert SystemVerilog source file(s)
convert() {
    description=$1
    out_file=$2
    shift 2
    $SV2V "$@" 2> $SHUNIT_TMPDIR/stderr > $out_file
    assertTrue "$description failed" $?
    if [ -s $SHUNIT_TMPDIR/stderr ]; then
        fail "$description emitted warnings:"
        cat $SHUNIT_TMPDIR/stderr
    fi
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

    cs=$SHUNIT_TMPDIR/cs.v
    convert "standard conversion" $cs $sv

    cv=$SHUNIT_TMPDIR/cv.v
    convert "verbose conversion" $cv $sv -v

    simulateAndCompare $ve $cs $cv $tb
}

simulateAndCompare() {
    ve=$1 # reference verilog
    cs=$2 # converted succinct
    cv=$3 # converted verbose
    tb=$4 # testbench

    ref_vcd=$SHUNIT_TMPDIR/ref.vcd
    cvs_vcd=$SHUNIT_TMPDIR/cvs.vcd
    cvv_vcd=$SHUNIT_TMPDIR/cvv.vcd
    ref_log=$SHUNIT_TMPDIR/ref.log
    cvs_log=$SHUNIT_TMPDIR/cvs.log
    cvv_log=$SHUNIT_TMPDIR/cvv.log

    # simulate the three files
    simulate $ref_vcd $ref_log top $ve $tb
    simulate $cvs_vcd $cvs_log top $cs $tb
    simulate $cvv_vcd $cvv_log top $cv $tb

    # compare reference verilog to converted succinct
    output=`diff $ref_vcd $cvs_vcd`
    assertTrue "VE/CS VCDs are different:\n$output" $?
    output=`diff $ref_log $cvs_log`
    assertTrue "VE/CS simulation outputs differ:\n$output" $?

    # compare converted verbose to converted succinct
    output=`diff $cvv_vcd $cvs_vcd`
    assertTrue "CV/CS VCDs are different:\n$output" $?
    output=`diff $cvv_log $cvs_log`
    assertTrue "CV/CS simulation outputs differ:\n$output" $?

    rm -f $ref_vcd $cvs_vcd $cvv_vcd $ref_log $cvs_log $cvv_log
}

runTest() {
    test=$1
    simpleTest "${test}.sv" "${test}.v" "${test}_tb.v"
}

runAndCapture() {
    $SV2V "$@" > "$SHUNIT_TMPDIR/stdout" 2> "$SHUNIT_TMPDIR/stderr"
    result=$?
    stdout=`cat $SHUNIT_TMPDIR/stdout`
    stderr=`cat $SHUNIT_TMPDIR/stderr`
}
