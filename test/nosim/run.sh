#!/bin/bash

runNoSimTest() {
    file=$1.sv
    assertConverts $file

    tmp_ref=$SHUNIT_TMPDIR/no-sim-ref.sv
    tmp_int=$SHUNIT_TMPDIR/no-sim-int.sv
    tmp_alt=$SHUNIT_TMPDIR/no-sim-alt.sv

    convert "reference $file" $tmp_ref $file
    convert "intermediate $file" $tmp_int --exclude assert $file
    convert "alternate $file" $tmp_alt $tmp_int

    diff --unified $tmp_ref $tmp_alt
    assertTrue "reference and alternate conversion differs for $file" $?
}

addTest() {
    test=$1
    eval "test_$test() { runNoSimTest $test; }"
    suite_addTest test_$test
}

source ../lib/functions.sh
source ../lib/discover.sh

. shunit2
