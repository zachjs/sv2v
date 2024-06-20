#!/bin/bash

test_basic() {
    out=$SHUNIT_TMPDIR/out.v
    runAndCapture --bugpoint="y = 4" --bugpoint="z = y" \
         --top top before.sv -w $out
    assertTrue "bugpoint conversion should succeed" $result
    assertNull "stdout should be empty" "$stdout"
    assertNotNull "stderr should not be empty" "$stderr"

    sed -i.bak -E 's/\t/    /g' $out
    echo >> $out
    diff --unified after.sv $out
    assertTrue "minimized output doesn't match" $?
}

source ../lib/functions.sh

. shunit2
