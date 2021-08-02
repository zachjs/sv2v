#!/bin/bash

test_default() {
    cs=$SHUNIT_TMPDIR/cs.v
    cv=$SHUNIT_TMPDIR/cv.v
    convert "standard conversion" $cs package.svh module.sv
    convert "verbose conversion" $cv package.svh module.sv -v
    simulateAndCompare reference.v $cs $cv "$SCRIPT_DIR/empty.v"
}

test_siloed() {
    runAndCapture --siloed package.svh module.sv
    assertFalse "siloed conversion should fail" $result
    assertNull "stdout should be empty" "$stdout"
    assertEquals "stderr should have missing macro" \
        "module.sv:8:35: Lexical error: Undefined macro: FANCY_SEEING_YOU" \
        "$stderr"
}

source ../lib/functions.sh

. shunit2
