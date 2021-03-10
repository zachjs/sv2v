#!/bin/bash

test_default() {
    cv="$SHUNIT_TMPDIR/conv.v"
    convert "$cv" package.svh module.sv
    simulateAndCompare "reference.v" "$cv" "$SCRIPT_DIR/empty.v"
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
