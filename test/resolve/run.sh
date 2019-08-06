#!/bin/bash

test_main() {
    cv="$SHUNIT_TMPDIR/conv.v"
    convert "$cv" --oneunit package.svh module.sv
    simulateAndCompare "reference.v" "$cv" "$SCRIPT_DIR/empty.v"
}

suite() {
    suite_addTest "test_main"
}

source ../lib/functions.sh

. shunit2
