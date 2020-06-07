#!/bin/bash

test_main() {
    cv="$SHUNIT_TMPDIR/conv.v"
    convert "$cv" --oneunit package.svh module.sv
    simulateAndCompare "reference.v" "$cv" "$SCRIPT_DIR/empty.v"
}

source ../lib/functions.sh

. shunit2
