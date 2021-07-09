#!/bin/bash

test_main() {
    runAndCapture -DDEFINED -DEND=endmodule -D"START=module top;" main.sv
    assertTrue "conversion should succeed" $result
    assertNotNull "stdout should not be empty" "$stdout"
    assertNull "stderr should be empty" "$stderr"

    substituted=$SHUNIT_TMPDIR/main.sv
    echo "$stdout" > $substituted
    simpleTest $substituted main.v _
}

source ../lib/functions.sh

. shunit2
