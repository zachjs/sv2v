#!/bin/bash

runErrorTest() {
    runAndCapture $1.sv
    assertFalse "conversion should have failed" $result
    assertNull "stdout should be empty" "$stdout"
    assertNotNull "stderr should not be empty" "$stderr"
    line=`head -n1 $1.sv`
    if [[ "$line" =~ \/\/\ pattern:\ .* ]]; then
        pattern=${line:12}
        if [[ ! "$stderr" =~ $pattern ]]; then
            fail "error message doesn't match\nexpected: $pattern\nactual: $stderr"
        fi
    fi
}

addTest() {
    test=$1
    eval "test_$test() { runErrorTest $test; }"
    suite_addTest test_$test
}

source ../lib/functions.sh
source ../lib/discover.sh

. shunit2
