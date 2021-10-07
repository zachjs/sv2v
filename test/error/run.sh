#!/bin/bash

runErrorTest() {
    extractFlag pattern $1.sv
    pattern="${flag:-.}"
    extractFlag location $1.sv
    location="${flag//./\.}"
    location="${location:-.}"

    runAndCapture $1.sv
    assertFalse "regular conversion should have failed" $result
    assertNull "regular stdout should be empty" "$stdout"
    assertNotNull "regular stderr should not be empty" "$stderr"
    assertMatch "regular error message" "$stderr" "$pattern"

    runAndCapture -v $1.sv
    assertFalse "verbose conversion should have failed" $result
    assertNull "verbose stdout should be empty" "$stdout"
    assertNotNull "verbose stderr should not be empty" "$stderr"
    assertMatch "verbose error message" "$stderr" "$pattern"
    assertMatch "verbose location" "$stderr" "$location[^0-9]"
}

addTest() {
    test=$1
    eval "test_$test() { runErrorTest $test; }"
    suite_addTest test_$test
}

source ../lib/functions.sh
source ../lib/discover.sh

. shunit2
