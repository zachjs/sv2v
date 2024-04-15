#!/bin/bash

test_help() {
    runAndCapture --help
    assertTrue "getting help should succeed" $result
    assertNotNull "stdout should not be empty" "$stdout"
    assertNull "stderr should be empty" "$stderr"
    python3 check_usage.py
}

source ../lib/functions.sh

. shunit2
