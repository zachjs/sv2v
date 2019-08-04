#!/bin/bash

SCRIPT_DIR=`dirname "${BASH_SOURCE[0]}"`

suite() {
    for test in `ls *.sv | sed -e "s_\.sv\\\$__"`; do
        eval "test_$test() { runTest \"$test\"; }"
        suite_addTest "test_$test"
    done
}

source "$SCRIPT_DIR/functions.sh"

. shunit2
