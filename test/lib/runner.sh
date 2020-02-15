#!/bin/bash

SCRIPT_DIR=`dirname "${BASH_SOURCE[0]}"`

tests=(`ls *.sv | sed -e "s_\.sv\\\$__"`)

if [ $1 ]; then
    tests=("$@")
    shift ${#tests[@]}
    for test in $tests; do
        if [ ! -f $test.sv ]; then
            echo "Could not find $test.sv"
            exit 1
        fi
    done
fi

addTest() {
    test=$1
    eval "test_$test() { runTest \"$test\"; }"
    suite_addTest test_$test
}

suite() {
    for test in "${tests[@]}"; do
        addTest $test
    done
}

source $SCRIPT_DIR/functions.sh

. shunit2
