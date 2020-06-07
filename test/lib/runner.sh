#!/bin/bash

SCRIPT_DIR=`dirname "${BASH_SOURCE[0]}"`

addTest() {
    test=$1
    eval "test_$test() { runTest \"$test\"; }"
    suite_addTest test_$test
}

source $SCRIPT_DIR/functions.sh
source $SCRIPT_DIR/discover.sh

. shunit2
