#!/bin/bash

addTest() {
    test=$1
    eval "test_$test() { assertConverts $test.sv; }"
    suite_addTest test_$test
}

source ../lib/functions.sh
source ../lib/discover.sh

. shunit2
