#!/bin/bash

addTest() {
    test=$1
    eval "test_$test() { \
        $SV2V $test.sv 2> /dev/null > /dev/null; \
        assertFalse \"conversion should have failed\" \$?; \
    }"
    suite_addTest test_$test
}

source ../lib/functions.sh
source ../lib/discover.sh

. shunit2
