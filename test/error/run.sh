#!/bin/bash

validateOutput() {
    stdout_len=`wc -l < $SHUNIT_TMPDIR/stdout`
    assertEquals "stdout should be empty" 0 $stdout_len
    stderr=`cat $SHUNIT_TMPDIR/stderr`
    assertNotNull "stderr should not be empty" "$stderr"
    line=`head -n1 $1`
    if [[ "$line" =~ \/\/\ pattern:\ .* ]]; then
        pattern=${line:12}
        if [[ ! "$stderr" =~ $pattern ]]; then
            fail "error message doesn't match\nexpected: $pattern\nactual: $stderr"
        fi
    fi
}

addTest() {
    test=$1
    eval "test_$test() { \
        $SV2V $test.sv 2> $SHUNIT_TMPDIR/stderr > $SHUNIT_TMPDIR/stdout; \
        assertFalse \"conversion should have failed\" \$?; \
        validateOutput $test.sv; \
    }"
    suite_addTest test_$test
}

source ../lib/functions.sh
source ../lib/discover.sh

. shunit2
