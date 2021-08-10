#!/bin/bash

testNumber() {
    mode=$1
    number=$2

    ve=$SHUNIT_TMPDIR/ve.v
    cs=$SHUNIT_TMPDIR/cs.v

    ve_log=$ve.log
    cs_log=$cs.log

    # substitute the current number literal into a copy of the test template
    sed -e "s/NUM/$number/" < template.v > $ve

    # convert in strict mode
    runAndCapture $ve
    assertTrue "conversion should succeed" $result
    assertNotNull "stdout should not be empty" "$stdout"
    if [ $mode = no_trunc ]; then
        assertNull "stderr should be empty:\n$stderr" "$stderr"
    else
        assertNotNull "stderr should not be empty" "$stderr"
    fi

    # convert result in strict mode
    mv -f $SHUNIT_TMPDIR/stdout $cs
    runAndCapture $cs
    assertTrue "conversion should succeed" $result
    assertNotNull "stdout should not be empty" "$stdout"
    assertNull "stderr should be empty:\n$stderr" "$stderr"

    # simulate and compare in strict mode
    EXPECT_IVERILOG_WARNINGS=`[ $mode = trunc_ivl_warns ]; echo $?` \
    simulate /dev/null $ve_log top $ve
    simulate /dev/null $cs_log top $cs
    output=`diff $ve_log $cs_log`
    assertTrue "number literals differ:\n$output" $?

    # convert in lax mode
    runAndCapture --oversized-numbers $ve
    assertTrue "conversion should succeed" $result
    assertNotNull "stdout should not be empty" "$stdout"
    if [ $mode = no_trunc ] || [[ ! "$number" =~ .\' ]]; then
        assertNull "stderr should be empty:\n$stderr" "$stderr"
    else
        assertNotNull "stderr should not be empty" "$stderr"
    fi

    # convert result in lax mode
    mv -f $SHUNIT_TMPDIR/stdout $cs
    runAndCapture --oversized-numbers $cs
    assertTrue "conversion should succeed" $result
    assertNotNull "stdout should not be empty" "$stdout"
    assertNull "stderr should be empty:\n$stderr" "$stderr"

    # simulate and compare in lax mode
    EXPECT_IVERILOG_WARNINGS=`[[ "$number" =~ .\' ]] && [ $mode = trunc_ivl_warns ]; echo $?` \
    simulate /dev/null $ve_log top -gno-strict-expr-width $ve
    simulate /dev/null $cs_log top -gno-strict-expr-width $cs
    output=`diff $ve_log $cs_log`
    assertTrue "number literals differ:\n$output" $?
}

addTest() {
    mode=$1
    number=$2
    test="${mode}_${number//\'/_}"
    eval "$test() { testNumber $mode \"$number\"; }"
    suite_addTest $test
}

suite() {
    modes=(no_trunc trunc_ivl_warns trunc_ivl_silent)
    for mode in ${modes[@]}; do
        while read number; do
            [ -n "$number" ] && \
                addTest $mode "$number"
        done < $mode.txt
    done
}

source ../lib/functions.sh

. shunit2
