#!/bin/bash

test_help() {
    runAndCapture --help
    assertTrue "getting help should succeed" $result
    assertNotNull "stdout should not be empty" "$stdout"
    assertNull "stderr should be empty" "$stderr"
    lines=`echo "$stdout" | wc -l`
    usage=`echo "$stdout" | tail -n +3 | head -n $(expr $lines - 5)`
    usage="\`\`\`
$usage
\`\`\`"
    if [[ ! $(<../../README.md) = *"$usage"* ]]; then
        fail "Did not find matching usage in README!"
    fi
}

source ../lib/functions.sh

. shunit2
