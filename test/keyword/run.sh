#!/bin/bash

VERSIONS=(
    1364-1995
    1364-2001-noconfig
    1364-2001
    1364-2005
    1800-2005
    1800-2009
    1800-2012
)

testKeyword() {
    sv2v_exit_code=$1
    ivl_exit_code=$(($1*2))
    version=$2
    keyword=$3

    source="\`begin_keywords \"$version\"\nmodule $keyword;endmodule\n\`end_keywords"

    $SV2V <(echo -e "$source") >/dev/null 2>/dev/null
    assertTrue "unexpected sv2v exit code $?" "[ $sv2v_exit_code -eq $? ]"

    iverilog -g2012 -o/dev/null <(echo -e "$source") >/dev/null 2>/dev/null
    assertTrue "unexpected ivl exit code $?" "[ $ivl_exit_code -eq $? ]"
}

addTest() {
    test="$1_$2_$3"
    eval "$test() { testKeyword $1 $2 $3; }"
    suite_addTest $test
}

addTests() {
    prev=
    for curr in "${VERSIONS[@]}"; do
        # This isn't a keyword in prior version.
        [ -n "$prev" -a $curr = $version ] && \
            addTest 0 $prev $keyword && \
            break
        prev=$curr
    done
    # This is a keyword in the specified version.
    addTest 1 $version $keyword
}

suite() {
    for version in "${VERSIONS[@]}"; do
        while read keyword; do
            addTests
        done < $version.txt
    done
}

source ../lib/functions.sh

. shunit2
