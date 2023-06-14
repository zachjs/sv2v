#!/bin/bash

evaluate() {
    design_v=$SHUNIT_TMPDIR/search_design.v
    output_log=$SHUNIT_TMPDIR/search.log
    touch $output_log
    simulate /dev/null $output_log top <(echo "$1") /dev/null
    tail -n1 $output_log
}

search() {
    top_sv=$SHUNIT_TMPDIR/search_top.sv
    echo "module top; $mod m(); endmodule" > $top_sv
    runAndCapture "$@" $top_sv
}

searchAndEvaluate() {
    search "$@"
    assertTrue "$mod conversion should succeed" $result
    assertNotNull "$mod stdout should not be empty" "$stdout"
    assertNull "$mod stderr should be empty" "$stderr"
    output=`evaluate "$stdout"`
}

checkFound() {
    searchAndEvaluate "$@"
    assertEquals "simulation output should match" $mod "$output"
}

checkNotFound() {
    searchAndEvaluate "$@"
    assertContains "iverilog should fail" "$output" "$mod referenced 1 times"
}

test_found() {
    for mod in apple orange; do
        checkFound -y.
        checkFound -y../base -y.
        checkFound -y. -y../base
    done
}

test_not_found_default() {
    for mod in apple orange; do
        checkNotFound
    done
}

test_not_found_missing() {
    for mod in apple orange doesnt_exist; do
        checkNotFound -y../base
    done
}

test_misdirect() {
    mod=misdirect
    runAndCapture -y. <(echo "module top; $mod m(); endmodule")
    assertFalse "conversion should not succeed" $result
    assertNull "stdout should be empty" "$stdout"
    assertContains "stderr should match expected error" "$stderr" \
        'Expected to find module or interface "misdirect" in file "./misdirect.sv" selected from the library path.'
}

test_found_write_adjacent() {
    files=(apple.v orange.v top.v)
    for file in "${files[@]}"; do
        assertTrue "$file should not exist" "[ ! -f $file ]"
    done

    runAndCapture -y. -wadj top.sv
    assertTrue "conversion should succeed" $result
    assertNull "stdout should be empty" "$stdout"
    assertNull "stderr should be empty" "$stderr"

    for file in "${files[@]}"; do
        assertTrue "$file should exist" "[ -f $file ]"
        rm -f $file
    done
}

source ../lib/functions.sh

. shunit2
