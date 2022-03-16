#!/bin/bash

clearArtifacts() {
    rm -f one.v two.v
}

createArtifacts() {
    touch one.v two.v
}

test_prereq() {
    for file in `ls *.sv`; do
        assertConverts "$file"
    done
}

test_default() {
    runAndCapture *.sv
    assertTrue "default conversion should succeed" $result
    assertNotNull "stdout should not be empty" "$stdout"
    assertNull "stderr should be empty" "$stderr"
}

test_stdout() {
    runAndCapture --write=stdout *.sv
    assertTrue "default conversion should succeed" $result
    assertNotNull "stdout should not be empty" "$stdout"
    assertNull "stderr should be empty" "$stderr"
}

test_adjacent() {
    runAndCapture --write=stdout *.sv
    expected="$stdout"

    runAndCapture --write=adjacent *.sv
    assertTrue "adjacent conversion should succeed" $result
    assertNull "stdout should be empty" "$stdout"
    assertNull "stderr should be empty" "$stderr"

    actual=`cat one.v two.v`
    assertEquals "adjacent output should match combined" "$expected" "$actual"
    clearArtifacts
}

test_adjacent_exist() {
    createArtifacts
    runAndCapture --write=adjacent *.sv
    assertTrue "adjacent conversion should overwrite and succeed" $result
    assertNull "stdout should be empty" "$stdout"
    assertNull "stderr should be empty" "$stderr"

    actual=`cat one.v two.v`
    assertEquals "adjacent output should match combined" "$expected" "$actual"
    clearArtifacts
}

test_adjacent_extension() {
    createArtifacts
    runAndCapture --write=adjacent *.v
    clearArtifacts

    assertFalse "adjacent conversion should fail" $result
    assertNull "stdout should be empty" "$stdout"
    assertEquals "stderr should list existing files" \
        "Refusing to write adjacent to \"one.v\" because that path does not end in \".sv\"" \
        "$stderr"
}

test_file() {
    runAndCapture --write=stdout *.sv
    expected="$stdout"

    rm -f out.v
    runAndCapture --write=out.v *.sv
    assertTrue "file conversion should succeed" $result
    assertNull "stdout should be empty" "$stdout"
    assertNull "stderr should be empty" "$stderr"

    actual=`cat out.v`
    assertEquals "file output should match combined" "$expected" "$actual"
    clearArtifacts
}

test_unknown() {
    runAndCapture --write=unknown *.sv
    assertFalse "unknown write mode should fail" $result
    assertNull "stdout should be empty" "$stdout"
    assertEquals "stderr should list valid write modes" \
        "invalid --write \"unknown\", expected stdout, adjacent, or a path ending in .v" \
        "$stderr"
}

source ../lib/functions.sh

. shunit2
