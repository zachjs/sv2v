#!/bin/bash

clearArtifacts() {
    rm -f one.v two.v
    rm -rf dirout
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

test_directory() {
    runAndCapture *.sv
    expected="$stdout"

    rm -f dirout/*
    mkdir -p dirout

    runAndCapture --write dirout *.sv
    assertTrue "directory conversion should succeed" $result
    assertNull "stdout should be empty" "$stdout"
    assertNull "stderr should be empty" "$stderr"

    assertTrue "one.v should exist" "[ -f dirout/one.v ]"
    assertTrue "two.v should exist" "[ -f dirout/two.v ]"
    assertTrue "three.v should exist" "[ -f dirout/three.v ]"
    assertFalse "P.v should not exist" "[ -f dirout/P.v ]"

    actual=`cat dirout/*.v`
    assertEquals "directory output should match combined" "$expected" "$actual"
    clearArtifacts
}

test_directory_pass_through() {
    rm -f dirout/*
    mkdir -p dirout

    runAndCapture --pass-through --write dirout *.sv
    assertTrue "directory conversion should succeed" $result
    assertNull "stdout should be empty" "$stdout"
    assertNull "stderr should be empty" "$stderr"

    assertTrue "one.v should exist" "[ -f dirout/one.v ]"
    assertTrue "two.v should exist" "[ -f dirout/two.v ]"
    assertTrue "three.v should exist" "[ -f dirout/three.v ]"
    assertTrue "P.v should exist" "[ -f dirout/P.v ]"
    clearArtifacts
}

test_directory_directives() {
    module_inp="logic a, b;module example;wire x;endmodule logic c, d;"
    module_out="logic a;
logic b;
module example;
    wire x;
endmodule
logic c;
logic d;"

    check_directory_example "$module_inp" "$module_out"
    check_directory_example "\`default_nettype none\n$module_inp\`resetall" "\`default_nettype none\n$module_out"
    check_directory_example "\`default_nettype none\n\`default_nettype wire\n$module_inp" "\`default_nettype wire\n$module_out"
    check_directory_example "\`celldefine\n$module_inp" "\`celldefine\n$module_out\n\`endcelldefine"
    check_directory_example "\`celldefine\n\`endcelldefine\n$module_inp" "$module_out"
    check_directory_example "\`unconnected_drive pull0\n$module_inp" "\`unconnected_drive pull0\n$module_out\n\`nounconnected_drive"
    check_directory_example "\`unconnected_drive pull0\n\`nounconnected_drive\n$module_inp" "$module_out"
    check_directory_example "\`default_nettype none\n\`celldefine\n\`unconnected_drive pull1\n\`resetall\n$module_inp" "$module_out"
    check_directory_example "\`default_nettype none
\`celldefine
\`unconnected_drive pull1
$module_inp" "\`default_nettype none
\`celldefine
\`unconnected_drive pull1
$module_out
\`nounconnected_drive
\`endcelldefine"
}

check_directory_example() {
    tmp_inp=$SHUNIT_TMPDIR/example.sv
    tmp_out=$SHUNIT_TMPDIR/example.v
    tmp_ref=$SHUNIT_TMPDIR/example_ref.v

    echo -e "$1" > $tmp_inp
    echo -e "$2" | sed -E 's/    /\t/g' > $tmp_ref

    rm -f $tmp_out
    runAndCapture --pass-through --write $SHUNIT_TMPDIR $tmp_inp
    assertTrue "directory conversion should succeed" $result
    assertNull "stdout should be empty: $stdout" "$stdout"
    assertNull "stderr should be empty: $stderr" "$stderr"

    grep -v '//' < $tmp_out > $tmp_out.bak
    mv $tmp_out.bak $tmp_out
    output=`diff --unified $tmp_ref $tmp_out`
    assertTrue "output doesn't match:\n$output" $?
}

test_unknown() {
    runAndCapture --write=unknown *.sv
    assertFalse "unknown write mode should fail" $result
    assertNull "stdout should be empty" "$stdout"
    assertEquals "stderr should list valid write modes" \
        "invalid --write \"unknown\", expected stdout, adjacent, a path ending in .v, or a path to an existing directory" \
        "$stderr"
}

source ../lib/functions.sh

. shunit2
