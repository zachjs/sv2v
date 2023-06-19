#!/bin/bash

assertHasPrints() {
    for module in "$@"; do
        assertContains $module "$stdout" "display(\"$module\");"
    done
}

assertDoesNotHavePrints() {
    for module in "$@"; do
        assertNotContains $module "$stdout" "display(\"$module\");"
    done
}

convertSuccessful() {
    runAndCapture main.sv "$@"
    assertTrue "conversion should succeed" $result
    assertNotNull "stdout should not be empty" "$stdout"
    assertNull "stderr should be empty" "$stderr"
}

test_default() {
    convertSuccessful
    assertHasPrints top_a top_b top_d mod intf sub
    assertDoesNotHavePrints top_c
}

test_tops_a() {
    convertSuccessful --top top_a
    assertHasPrints top_a mod
    assertDoesNotHavePrints top_b top_c top_d intf sub
}

test_tops_b() {
    convertSuccessful --top top_b
    assertHasPrints top_b intf sub
    assertDoesNotHavePrints top_a top_c top_d mod
}

test_tops_mod() {
    convertSuccessful --top mod
    assertHasPrints mod
    assertDoesNotHavePrints top_a top_b top_c top_d intf sub
}

test_tops_a_b() {
    convertSuccessful --top top_a --top top_b
    assertHasPrints top_a top_b mod intf sub
    assertDoesNotHavePrints top_c top_d
}

test_tops_a_d() {
    convertSuccessful --top top_a --top top_d
    assertHasPrints top_a top_d mod
    assertDoesNotHavePrints top_b top_c intf sub
}

test_tops_b_d_mod() {
    convertSuccessful --top top_b --top top_d --top mod
    assertHasPrints top_b top_d mod intf sub
    assertDoesNotHavePrints top_a top_c
}

test_error_no_default() {
    runAndCapture main.sv --top top_c
    assertFalse "conversion should fail" $result
    assertNull "stdout should be empty" "$stdout"
    assertContains "$stderr" "Specified top module top_c is missing default parameter value(s) for T, U"
}

test_error_is_an_interface() {
    runAndCapture main.sv --top intf
    assertFalse "conversion should fail" $result
    assertNull "stdout should be empty" "$stdout"
    assertContains "$stderr" "Specified top module intf is an interface. Please"
}

test_error_has_interface_ports() {
    runAndCapture main.sv --top sub
    assertFalse "conversion should fail" $result
    assertNull "stdout should be empty" "$stdout"
    assertContains "$stderr" "Specified top module sub has interface ports. Please"
}

test_error_not_found() {
    runAndCapture main.sv --top fake
    assertFalse "conversion should fail" $result
    assertNull "stdout should be empty" "$stdout"
    assertContains "$stderr" "Could not find top module fake"
}

source ../lib/functions.sh

. shunit2
