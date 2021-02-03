#!/bin/bash

PACKAGE_WARNING="Warning: Source includes packages but no modules. Please convert packages alongside the modules that use them."
INTERFACE_WARNING="Warning: Source includes an interface but output is empty because there is no top-level module which has no ports which are interfaces."

test_default() {
    runAndCapture *.sv
    assertTrue "default conversion should succeed" $result
    assertNotNull "stdout should not be empty" "$stdout"
    assertNull "stderr should be empty" "$stderr"
}

test_only_package() {
    runAndCapture package.sv
    assertTrue "conversion should succeed" $result
    assertNull "stdout should be empty" "$stdout"
    assertEquals "stderr should have warning" "$PACKAGE_WARNING" "$stderr"
}

test_only_package_verbose() {
    runAndCapture -v package.sv
    assertTrue "conversion should succeed" $result
    assertNotNull "stdout should not be empty" "$stdout"
    assertEquals "stderr should have warning" "$PACKAGE_WARNING" "$stderr"
}

test_only_interface() {
    runAndCapture interface.sv
    assertTrue "conversion should succeed" $result
    assertNull "stdout should be empty" "$stdout"
    assertEquals "stderr should have warning" "$INTERFACE_WARNING" "$stderr"
}

test_only_interface_verbose() {
    runAndCapture -v interface.sv
    assertTrue "conversion should succeed" $result
    assertNotNull "stdout should not be empty" "$stdout"
    assertEquals "stderr should have warning" "$INTERFACE_WARNING" "$stderr"
}

source ../lib/functions.sh

. shunit2
