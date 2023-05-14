#!/bin/bash

NO_FILES_WARNING="Warning: No input files specified (try \`sv2v --help\`)"
PACKAGE_WARNING="Warning: Source includes packages but no modules. Please convert packages alongside the modules that use them."
INTERFACE_WARNING="Warning: Source includes an interface but output is empty because there is no top-level module which has no ports which are interfaces."
PORT_CONN_ATTR_WARNING="attr.sv:6:11: Warning: Ignored port connection attributes (* foo *)(* bar *)."
DEPRECATED_D_WARNING="Deprecation warning: -d has been renamed to -D"
DEPRECATED_E_WARNING="Deprecation warning: -e has been renamed to -E"
DEPRECATED_I_WARNING="Deprecation warning: -i has been renamed to -I"

test_default() {
    runAndCapture interface.sv module.sv package.sv
    assertTrue "default conversion should succeed" $result
    assertNotNull "stdout should not be empty" "$stdout"
    assertNull "stderr should be empty" "$stderr"
}

test_no_files() {
    runAndCapture
    assertTrue "conversion should succeed" $result
    assertNull "stdout should be empty" "$stdout"
    assertEquals "stderr should should have warning" "$NO_FILES_WARNING" "$stderr"
}

test_port_conn_attr() {
    runAndCapture attr.sv
    assertTrue "conversion should succeed" $result
    assertNotNull "stdout should not be empty" "$stdout"
    assertEquals "stderr should should have warning" "$PORT_CONN_ATTR_WARNING" "$stderr"
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

test_only_localparam() {
    runAndCapture localparam.sv
    assertTrue "conversion should succeed" $result
    assertNull "stdout should be empty" "$stdout"
    assertNull "stderr should be empty" "$stderr"
}

test_only_localparam_verbose() {
    runAndCapture -v localparam.sv
    assertTrue "conversion should succeed" $result
    assertNotNull "stdout should not be empty" "$stdout"
    assertNull "stderr should be empty" "$stderr"
}

test_deprecated_d() {
    runAndCapture -d FOO -v localparam.sv
    assertTrue "conversion should succeed" $result
    assertNotNull "stdout should not be empty" "$stdout"
    assertEquals "stderr should have warning" "$DEPRECATED_D_WARNING" "$stderr"
}

test_deprecated_e() {
    runAndCapture -e assert -v localparam.sv
    assertTrue "conversion should succeed" $result
    assertNotNull "stdout should not be empty" "$stdout"
    assertEquals "stderr should have warning" "$DEPRECATED_E_WARNING" "$stderr"
}

test_deprecated_i() {
    runAndCapture -i. -v localparam.sv
    assertTrue "conversion should succeed" $result
    assertNotNull "stdout should not be empty" "$stdout"
    assertEquals "stderr should have warning" "$DEPRECATED_I_WARNING" "$stderr"
}

source ../lib/functions.sh

. shunit2
