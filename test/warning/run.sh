#!/bin/bash

NO_FILES_WARNING="Warning: No input files specified (try \`sv2v --help\`)"
INTERFACE_WARNING="Warning: Source includes an interface but the output is empty because there are no modules without any interface ports. Please convert interfaces alongside the modules that instantiate them."
PORT_CONN_ATTR_WARNING="attr.sv:6:11: Warning: Ignored port connection attributes (* foo *)(* bar *)."
DUPLICATE_MODULE_WARNING="module_dupe_2.sv:2:5: Warning: Redefinition of \"top\". Previously defined at module_dupe_1.sv:2:5."

test_default() {
    runAndCapture \
        interface.sv module.sv \
        package.sv class.sv \
        localparam.sv task.sv function.sv
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

test_duplicate_module() {
    runAndCapture module_dupe_1.sv module_dupe_2.sv
    assertTrue "conversion should succeed" $result
    assertNotNull "stdout should not be empty" "$stdout"
    assertEquals "stderr should should have warning" "$DUPLICATE_MODULE_WARNING" "$stderr"
}

no_modules_test() {
    file=$1
    warning="$2"

    runAndCapture $file
    assertTrue "conversion should succeed" $result
    assertNull "stdout should be empty" "$stdout"
    assertEquals "stderr should have warning" "$warning" "$stderr"

    runAndCapture -v $file
    assertTrue "conversion should succeed" $result
    assertNotNull "stdout should not be empty" "$stdout"
    assertEquals "stderr should have warning" "$warning" "$stderr"
}

test_only_interface() {
    no_modules_test interface.sv "$INTERFACE_WARNING"
}

basic_no_modules_test() {
    kind=$1
    warning="Warning: Source includes a $kind but no modules. Such elements are elaborated into the modules that use them. Please convert all sources in one invocation."
    no_modules_test $kind.sv "$warning"
}

test_only_package() {
    basic_no_modules_test package
}

test_only_class() {
    basic_no_modules_test class
}

test_only_function() {
    basic_no_modules_test function
}

test_only_task() {
    basic_no_modules_test task
}

test_only_localparam() {
    basic_no_modules_test localparam
}

source ../lib/functions.sh

. shunit2
