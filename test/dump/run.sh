#!/bin/bash

PHASES=(
    parse
    initial
    main_1
    main_2
    final
)

test_dump_prefix() {
    runAndCapture --dump-prefix=dump_ example.sv
    assertTrue "default conversion should succeed" $result
    assertNotNull "stdout should not be empty" "$stdout"
    assertNull "stderr should be empty" "$stderr"

    assertTrue "main_3 dump should not exist" "[ ! -f dump_main_3.sv ]"
    for phase in "${PHASES[@]}"; do
        path=dump_$phase.sv
        assertTrue "$phase dump should be non-empty" "[ -s $path ]"
        rm $path
    done
}

source ../lib/functions.sh

. shunit2
