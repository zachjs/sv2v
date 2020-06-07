#!/bin/bash

tests=(`basename -s .sv *.sv`)

if [ $1 ]; then
    tests=(`basename -s .sv "$@"`)
    shift ${#tests[@]}
fi

suite() {
    for test in "${tests[@]}"; do
        if [ ! -f $test.sv ]; then
            echo "Could not find $test.sv"
            exit 1
        fi
        addTest $test
    done
}
