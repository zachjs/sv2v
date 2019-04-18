#!/bin/bash

cd `dirname "${BASH_SOURCE[0]}"`

failures=0
for script in `ls */run.sh`; do
    suite=`dirname $script`
    echo "--------------------"
    echo "SUITE: $suite"
    echo "--------------------"
    (cd $suite; ./run.sh)
    if [ $? -ne 0 ]; then
        failures=`expr $failures + 1`
    fi
done
if [ $failures -ne 0 ]; then
    echo "$failures test suite(s) failed"
    exit 1
fi
