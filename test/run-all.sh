#!/bin/bash

cd `dirname "${BASH_SOURCE[0]}"`

for script in `ls */run.sh`; do
    suite=`dirname $script`
    echo "--------------------"
    echo "SUITE: $suite"
    echo "--------------------"
    (cd $suite; ./run.sh)
done
