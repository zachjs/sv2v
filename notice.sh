#!/bin/bash

dependencies=`stack ls dependencies \
    | sed -e 's/ /-/' \
    | grep -v "^sv2v-[0-9\.]\+\$" \
    | grep -v "^rts-1\.0\$" \
    | grep -v "^ghc-boot-th" \
    `

for dependency in `echo "$dependencies"`; do
    license_url="https://hackage.haskell.org/package/$dependency/src/LICENSE"

    echo "================================================================================"
    echo "Dependency: $dependency"
    echo "================================================================================"
    echo ""
    curl "$license_url" 2> /dev/null | sed -e "s/^/  /"
    echo ""
done
