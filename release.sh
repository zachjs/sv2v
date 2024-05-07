#!/bin/bash

set -e
set -x

version=$1

# ensure there are no uncommitted changes
[ "" == "$(git status --porcelain)" ]

# update the version in sv2v.cabal
sed -i.bak -e "s/^version.*/version: $version/" sv2v.cabal
diff sv2v.cabal{,.bak} && echo not changed && exit 1 || true
rm sv2v.cabal.bak

# update the version in CHANGELOG.md
sed -i.bak -e "s/^## Unreleased$/## v$version/" CHANGELOG.md
diff CHANGELOG.md{,.bak} && echo not changed && exit 1 || true
rm CHANGELOG.md.bak

# create the release commit and tag
git commit -a -m "release v$version"
git tag -a v$version HEAD -m "Release v$version"

# build and test
make
make test
[ $version == `bin/sv2v --numeric-version` ]

# push the release commit and tag
git push
git push origin v$version

# create the GitHub release
notes=`pandoc --from markdown --to markdown --wrap none CHANGELOG.md | \
    sed '3,/^## /!d' | \
    tac | tail -n +3 | tac`
gh release create v$version --title v$version --notes "$notes"

# create the Hackage release candidate
stack upload --test-tarball --candidate --pvp-bounds upper .
