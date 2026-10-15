#!/bin/sh
# Test a built cabal2json without a build tree: run it on every .cabal file
# in tests/golden and compare the output with the committed .json file.
# The full suite (flags, --os, unit tests) is `cabal run -- golden-tests`.
#
# Usage: tests/run.sh BINARY
set -eu

if [ $# -ne 1 ]; then
    echo "usage: $0 BINARY" >&2
    exit 2
fi
bin=$1
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT

"$bin" --help > /dev/null

failed=0
count=0
for cabalfile in tests/golden/*.cabal; do
    name=$(basename "$cabalfile" .cabal)
    golden=tests/golden/$name.json
    count=$((count + 1))
    if ! "$bin" --out "$out/$name.json" "$cabalfile"; then
        echo "FAIL $cabalfile: cabal2json failed"
        failed=$((failed + 1))
    elif ! diff --strip-trailing-cr -u "$golden" "$out/$name.json"; then
        echo "FAIL $cabalfile: output differs from $golden"
        failed=$((failed + 1))
    fi
done

if [ "$failed" -ne 0 ]; then
    echo "$failed of $count golden files failed"
    exit 1
fi
echo "All $count golden files passed"
