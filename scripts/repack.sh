#!/bin/bash
set -euo pipefail
trap 's=$?; echo ": Error on line "$LINENO": $BASH_COMMAND"; exit $s' ERR
IFS=$'\n\t'

unset WORKDIR
workdir() {
    if [ -z "${WORKDIR:+x}" ]; then
        # if a temporary working directory hasn't been established, create one
        TMPDIR="${TMPDIR:-$(dirname $(mktemp -u))}"
        WORKDIR=$(mktemp -d "$TMPDIR/.$(basename "$0").XXXXXXXXXX")
        # it's polite to clean up after ourselves
        trap "rm -rf \"$WORKDIR\"" INT EXIT
    fi
}

workdir

FILENAME=".nearest.zst"
zstdcat "$FILENAME" | \
zstd -v -T0 --long --ultra -22 -o "$WORKDIR/$FILENAME" &&
mv "$WORKDIR/$FILENAME" "$FILENAME"
