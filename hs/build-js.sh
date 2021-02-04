#!/usr/bin/env bash

set -o errexit
set -o pipefail

cd "$(dirname "${BASH_SOURCE[0]}")"

read -r -d '' scr << 'EOF' || true
set -o errexit
set -o xtrace
set -o nounset

tmp=$(mktemp -d)

rm -rf out || true
mkdir out

ahc-cabal install calc-js \
  --project-file cabal.project.js \
  --installdir $tmp/installdir/ \
  --builddir $tmp/builddir

ahc-dist \
  --no-main \
  --export-function runCalc \
  --input-exe $tmp/installdir/calc-js \
  --output-directory out/
EOF

docker run \
  -it --rm \
  -v "$(pwd):/workspace" -w /workspace \
  terrorjack/asterius:210111 \
  bash -c "$scr"
