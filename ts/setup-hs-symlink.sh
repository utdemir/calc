#!/usr/bin/env sh

set -o errexit
set -o pipefail

cd "$(dirname "${BASH_SOURCE[0]}")"

nix-build ../hs -A calc-js -o public/calc-js --builders ""
