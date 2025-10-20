#!/usr/bin/env bash

set -euo pipefail

cabal repl Color --build-depends=QuickCheck --build-depends=JuicyPixels --with-compiler=doctest --repl-options='-w -Wdefault'
