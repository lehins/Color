#!/usr/bin/env bash

set -euo pipefail

cabal repl Color --build-depends=QuickCheck --build-depends=JuicyPixels --build-depends=QuickCheck --with-compiler=doctest --repl-options='-w -Wdefault'
