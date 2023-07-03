#!/usr/bin/env bash

SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $SCRIPTDIR

nix run .#cabal2nix -- . --no-check --compiler ghc-8.10 > default.nix

# nix run .#cabal2nix -- . --no-check --compiler ghc-8.10 > default-810.nix
# nix run .#cabal2nix -- . --no-check --compiler ghc-9.0 > default-90.nix
# nix run .#cabal2nix -- . --no-check --compiler ghc-9.2 > default-92.nix
# nix run .#cabal2nix -- . --no-check --compiler ghc-9.4 > default-94.nix
# nix run .#cabal2nix -- . --no-check --compiler ghc-9.6 > default-96.nix
