#!/usr/bin/env bash

SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $SCRIPTDIR

nix run .#cabal2nix -- . --no-check > default.nix
