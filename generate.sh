#!/usr/bin/env bash
set -euo pipefail

SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPTDIR"

set +e
OUTPUT=$(nix-build -A stack-nix.passthru.calculateMaterializedSha --no-out-link 2>&1)
EXIT_CODE=$?
if [[ "$EXIT_CODE" -ne 0 ]]; then
    STACK_SHA256=$(echo "$OUTPUT" | grep "stack-sha256 = \".*\";" | sed -E 's/\s*stack-sha256 = "(.*)";\s*/\1/g')
    SCRIPT=$(echo "$OUTPUT" | grep "To fix run: /nix/.*" | sed -E 's|.*To fix run: (/nix/.*)|\1|g')
else
    STACK_SHA256=$(bash "$OUTPUT")
    SCRIPT=$(nix-build -A stack-nix.passthru.updateMaterialized --no-out-link)
fi

# Update stack-sha256
if [[ -n "$STACK_SHA256" ]]; then
    echo "Got stack-sha256: $STACK_SHA256"
    sed -i "s/stack-sha256 = .*;/stack-sha256 = \"$STACK_SHA256\";/g" default.nix
else
    echo "Error, didn't get stack-sha256!"
fi

# Update materialization
if [[ -n "$SCRIPT" ]]; then
    echo "Got script: $SCRIPT"
    bash "$SCRIPT"
else
    echo "Error, didn't get script!"
fi
