#! /usr/bin/env nix-shell
#! nix-shell -i bash -p jq github-cli

set -e

BUILT=$(nix build .#githubArtifacts --no-link --json | jq -r '.[0].outputs.out')
echo "Built: $BUILT"
ls "$BUILT"

VERSION="$(nix eval .#ghc928-static.version --raw)"

IFS=$'\n'
ARTIFACTS=$(find "$BUILT" -name "*.tar.gz")
unset IFS
echo "Saw artifacts: $ARTIFACTS"

# Smoke check
# FIRST_EXECUTABLE=$(find "$BUILT" -type f -executable | head -n 1)
# echo "FIRST_EXECUTABLE: $FIRST_EXECUTABLE"
# $FIRST_EXECUTABLE --help > /dev/null 2>&1

TAG=v"$VERSION"
echo "Tagging at $TAG"
git tag "$TAG" -f
echo ""

echo "Pushing tags"
git push --tags
echo ""

echo "Creating release $TAG"
gh release create "$TAG" \
  ${ARTIFACTS[@]} \
  --title "$TAG" \
  --notes ""
