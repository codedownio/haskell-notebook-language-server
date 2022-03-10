{
  haskellNix ? import (builtins.fetchTarball "https://github.com/codedownio/haskell.nix/archive/11321af2b62e67a11ddfc8250617ed32e92aa631.tar.gz") {}

, nixpkgsArgs ? haskellNix.nixpkgsArgs

, nixpkgs ? import haskellNix.sources.nixpkgs-unstable nixpkgsArgs

, pkgs ? if static then nixpkgs.pkgsCross.musl64 else nixpkgs

, static ? false

, symbols ? false

, checkMaterialization ? true

, profile ? false
}:

with pkgs;

let
  gitignoreSource = callPackage ./nix/hercules-gitignore.nix {};

  src = gitignoreSource ./.;

  filterSubdir = subDir: pkgs.haskell-nix.haskellLib.cleanSourceWith { inherit src subDir; };

  staticOptions = import ./static.nix { inherit pkgs symbols environment; };

in

haskell-nix.project {
  inherit src;

  stack-sha256 = "149p2nlb7di9zg27z8gv0k4kdjxbxkf7p5d8grijk0j1d9fln0zr";
  materialized = ./materialized;
  inherit checkMaterialization;
}
