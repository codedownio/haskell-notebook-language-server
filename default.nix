{
  haskellNix ? import (builtins.fetchTarball "https://github.com/codedownio/haskell.nix/archive/d9f17eca28b3cb336aa33080773f8196f01f458a.tar.gz") {}

, nixpkgsArgs ? haskellNix.nixpkgsArgs

, nixpkgs ? import haskellNix.sources.nixpkgs-unstable nixpkgsArgs

, pkgs ? if static then nixpkgs.pkgsCross.musl64 else nixpkgs

, static ? false

, symbols ? false

, checkMaterialization ? false

, profile ? false
}:

with pkgs;

let
  gitignoreSource = callPackage ./nix/hercules-gitignore.nix {};

  src = gitignoreSource ./.;

  filterSubdir = subDir: pkgs.haskell-nix.haskellLib.cleanSourceWith { inherit src subDir; };

  staticOptions = import ./static.nix { inherit pkgs symbols environment; };

in

haskell-nix.stackProject {
  inherit src;

  stack-sha256 = "sha256-lYQ4LIqzMerEFuryhBr20sysUF+5iWJqKN+zApXrny7=";
  # materialized = ./materialized;
  # inherit checkMaterialization;
}
