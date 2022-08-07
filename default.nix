{
  haskellNix ? import (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/9709b2d05acb8b2d1451e5d7593756ca3a1be7d7.tar.gz") {}

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

  stack-sha256 = "01mxvgqrkqbllay4dmpa86f7dkakx0l29kkr82apbypxirf08fav";
  materialized = ./materialized;
  inherit checkMaterialization;

  modules = [{
    # https://github.com/input-output-hk/haskell.nix/issues/1177#issuecomment-891568396
    nonReinstallablePkgs = [
      "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
      "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
      # ghcjs custom packages
      "ghcjs-prim" "ghcjs-th"
      "ghc-bignum" "exceptions" "stm"
      "ghc-boot"
      "ghc" "Cabal" "Win32" "array" "binary" "bytestring" "containers"
      "directory" "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
      # "ghci" "haskeline"
      "hpc"
      "mtl" "parsec" "process" "text" "time" "transformers"
      "unix" "xhtml" "terminfo"
    ];

  }];
}
