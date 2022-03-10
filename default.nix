{
  haskellNix ? import (builtins.fetchTarball "https://github.com/codedownio/haskell.nix/archive/5867bbb4aba60d7bcc1d4291e23325a4c38efdb5.tar.gz") {}

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
